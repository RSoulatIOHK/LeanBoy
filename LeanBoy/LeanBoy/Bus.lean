import LeanBoy.Apu
import LeanBoy.Cartridge.Cartridge
import LeanBoy.Gpu.Gpu
import LeanBoy.Ram
import LeanBoy.Timer
import LeanBoy.Joypad
import LeanBoy.SerialPort
import LeanBoy.InterruptController
/-!
# Memory Bus (MMU)

Routes 16-bit addresses to the correct hardware component.

Address map:
  0x0000–0x7FFF  Cartridge ROM
  0x8000–0x9FFF  VRAM (GPU tile data + tile maps; CGB: banked via VBK 0xFF4F)
  0xA000–0xBFFF  Cartridge RAM (external)
  0xC000–0xDFFF  WRAM (CGB: 32 KB banked via SVBK 0xFF70)
  0xE000–0xFDFF  Echo RAM → mirrors WRAM
  0xFE00–0xFE9F  OAM (sprite table)
  0xFEA0–0xFEFF  Unused (returns 0xFF)
  0xFF00         Joypad
  0xFF01–0xFF02  Serial port
  0xFF04–0xFF07  Timer
  0xFF0F         Interrupt flags (IF)
  0xFF40–0xFF4B  GPU registers
  0xFF46         OAM DMA transfer trigger
  0xFF4D         KEY1: CPU speed switch (CGB)
  0xFF51–0xFF55  HDMA/GDMA registers (CGB)
  0xFF70         SVBK: WRAM bank select (CGB)
  0xFF80–0xFFFE  HRAM / ZRAM
  0xFFFF         Interrupt enable (IE)

Implementation note: the Bus holds IO.Refs to each component so that
the CPU, GPU, Timer etc. can be mutated in place during emulation.
-/

namespace LeanBoy

open Cartridge Gpu

-- CGB HDMA/GDMA state (registers 0xFF51–0xFF55).
structure HdmaState where
  reg1   : UInt8  := 0xFF   -- 0xFF51: source high byte
  reg2   : UInt8  := 0xFF   -- 0xFF52: source low byte (low nibble ignored = 16-byte aligned)
  reg3   : UInt8  := 0xFF   -- 0xFF53: dest high byte (upper 3 bits ignored)
  reg4   : UInt8  := 0xFF   -- 0xFF54: dest low byte (low nibble ignored)
  active : Bool   := false  -- true = HDMA transfer in progress
  blocks : UInt8  := 0      -- remaining 16-byte blocks to copy
  src    : UInt16 := 0      -- current source address (live during HDMA)
  dst    : UInt16 := 0x8000 -- current VRAM destination (live during HDMA)

structure Bus where
  cartridge   : IO.Ref Cartridge
  gpu         : IO.Ref Gpu
  apu         : IO.Ref Apu
  wram        : IO.Ref ByteArray   -- 32 KB flat (8 banks × 4 KB); bank N at offset N*0x1000
  wramBank    : IO.Ref UInt8       -- current bank for 0xD000–0xDFFF (1–7; 0 treated as 1)
  hram        : IO.Ref Ram         -- 0xFF80–0xFFFE
  timer       : IO.Ref Timer
  joypad      : IO.Ref Joypad
  serialPort  : IO.Ref SerialPort
  interrupt   : IO.Ref InterruptController
  hdma        : IO.Ref HdmaState   -- HDMA/GDMA state (CGB)
  doubleSpeed : IO.Ref Bool        -- true = CPU running at 2× speed (CGB)
  speedArmed  : IO.Ref Bool        -- true = speed switch armed via KEY1 bit 0
  isCgb       : Bool
  printSerial : Bool

namespace Bus

def create (cart : Cartridge) (isCgb : Bool := false) (printSerial : Bool := false) : IO Bus := do
  return {
    cartridge   := ← IO.mkRef cart
    gpu         := ← IO.mkRef { cgbMode := isCgb }
    apu         := ← IO.mkRef {}
    wram        := ← IO.mkRef (ByteArray.mk (Array.replicate 0x8000 0))  -- 32 KB, 8 banks × 4 KB
    wramBank    := ← IO.mkRef 1  -- bank 1 default (SVBK=0 → bank 1 per spec)
    hram        := ← IO.mkRef (Ram.create 0xFF80 0xFFFE)
    timer       := ← IO.mkRef {}
    joypad      := ← IO.mkRef {}
    serialPort  := ← IO.mkRef {}
    interrupt   := ← IO.mkRef {}
    hdma        := ← IO.mkRef {}
    doubleSpeed := ← IO.mkRef false
    speedArmed  := ← IO.mkRef false
    isCgb
    printSerial
  }

def readByte (bus : Bus) (addr : UInt16) : IO UInt8 := do
  if addr < 0x8000 then
    let cart ← bus.cartridge.get
    return cart.readByte addr
  else if addr < 0xA000 then
    let gpu ← bus.gpu.get
    return gpu.readByte addr
  else if addr < 0xC000 then
    let cart ← bus.cartridge.get
    return cart.readByte addr
  else if addr < 0xE000 then
    let wram ← bus.wram.get
    let wb   ← bus.wramBank.get
    let bank  := if addr < 0xD000 then 0 else wb.toNat
    let off   := if addr < 0xD000 then addr.toNat - 0xC000 else addr.toNat - 0xD000
    return wram.get! (bank * 0x1000 + off)
  else if addr < 0xFE00 then
    -- Echo RAM: mirrors WRAM (0xE000-0xEFFF → bank 0; 0xF000-0xFDFF → switched bank)
    let wram ← bus.wram.get
    let wb   ← bus.wramBank.get
    let bank  := if addr < 0xF000 then 0 else wb.toNat
    let off   := if addr < 0xF000 then addr.toNat - 0xE000 else addr.toNat - 0xF000
    return wram.get! (bank * 0x1000 + off)
  else if addr < 0xFEA0 then
    let gpu ← bus.gpu.get
    return gpu.readByte addr   -- OAM
  else if addr < 0xFF00 then
    return 0xFF   -- unusable region
  else if addr == 0xFF00 then
    let jp ← bus.joypad.get
    return jp.readP1
  else if addr < 0xFF03 then
    let sp ← bus.serialPort.get
    return sp.readByte addr
  else if addr < 0xFF08 then
    let t ← bus.timer.get
    return t.readByte addr
  else if addr == 0xFF0F then
    let ic ← bus.interrupt.get
    return ic.readByte addr
  else if addr >= 0xFF10 && addr < 0xFF40 then
    let apu ← bus.apu.get
    return apu.readByte addr   -- APU registers + Wave RAM
  else if addr == 0xFF4D then
    -- KEY1: CPU speed switch register (CGB only)
    if bus.isCgb then
      let ds    ← bus.doubleSpeed.get
      let armed ← bus.speedArmed.get
      return ((if ds then 0x80 else 0x00) ||| (if armed then 0x01 else 0x00))
    else return 0xFF
  else if addr >= 0xFF51 && addr <= 0xFF54 then
    return 0xFF   -- HDMA1-4 are write-only
  else if addr == 0xFF55 then
    -- HDMA5: bit7=0 when active, bits6-0 = remaining blocks - 1; 0xFF when idle
    if bus.isCgb then
      let h ← bus.hdma.get
      if h.active then return (h.blocks - 1) &&& 0x7F
      else return 0xFF
    else return 0xFF
  else if addr == 0xFF70 then
    -- SVBK: WRAM bank register (CGB only)
    if bus.isCgb then
      let wb ← bus.wramBank.get
      return wb
    else return 0xFF
  else if addr < 0xFF80 then
    let gpu ← bus.gpu.get
    return gpu.readByte addr   -- GPU registers
  else if addr < 0xFFFF then
    let hram ← bus.hram.get
    return hram.readByte addr
  else
    let ic ← bus.interrupt.get
    return ic.readByte addr   -- 0xFFFF = IE

-- OAM DMA: copy 160 bytes from source to OAM.
private def dmaTransfer (bus : Bus) (v : UInt8) : IO Unit := do
  let srcBase := v.toNat * 0x100
  let srcBytes ← (List.range 160).mapM (fun i => bus.readByte (srcBase + i).toUInt16)
  let srcArray := ByteArray.mk srcBytes.toArray
  bus.gpu.modify (fun gpu => { gpu with oam := gpu.oam.dmaLoad srcArray 0 })

-- GDMA: copy `blocks` × 16 bytes from `src` into VRAM immediately.
private def hdmaTransfer (bus : Bus) (src dst : UInt16) (blocks : Nat) : IO Unit := do
  for i in List.range (blocks * 16) do
    let b ← bus.readByte (src + i.toUInt16)
    bus.gpu.modify (fun g => g.writeByte (dst + i.toUInt16) b)

def writeByte (bus : Bus) (addr : UInt16) (v : UInt8) : IO Unit := do
  if addr < 0x8000 then
    bus.cartridge.modify (fun c => c.writeByte addr v)
  else if addr < 0xA000 then
    bus.gpu.modify (fun g => g.writeByte addr v)
  else if addr < 0xC000 then
    bus.cartridge.modify (fun c => c.writeByte addr v)
  else if addr < 0xE000 then
    let wb   ← bus.wramBank.get
    let bank  := if addr < 0xD000 then 0 else wb.toNat
    let off   := if addr < 0xD000 then addr.toNat - 0xC000 else addr.toNat - 0xD000
    bus.wram.modify (fun w => w.set! (bank * 0x1000 + off) v)
  else if addr < 0xFE00 then
    -- Echo RAM write mirrors WRAM
    let wb   ← bus.wramBank.get
    let bank  := if addr < 0xF000 then 0 else wb.toNat
    let off   := if addr < 0xF000 then addr.toNat - 0xE000 else addr.toNat - 0xF000
    bus.wram.modify (fun w => w.set! (bank * 0x1000 + off) v)
  else if addr < 0xFEA0 then
    bus.gpu.modify (fun g => g.writeByte addr v)
  else if addr < 0xFF00 then
    return ()   -- unusable
  else if addr == 0xFF00 then
    bus.joypad.modify (fun jp => jp.writeP1 v)
  else if addr < 0xFF03 then
    let sp ← bus.serialPort.get
    let sp' ← sp.writeByte addr v bus.printSerial
    bus.serialPort.set sp'
  else if addr < 0xFF08 then
    bus.timer.modify (fun t => t.writeByte addr v)
  else if addr == 0xFF0F then
    bus.interrupt.modify (fun ic => ic.writeByte addr v)
  else if addr >= 0xFF10 && addr < 0xFF40 then
    bus.apu.modify (fun a => a.writeByte addr v)  -- APU registers + Wave RAM
  else if addr == 0xFF4D then
    -- KEY1: arm CPU speed switch (CGB only; bit 0 = prepare switch)
    if bus.isCgb then bus.speedArmed.set ((v &&& 0x01) != 0)
  else if addr == 0xFF51 then bus.hdma.modify (fun h => { h with reg1 := v })
  else if addr == 0xFF52 then bus.hdma.modify (fun h => { h with reg2 := v })
  else if addr == 0xFF53 then bus.hdma.modify (fun h => { h with reg3 := v })
  else if addr == 0xFF54 then bus.hdma.modify (fun h => { h with reg4 := v })
  else if addr == 0xFF55 then
    if bus.isCgb then
      let h   ← bus.hdma.get
      let src  := (h.reg1.toUInt16 <<< 8) ||| (h.reg2 &&& 0xF0).toUInt16
      let dst  := (0x8000 : UInt16) ||| ((h.reg3 &&& 0x1F).toUInt16 <<< 8) ||| (h.reg4 &&& 0xF0).toUInt16
      let blk  := (v &&& 0x7F).toNat + 1   -- number of 16-byte blocks
      if v &&& 0x80 == 0 then
        -- GDMA: copy all blocks immediately
        bus.hdma.set { h with active := false, blocks := 0 }
        hdmaTransfer bus src dst blk
      else if h.active then
        -- Writing bit7=1 while HDMA active = terminate
        bus.hdma.set { h with active := false }
      else
        -- Start HDMA: copy 16 bytes per HBlank
        bus.hdma.set { h with active := true, blocks := (v &&& 0x7F) + 1, src := src, dst := dst }
  else if addr == 0xFF70 then
    -- SVBK: WRAM bank select (CGB only; 0 → bank 1 per spec)
    if bus.isCgb then
      let bank := let b := v &&& 0x07; if b == 0 then 1 else b
      bus.wramBank.set bank
  else if addr == 0xFF46 then
    dmaTransfer bus v
  else if addr < 0xFF80 then
    bus.gpu.modify (fun g => g.writeByte addr v)
  else if addr < 0xFFFF then
    bus.hram.modify (fun r => r.writeByte addr v)
  else
    bus.interrupt.modify (fun ic => ic.writeByte addr v)

-- HDMA step: copy one 16-byte chunk into VRAM (called once per HBlank when active).
def hdmaStep (bus : Bus) : IO Unit := do
  let h ← bus.hdma.get
  if !h.active || h.blocks == 0 then return
  for i in List.range 16 do
    let b ← bus.readByte (h.src + i.toUInt16)
    bus.gpu.modify (fun g => g.writeByte (h.dst + i.toUInt16) b)
  let blocks' := h.blocks - 1
  bus.hdma.set { h with
    src    := h.src + 16
    dst    := h.dst + 16
    blocks := blocks'
    active := blocks' > 0 }

-- 16-bit reads/writes (little-endian).
def readWord (bus : Bus) (addr : UInt16) : IO UInt16 := do
  let lo ← bus.readByte addr
  let hi ← bus.readByte (addr + 1)
  return (hi.toUInt16 <<< 8) ||| lo.toUInt16

def writeWord (bus : Bus) (addr : UInt16) (v : UInt16) : IO Unit := do
  bus.writeByte addr v.toUInt8
  bus.writeByte (addr + 1) (v >>> 8).toUInt8

end Bus

end LeanBoy
