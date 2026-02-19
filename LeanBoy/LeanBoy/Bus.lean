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
  0x8000–0x9FFF  VRAM (GPU tile data + tile maps)
  0xA000–0xBFFF  Cartridge RAM (external)
  0xC000–0xDFFF  WRAM (8 KB)
  0xE000–0xFDFF  Echo RAM → mirrors WRAM
  0xFE00–0xFE9F  OAM (sprite table)
  0xFEA0–0xFEFF  Unused (returns 0xFF)
  0xFF00         Joypad
  0xFF01–0xFF02  Serial port
  0xFF04–0xFF07  Timer
  0xFF0F         Interrupt flags (IF)
  0xFF40–0xFF4B  GPU registers
  0xFF46         DMA transfer trigger
  0xFF80–0xFFFE  HRAM / ZRAM
  0xFFFF         Interrupt enable (IE)

Implementation note: the Bus holds IO.Refs to each component so that
the CPU, GPU, Timer etc. can be mutated in place during emulation.
-/

namespace LeanBoy

open Cartridge Gpu

structure Bus where
  cartridge   : IO.Ref Cartridge
  gpu         : IO.Ref Gpu
  apu         : IO.Ref Apu
  wram        : IO.Ref Ram         -- 0xC000–0xDFFF
  hram        : IO.Ref Ram         -- 0xFF80–0xFFFE
  timer       : IO.Ref Timer
  joypad      : IO.Ref Joypad
  serialPort  : IO.Ref SerialPort
  interrupt   : IO.Ref InterruptController
  printSerial : Bool

namespace Bus

def create (cart : Cartridge) (printSerial : Bool := false) : IO Bus := do
  return {
    cartridge  := ← IO.mkRef cart
    gpu        := ← IO.mkRef {}
    apu        := ← IO.mkRef {}
    wram       := ← IO.mkRef (Ram.create 0xC000 0xDFFF)
    hram       := ← IO.mkRef (Ram.create 0xFF80 0xFFFE)
    timer      := ← IO.mkRef {}
    joypad     := ← IO.mkRef {}
    serialPort := ← IO.mkRef {}
    interrupt  := ← IO.mkRef {}
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
    return wram.readByte addr
  else if addr < 0xFE00 then
    -- Echo RAM: mirror of WRAM
    let wram ← bus.wram.get
    return wram.readByte (addr - 0x2000)
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
  else if addr < 0xFF80 then
    let gpu ← bus.gpu.get
    return gpu.readByte addr   -- GPU registers
  else if addr < 0xFFFF then
    let hram ← bus.hram.get
    return hram.readByte addr
  else
    let ic ← bus.interrupt.get
    return ic.readByte addr   -- 0xFFFF = IE

-- DMA: copy 160 bytes from source to OAM.
private def dmaTransfer (bus : Bus) (v : UInt8) : IO Unit := do
  let srcBase := v.toNat * 0x100
  -- Read 160 bytes from the source region
  let srcBytes ← (List.range 160).mapM (fun i => bus.readByte (srcBase + i).toUInt16)
  let srcArray := ByteArray.mk srcBytes.toArray
  bus.gpu.modify (fun gpu => { gpu with oam := gpu.oam.dmaLoad srcArray 0 })

def writeByte (bus : Bus) (addr : UInt16) (v : UInt8) : IO Unit := do
  if addr < 0x8000 then
    bus.cartridge.modify (fun c => c.writeByte addr v)
  else if addr < 0xA000 then
    bus.gpu.modify (fun g => g.writeByte addr v)
  else if addr < 0xC000 then
    bus.cartridge.modify (fun c => c.writeByte addr v)
  else if addr < 0xE000 then
    bus.wram.modify (fun r => r.writeByte addr v)
  else if addr < 0xFE00 then
    bus.wram.modify (fun r => r.writeByte (addr - 0x2000) v)
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
  else if addr == 0xFF46 then
    dmaTransfer bus v
  else if addr < 0xFF80 then
    bus.gpu.modify (fun g => g.writeByte addr v)
  else if addr < 0xFFFF then
    bus.hram.modify (fun r => r.writeByte addr v)
  else
    bus.interrupt.modify (fun ic => ic.writeByte addr v)

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
