import LeanBoy.Bus
import LeanBoy.Cpu.Cpu
import LeanBoy.Gpu.Gpu
import LeanBoy.Joypad
import LeanBoy.Cartridge.Cartridge
import LeanBoy.Debug
/-!
# Emulator

Top-level facade that wires the CPU, GPU, Timer, and Joypad together
and exposes the `runInstruction` step function used by the frontend.
-/

namespace LeanBoy

open Cartridge Gpu Cpu Debug

structure Emulator where
  cpuRef  : IO.Ref Cpu.Cpu
  bus     : Bus    -- shared reference — CPU and Emulator both hold it
  frameNo : IO.Ref Nat   -- frame counter for debug

namespace Emulator

-- Load a ROM from bytes and initialize all hardware to post-boot state.
def create (romBytes : ByteArray) (printSerial : Bool := false) : IO Emulator := do
  let cart  ← IO.ofExcept (Cartridge.detect romBytes)
  let isCgb := romBytes.size > 0x143 && (romBytes.get! 0x143 &&& 0x80) != 0
  let bus   ← Bus.create cart isCgb printSerial
  -- Post-boot ROM register state varies by hardware:
  --   DMG: A=0x01, F=0xB0, BC=0x0013, DE=0x00D8, HL=0x014D
  --   CGB: A=0x11, F=0x80, BC=0x0000, DE=0xFF56, HL=0x000D
  -- Games check A=0x11 to activate CGB code paths (palette setup, VRAM banking, etc.)
  let initRegs : Registers :=
    if isCgb then { a := 0x11, f := 0x80, b := 0x00, c := 0x00
                  , d := 0xFF, e := 0x56, h := 0x00, l := 0x0D }
    else          {}   -- DMG defaults from Registers struct
  let cpu     := { bus, registers := initRegs }
  let cpuRef  ← IO.mkRef cpu
  let frameNo ← IO.mkRef 0
  -- Dump initial LCD state so we have a baseline even before frame 1.
  let gpu ← bus.gpu.get
  log1 s!"[EMU] ROM loaded ({romBytes.size} bytes). \
LCDC=0x{hexByte gpu.lcdControl.value}  BGP=0x{hexByte gpu.palettes.bgp.value}"
  return { cpuRef, bus, frameNo }

-- Run one CPU instruction, tick the GPU, Timer, and APU accordingly.
-- Returns `some (frameBuffer, audioSamples)` when a full frame has been rendered.
-- audioSamples is an interleaved stereo S16LE ByteArray at 44100 Hz.
def step (emu : Emulator) : IO (Option (FrameBuffer × ByteArray)) := do
  let cycles ← Cpu.Cpu.runInstruction emu.cpuRef
  -- runInstruction returns M-cycles; GPU and Timer thresholds are in T-cycles.
  -- Normal: 1 M-cycle = 4 T-cycles. CGB double-speed: CPU runs 2×, GPU/Timer still at 1×.
  let ds      ← emu.bus.doubleSpeed.get
  let tCycles := if ds then cycles * 2 else cycles * 4
  -- Tick timer
  let ic ← emu.bus.interrupt.get
  let t  ← emu.bus.timer.get
  let (t', ic') := t.tick ic tCycles
  emu.bus.timer.set t'
  -- Tick GPU (pass ic' directly; write final IC once after both ticks)
  let gpu      ← emu.bus.gpu.get
  let prevMode := gpu.mode
  let fn       ← emu.frameNo.get
  let (gpu', ic'', result, dbgIO) := gpu.tick ic' tCycles fn
  emu.bus.gpu.set gpu'
  emu.bus.interrupt.set ic''
  dbgIO   -- execute any debug IO from the GPU tick
  -- HDMA: copy one 16-byte block whenever the GPU enters HBlank
  if prevMode != .HBlank && gpu'.mode == .HBlank then
    Bus.hdmaStep emu.bus
  -- Tick APU
  emu.bus.apu.modify (fun a => a.tick tCycles.toUInt32)
  match result with
  | .InFrame        => return none
  | .FrameEnded fb  =>
    let fn' ← emu.frameNo.get
    emu.frameNo.set (fn' + 1)
    -- Drain accumulated audio samples for this frame (only the filled portion)
    let apu ← emu.bus.apu.get
    let byteCount := apu.sampleCount.toNat * 4
    let audioSamples := apu.samples.extract 0 byteCount
    emu.bus.apu.set { apu with sampleCount := 0 }
    return some (fb, audioSamples)

-- Returns Some ram if the cartridge is battery-backed and has non-empty RAM
-- and was either written to (dirty) or had a save loaded. Returns None otherwise.
def getSaveRam (emu : Emulator) : IO (Option ByteArray) := do
  let cart ← emu.bus.cartridge.get
  if cart.hasBattery && cart.isRamDirty then
    return some (cart.getRam)
  else
    return none

-- Load save RAM into the cartridge (used to restore a .sav file on startup).
def loadSaveRam (emu : Emulator) (ram : ByteArray) : IO Unit := do
  emu.bus.cartridge.modify (fun c => c.withRam ram)

-- Press a key (generates a joypad interrupt).
def pressKey (emu : Emulator) (k : Key) : IO Unit := do
  let jp ← emu.bus.joypad.get
  let ic ← emu.bus.interrupt.get
  let (jp', ic') := jp.press ic k
  emu.bus.joypad.set jp'
  emu.bus.interrupt.set ic'

-- Release a key.
def releaseKey (emu : Emulator) (k : Key) : IO Unit := do
  emu.bus.joypad.modify (fun jp => jp.release k)


end Emulator

end LeanBoy
