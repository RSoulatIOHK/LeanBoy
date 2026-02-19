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
  let cart ← IO.ofExcept (Cartridge.detect romBytes)
  let bus  ← Bus.create cart printSerial
  let cpu  := { bus }
  let cpuRef  ← IO.mkRef cpu
  let frameNo ← IO.mkRef 0
  -- Dump initial LCD state so we have a baseline even before frame 1.
  let gpu ← bus.gpu.get
  log1 s!"[EMU] ROM loaded ({romBytes.size} bytes). \
LCDC=0x{hexByte gpu.lcdControl.value}  BGP=0x{hexByte gpu.palettes.bgp.value}"
  return { cpuRef, bus, frameNo }

-- Run one CPU instruction, tick the GPU and Timer accordingly.
-- Returns `some frameBuffer` when a full frame has been rendered.
def step (emu : Emulator) : IO (Option FrameBuffer) := do
  let cycles ← Cpu.Cpu.runInstruction emu.cpuRef
  -- runInstruction returns M-cycles; GPU and Timer thresholds are in T-cycles.
  -- 1 M-cycle = 4 T-cycles on the Game Boy.
  let tCycles := cycles * 4
  -- Tick timer
  let ic ← emu.bus.interrupt.get
  let t  ← emu.bus.timer.get
  let (t', ic') := t.tick ic tCycles
  emu.bus.timer.set t'
  emu.bus.interrupt.set ic'
  -- Tick GPU
  let gpu ← emu.bus.gpu.get
  let fn  ← emu.frameNo.get
  let (gpu', ic'', result, dbgIO) := gpu.tick ic' tCycles fn
  emu.bus.gpu.set gpu'
  emu.bus.interrupt.set ic''
  dbgIO   -- execute any debug IO from the GPU tick
  match result with
  | .InFrame        => return none
  | .FrameEnded fb  =>
    let fn' ← emu.frameNo.get
    emu.frameNo.set (fn' + 1)
    return some fb

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
