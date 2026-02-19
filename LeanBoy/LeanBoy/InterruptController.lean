import LeanBoy.Utils.Bits
/-!
# Interrupt Controller

Manages the two interrupt registers:
  IE  (0xFFFF) – Interrupt Enable
  IF  (0xFF0F) – Interrupt Flags

Priority order (lowest address = highest priority):
  Bit 0 – VBlank
  Bit 1 – LCD STAT
  Bit 2 – Timer
  Bit 3 – Serial port
  Bit 4 – Joypad
-/

namespace LeanBoy

open Utils.Bits

inductive InterruptType
  | VBlank
  | LcdStat
  | Timer
  | SerialPort
  | Joypad
  deriving Repr, BEq

namespace InterruptType

def bit : InterruptType → Nat
  | VBlank     => 0
  | LcdStat    => 1
  | Timer      => 2
  | SerialPort => 3
  | Joypad     => 4

end InterruptType

structure InterruptController where
  ie  : UInt8 := 0x00   -- 0xFFFF
  ifl : UInt8 := 0x00   -- 0xFF0F  (named ifl to avoid shadowing `if`)
  deriving Repr

namespace InterruptController

-- Request an interrupt (set bit in IF).
def request (ic : InterruptController) (t : InterruptType) : InterruptController :=
  { ic with ifl := setBit ic.ifl t.bit }

-- Clear an interrupt (clear bit in IF).
def clear (ic : InterruptController) (t : InterruptType) : InterruptController :=
  { ic with ifl := clearBit ic.ifl t.bit }

-- Return the highest-priority pending interrupt that is both requested (IF) and enabled (IE).
def nextPending (ic : InterruptController) : Option InterruptType := do
  let pending := ic.ifl &&& ic.ie &&& 0x1F
  if pending == 0 then none
  else
    -- Check each bit in priority order
    let candidates := [.VBlank, .LcdStat, .Timer, .SerialPort, .Joypad]
    candidates.find? (fun t => testBit pending t.bit)

-- The vector address for a given interrupt type.
def vector (t : InterruptType) : UInt16 :=
  match t with
  | .VBlank     => 0x0040
  | .LcdStat    => 0x0048
  | .Timer      => 0x0050
  | .SerialPort => 0x0058
  | .Joypad     => 0x0060

-- Read a byte from the interrupt controller's address space.
def readByte (ic : InterruptController) (addr : UInt16) : UInt8 :=
  match addr with
  | 0xFF0F => ic.ifl ||| 0xE0   -- upper 3 bits always read as 1
  | 0xFFFF => ic.ie
  | _      => 0xFF

-- Write a byte to the interrupt controller's address space.
def writeByte (ic : InterruptController) (addr : UInt16) (v : UInt8) : InterruptController :=
  match addr with
  | 0xFF0F => { ic with ifl := v &&& 0x1F }
  | 0xFFFF => { ic with ie  := v &&& 0x1F }
  | _      => ic

end InterruptController

end LeanBoy
