import LeanBoy.Utils.Bits
import LeanBoy.InterruptController
/-!
# Timer

Implements the Game Boy timer hardware:
  DIV  (0xFF04) – Divider register (read-only; write resets internal counter)
  TIMA (0xFF05) – Timer counter
  TMA  (0xFF06) – Timer modulo (reload value on overflow)
  TAC  (0xFF07) – Timer control (enable + clock select)

The internal counter is 16-bit; DIV exposes only the high byte.
TIMA increments at a rate selected by TAC bits 0–1:
  00 → 1024 m-cycles   (4096 Hz)
  01 → 16 m-cycles     (262144 Hz)
  10 → 64 m-cycles     (65536 Hz)
  11 → 256 m-cycles    (16384 Hz)
-/

namespace LeanBoy

open Utils.Bits

structure Timer where
  -- Internal 16-bit counter. The high byte is exposed as DIV (0xFF04).
  internal : UInt16 := 0xABCC    -- post-boot ROM value
  tima     : UInt8  := 0x00
  tma      : UInt8  := 0x00
  tac      : UInt8  := 0xF8      -- upper bits always 1; bits 2-0 used
  deriving Repr

namespace Timer

-- The bit of `internal` that, when it falls from 1→0, ticks TIMA.
private def timaClockBit (tac : UInt8) : Nat :=
  match tac &&& 0x03 with
  | 0x00 => 9
  | 0x01 => 3
  | 0x02 => 5
  | _    => 7

-- Whether the timer is enabled (TAC bit 2).
@[inline] private def enabled (tac : UInt8) : Bool := testBit tac 2

-- Advance the timer by `cycles` m-cycles; may request a Timer interrupt.
def tick (t : Timer) (ic : InterruptController) (cycles : Nat) :
    Timer × InterruptController := Id.run do
  let mut timer := t
  let mut ic    := ic
  for _ in List.range cycles do
    let prevInternal := timer.internal
    timer := { timer with internal := timer.internal + 1 }
    -- TIMA increments when the selected bit of `internal` falls (AND with enable)
    let bit := timaClockBit timer.tac
    let wasSet := testBit16 prevInternal bit
    let nowSet := testBit16 timer.internal bit
    if enabled timer.tac && wasSet && !nowSet then
      if timer.tima == 0xFF then
        -- Overflow: reload from TMA and request interrupt
        timer := { timer with tima := timer.tma }
        ic    := ic.request .Timer
      else
        timer := { timer with tima := timer.tima + 1 }
  return (timer, ic)

-- Read a byte from the timer's address space.
def readByte (t : Timer) (addr : UInt16) : UInt8 :=
  match addr with
  | 0xFF04 => highByte t.internal
  | 0xFF05 => t.tima
  | 0xFF06 => t.tma
  | 0xFF07 => t.tac ||| 0xF8
  | _      => 0xFF

-- Write a byte to the timer's address space.
def writeByte (t : Timer) (addr : UInt16) (v : UInt8) : Timer :=
  match addr with
  | 0xFF04 => { t with internal := 0 }   -- any write resets DIV
  | 0xFF05 => { t with tima := v }
  | 0xFF06 => { t with tma  := v }
  | 0xFF07 => { t with tac  := v ||| 0xF8 }
  | _      => t

end Timer

end LeanBoy
