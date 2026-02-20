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

-- Advance the timer by `cycles` T-cycles; may request a Timer interrupt.
-- Closed-form: counts falling edges of the clock bit without a per-cycle loop.
-- Falling edges of bit N in [old, old+cycles) = (new >> (N+1)) - (old >> (N+1)),
-- where arithmetic is over Nat so UInt16 wraparound is handled correctly.
def tick (t : Timer) (ic : InterruptController) (cycles : Nat) :
    Timer × InterruptController :=
  let oldNat      := t.internal.toNat
  let newNat      := oldNat + cycles
  let newInternal := newNat.toUInt16   -- wraps mod 65536 like hardware
  let bit         := timaClockBit t.tac
  let timaEdges   :=
    if enabled t.tac
    then (newNat >>> (bit + 1)) - (oldNat >>> (bit + 1))
    else 0
  if timaEdges == 0 then
    ({ t with internal := newInternal }, ic)
  else Id.run do
    -- Apply TIMA ticks; at most 1–2 iterations in practice.
    let mut tima := t.tima
    let mut ic   := ic
    let mut rem  := timaEdges
    while rem > 0 do
      if tima == 0xFF then
        tima := t.tma
        ic   := ic.request .Timer
      else
        tima := tima + 1
      rem := rem - 1
    return ({ t with internal := newInternal, tima }, ic)

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
