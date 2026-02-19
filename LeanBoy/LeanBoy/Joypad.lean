import LeanBoy.Utils.Bits
import LeanBoy.InterruptController
/-!
# Joypad

Handles the P1 register (0xFF00) and the eight Game Boy buttons.

The P1 register selects which button group to expose:
  Bit 5 = 0 → expose action buttons  (A, B, Select, Start)
  Bit 4 = 0 → expose direction buttons (Right, Left, Up, Down)

For each selected group, bits 3–0 read the button state (0 = pressed).
A keypress requests a Joypad interrupt.
-/

namespace LeanBoy

open Utils.Bits

inductive Key
  | Up | Down | Left | Right
  | A | B | Start | Select
  deriving Repr, BEq

structure Joypad where
  -- P1 register (bits 5–4 are the select lines written by software)
  p1        : UInt8 := 0xFF
  -- Low-nibble state for each group (0 = pressed, 1 = released)
  direction : UInt8 := 0x0F   -- Right, Left, Up, Down → bits 0–3
  action    : UInt8 := 0x0F   -- A, B, Select, Start   → bits 0–3
  deriving Repr

namespace Joypad

-- Map a key to its group and bit position.
private def keyInfo (k : Key) : Bool × Nat :=
  match k with
  | .Right  => (true,  0)
  | .Left   => (true,  1)
  | .Up     => (true,  2)
  | .Down   => (true,  3)
  | .A      => (false, 0)
  | .B      => (false, 1)
  | .Select => (false, 2)
  | .Start  => (false, 3)

-- Read the P1 register, combining the select bits with the button state.
def readP1 (jp : Joypad) : UInt8 :=
  let selectDirection := !(testBit jp.p1 4)
  let selectAction    := !(testBit jp.p1 5)
  let lo :=
    match selectDirection, selectAction with
    | true,  false => jp.direction
    | false, true  => jp.action
    | true,  true  => jp.direction &&& jp.action
    | false, false => 0x0F
  (jp.p1 &&& 0x30) ||| lo ||| 0xC0   -- bits 7–6 always 1, bits 5–4 are select lines

-- Write to P1 (only bits 5–4 are writable).
def writeP1 (jp : Joypad) (v : UInt8) : Joypad :=
  { jp with p1 := (v &&& 0x30) ||| (jp.p1 &&& 0xCF) }

-- Press a key (clear the corresponding bit).
def press (jp : Joypad) (ic : InterruptController) (k : Key) :
    Joypad × InterruptController :=
  let (isDirection, bit) := keyInfo k
  let jp' :=
    if isDirection then { jp with direction := clearBit jp.direction bit }
    else                { jp with action    := clearBit jp.action    bit }
  (jp', ic.request .Joypad)

-- Release a key (set the corresponding bit).
def release (jp : Joypad) (k : Key) : Joypad :=
  let (isDirection, bit) := keyInfo k
  if isDirection then { jp with direction := setBit jp.direction bit }
  else                { jp with action    := setBit jp.action    bit }

end Joypad

end LeanBoy
