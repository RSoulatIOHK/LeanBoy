/-!
# Bit manipulation utilities

Low-level helpers for the 8- and 16-bit integer operations that hardware
emulation relies on throughout the codebase.
-/

namespace LeanBoy.Utils.Bits

-- Test whether bit `n` (0 = LSB) is set in a byte.
@[inline] def testBit (byte : UInt8) (n : Nat) : Bool :=
  (byte >>> n.toUInt8) &&& 1 == 1

-- Test whether bit `n` (0 = LSB) is set in a 16-bit word.
@[inline] def testBit16 (word : UInt16) (n : Nat) : Bool :=
  (word >>> n.toUInt16) &&& 1 == 1

-- Set bit `n` in a byte.
@[inline] def setBit (byte : UInt8) (n : Nat) : UInt8 :=
  byte ||| (1 <<< n.toUInt8)

-- Clear bit `n` in a byte.
@[inline] def clearBit (byte : UInt8) (n : Nat) : UInt8 :=
  byte &&& ~~~(1 <<< n.toUInt8)

-- Write `v` into bit `n` of a byte.
@[inline] def writeBit (byte : UInt8) (n : Nat) (v : Bool) : UInt8 :=
  if v then setBit byte n else clearBit byte n

-- Return the high byte of a 16-bit word.
@[inline] def highByte (w : UInt16) : UInt8 :=
  (w >>> 8).toUInt8

-- Return the low byte of a 16-bit word.
@[inline] def lowByte (w : UInt16) : UInt8 :=
  w.toUInt8

-- Combine two bytes into a 16-bit word (hi, lo).
@[inline] def makeWord (hi lo : UInt8) : UInt16 :=
  (hi.toUInt16 <<< 8) ||| lo.toUInt16

-- Interpret a UInt8 as a signed offset (-128..127) and return the Int.
@[inline] def toSignedOffset (b : UInt8) : Int :=
  if b < 0x80 then Int.ofNat b.toNat
  else Int.ofNat b.toNat - 256

end LeanBoy.Utils.Bits
