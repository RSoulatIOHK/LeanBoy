import LeanBoy.Utils.Bits
/-!
# CPU Registers

The Sharp SM83 (Game Boy CPU) has eight 8-bit registers (A, B, C, D, E, F, H, L)
that can also be read as four 16-bit pairs (AF, BC, DE, HL), plus a 16-bit stack
pointer (SP) and program counter (PC) held in the CPU structure.

The F register encodes four flags in its upper nibble:
  Bit 7 – Zero (Z)
  Bit 6 – Subtract (N)
  Bit 5 – Half-carry (H)
  Bit 4 – Carry (C)
-/

namespace LeanBoy

open Utils.Bits

structure Registers where
  a : UInt8 := 0x01   -- Accumulator
  b : UInt8 := 0x00
  c : UInt8 := 0x13
  d : UInt8 := 0x00
  e : UInt8 := 0xD8
  f : UInt8 := 0xB0   -- Flags register (only upper nibble used)
  h : UInt8 := 0x01
  l : UInt8 := 0x4D
  deriving Repr

namespace Registers

-- 16-bit register pair accessors
@[inline] def af (r : Registers) : UInt16 := makeWord r.a r.f
@[inline] def bc (r : Registers) : UInt16 := makeWord r.b r.c
@[inline] def de (r : Registers) : UInt16 := makeWord r.d r.e
@[inline] def hl (r : Registers) : UInt16 := makeWord r.h r.l

-- 16-bit register pair setters
@[inline] def setAf (r : Registers) (v : UInt16) : Registers :=
  { r with a := highByte v, f := lowByte v &&& 0xF0 }   -- lower nibble of F always 0

@[inline] def setBc (r : Registers) (v : UInt16) : Registers :=
  { r with b := highByte v, c := lowByte v }

@[inline] def setDe (r : Registers) (v : UInt16) : Registers :=
  { r with d := highByte v, e := lowByte v }

@[inline] def setHl (r : Registers) (v : UInt16) : Registers :=
  { r with h := highByte v, l := lowByte v }

-- Flag accessors (bits 7–4 of F)
@[inline] def zeroFlag      (r : Registers) : Bool := testBit r.f 7
@[inline] def subtractFlag  (r : Registers) : Bool := testBit r.f 6
@[inline] def halfCarryFlag (r : Registers) : Bool := testBit r.f 5
@[inline] def carryFlag     (r : Registers) : Bool := testBit r.f 4

-- Flag setters
@[inline] def setZeroFlag      (r : Registers) (v : Bool) : Registers :=
  { r with f := writeBit r.f 7 v }
@[inline] def setSubtractFlag  (r : Registers) (v : Bool) : Registers :=
  { r with f := writeBit r.f 6 v }
@[inline] def setHalfCarryFlag (r : Registers) (v : Bool) : Registers :=
  { r with f := writeBit r.f 5 v }
@[inline] def setCarryFlag     (r : Registers) (v : Bool) : Registers :=
  { r with f := writeBit r.f 4 v }

-- Set all four flags at once
@[inline] def setFlags (r : Registers) (z n h c : Bool) : Registers :=
  { r with f := (r.f &&& 0x0F)
               ||| (if z then 0x80 else 0)
               ||| (if n then 0x40 else 0)
               ||| (if h then 0x20 else 0)
               ||| (if c then 0x10 else 0) }

end Registers

end LeanBoy
