import LeanBoy.Utils.Bits
/-!
# LCD Status Register (0xFF41)

  Bit 6 – LYC=LY STAT interrupt source enable
  Bit 5 – Mode 2 (OAM search) STAT interrupt source enable
  Bit 4 – Mode 1 (VBlank) STAT interrupt source enable
  Bit 3 – Mode 0 (HBlank) STAT interrupt source enable
  Bit 2 – LYC=LY flag (read-only, set by GPU)
  Bits 1–0 – GPU mode (read-only)
-/

namespace LeanBoy.Gpu

open Utils.Bits

inductive GpuMode
  | HBlank       -- mode 0
  | VBlank       -- mode 1
  | OamSearch    -- mode 2
  | PixelTransfer -- mode 3
  deriving Repr, BEq

namespace GpuMode

def toUInt8 : GpuMode → UInt8
  | HBlank        => 0
  | VBlank        => 1
  | OamSearch     => 2
  | PixelTransfer => 3

end GpuMode

structure LcdStat where
  value : UInt8 := 0x85
  deriving Repr

namespace LcdStat

@[inline] def lycLyIntEnable   (s : LcdStat) : Bool := testBit s.value 6
@[inline] def oamIntEnable     (s : LcdStat) : Bool := testBit s.value 5
@[inline] def vblankIntEnable  (s : LcdStat) : Bool := testBit s.value 4
@[inline] def hblankIntEnable  (s : LcdStat) : Bool := testBit s.value 3
@[inline] def lycLyFlag        (s : LcdStat) : Bool := testBit s.value 2

def setMode (s : LcdStat) (m : GpuMode) : LcdStat :=
  { s with value := (s.value &&& 0xFC) ||| m.toUInt8 }

def setLycLyFlag (s : LcdStat) (v : Bool) : LcdStat :=
  { s with value := writeBit s.value 2 v }

-- Writable bits are 3–6 only.
def write (s : LcdStat) (v : UInt8) : LcdStat :=
  { s with value := 0x80 ||| (s.value &&& 0x07) ||| (v &&& 0x78) }

end LcdStat

end LeanBoy.Gpu
