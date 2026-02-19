import LeanBoy.Utils.Bits
/-!
# LCD Control Register (0xFF40)

  Bit 7 – LCD enable
  Bit 6 – Window tile map area (0=0x9800, 1=0x9C00)
  Bit 5 – Window enable
  Bit 4 – BG/Window tile data area (0=0x8800, 1=0x8000)
  Bit 3 – BG tile map area (0=0x9800, 1=0x9C00)
  Bit 2 – Sprite size (0=8×8, 1=8×16)
  Bit 1 – Sprite enable
  Bit 0 – BG/Window enable
-/

namespace LeanBoy.Gpu

open Utils.Bits

structure LcdControl where
  value : UInt8 := 0x91
  deriving Repr

namespace LcdControl

@[inline] def lcdEnable         (lc : LcdControl) : Bool := testBit lc.value 7
@[inline] def windowTileMapArea (lc : LcdControl) : Bool := testBit lc.value 6
@[inline] def windowEnable      (lc : LcdControl) : Bool := testBit lc.value 5
@[inline] def tileDataArea      (lc : LcdControl) : Bool := testBit lc.value 4
@[inline] def bgTileMapArea     (lc : LcdControl) : Bool := testBit lc.value 3
@[inline] def spriteSize        (lc : LcdControl) : Bool := testBit lc.value 2   -- false=8x8, true=8x16
@[inline] def spriteEnable      (lc : LcdControl) : Bool := testBit lc.value 1
@[inline] def bgWindowEnable    (lc : LcdControl) : Bool := testBit lc.value 0

-- Height of sprites in pixels.
@[inline] def spriteHeight (lc : LcdControl) : Nat :=
  if lc.spriteSize then 16 else 8

end LcdControl

end LeanBoy.Gpu
