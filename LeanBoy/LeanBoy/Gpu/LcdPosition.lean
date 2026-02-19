/-!
# LCD Position Registers

  LY  (0xFF44) – Current scanline (0–153, read-only)
  LYC (0xFF45) – LY Compare
  SCX (0xFF43) – Scroll X
  SCY (0xFF42) – Scroll Y
  WX  (0xFF4B) – Window X position minus 7
  WY  (0xFF4A) – Window Y position
-/

namespace LeanBoy.Gpu

structure LcdPosition where
  ly  : UInt8 := 0x00
  lyc : UInt8 := 0x00
  scx : UInt8 := 0x00
  scy : UInt8 := 0x00
  wx  : UInt8 := 0x00
  wy  : UInt8 := 0x00
  deriving Repr

namespace LcdPosition

def readByte (pos : LcdPosition) (addr : UInt16) : UInt8 :=
  match addr with
  | 0xFF42 => pos.scy
  | 0xFF43 => pos.scx
  | 0xFF44 => pos.ly
  | 0xFF45 => pos.lyc
  | 0xFF4A => pos.wy
  | 0xFF4B => pos.wx
  | _      => 0xFF

def writeByte (pos : LcdPosition) (addr : UInt16) (v : UInt8) : LcdPosition :=
  match addr with
  | 0xFF42 => { pos with scy := v }
  | 0xFF43 => { pos with scx := v }
  | 0xFF44 => pos                   -- LY is read-only
  | 0xFF45 => { pos with lyc := v }
  | 0xFF4A => { pos with wy  := v }
  | 0xFF4B => { pos with wx  := v }
  | _      => pos

end LcdPosition

end LeanBoy.Gpu
