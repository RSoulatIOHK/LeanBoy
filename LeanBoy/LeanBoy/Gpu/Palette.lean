/-!
# Palettes

BGP  (0xFF47) – Background palette
OBP0 (0xFF48) – Object palette 0
OBP1 (0xFF49) – Object palette 1

Each palette maps 4 color IDs (2 bits each) to a Game Boy gray shade:
  0 = White, 1 = Light gray, 2 = Dark gray, 3 = Black

The byte encodes:
  Bits 7–6 → color for ID 3
  Bits 5–4 → color for ID 2
  Bits 3–2 → color for ID 1
  Bits 1–0 → color for ID 0
-/

namespace LeanBoy.Gpu

-- Game Boy color IDs (2bpp).
inductive ColorId
  | White | LightGray | DarkGray | Black
  deriving Repr, BEq

namespace ColorId

def toUInt8 : ColorId → UInt8
  | White     => 0
  | LightGray => 1
  | DarkGray  => 2
  | Black     => 3

def ofUInt8 (v : UInt8) : ColorId :=
  match v &&& 0x03 with
  | 0 => .White
  | 1 => .LightGray
  | 2 => .DarkGray
  | _ => .Black

-- Convert to an 8-bit RGB grayscale value (for framebuffer output).
def toGray : ColorId → UInt8
  | White     => 255
  | LightGray => 170
  | DarkGray  => 85
  | Black     => 0

end ColorId

structure Palette where
  value : UInt8 := 0xFC
  deriving Repr

namespace Palette

-- Look up the color for a 2-bit color ID.
def lookup (p : Palette) (id : ColorId) : ColorId :=
  let shift := id.toUInt8 * 2
  ColorId.ofUInt8 (p.value >>> shift)

end Palette

structure Palettes where
  bgp  : Palette := { value := 0xFC }
  obp0 : Palette := { value := 0xFF }
  obp1 : Palette := { value := 0xFF }
  deriving Repr

namespace Palettes

def readByte (ps : Palettes) (addr : UInt16) : UInt8 :=
  match addr with
  | 0xFF47 => ps.bgp.value
  | 0xFF48 => ps.obp0.value
  | 0xFF49 => ps.obp1.value
  | _      => 0xFF

def writeByte (ps : Palettes) (addr : UInt16) (v : UInt8) : Palettes :=
  match addr with
  | 0xFF47 => { ps with bgp  := { value := v } }
  | 0xFF48 => { ps with obp0 := { value := v } }
  | 0xFF49 => { ps with obp1 := { value := v } }
  | _      => ps

end Palettes

end LeanBoy.Gpu
