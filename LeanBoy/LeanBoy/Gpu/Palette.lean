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

-- ─── CGB Colour Palette RAM ──────────────────────────────────────────────────
-- 64-byte RAM (8 palettes × 4 colors × 2 bytes each, RGB555 little-endian).
-- The index register (BCPS/OCPS) packs a byte address in bits 5–0 and an
-- auto-increment flag in bit 7.
structure CgbPaletteRam where
  data  : ByteArray := ByteArray.mk (Array.replicate 64 0xFF)
  index : UInt8     := 0

namespace CgbPaletteRam

def readIndex (ram : CgbPaletteRam) : UInt8 := ram.index

def writeIndex (ram : CgbPaletteRam) (v : UInt8) : CgbPaletteRam :=
  { ram with index := v }

def readData (ram : CgbPaletteRam) : UInt8 :=
  ram.data.get! (ram.index &&& 0x3F).toNat

def writeData (ram : CgbPaletteRam) (v : UInt8) : CgbPaletteRam :=
  let idx    := (ram.index &&& 0x3F).toNat
  let data'  := ram.data.set! idx v
  let index' :=
    if ram.index &&& 0x80 != 0 then
      0x80 ||| ((ram.index + 1) &&& 0x3F)   -- auto-increment, preserve bit 7
    else ram.index
  { data := data', index := index' }

-- Decode a color: paletteIdx 0–7, colorId 0–3 → packed 0x00RRGGBB.
-- RGB555 layout: bits 4–0 = R, bits 9–5 = G, bits 14–10 = B.
def lookupColor (ram : CgbPaletteRam) (paletteIdx colorId : Nat) : UInt32 :=
  let base := paletteIdx * 8 + colorId * 2
  let lo   := (ram.data.get! base).toUInt32
  let hi   := (ram.data.get! (base + 1)).toUInt32
  let word := lo ||| (hi <<< 8)
  let r5   := word &&& 0x1F
  let g5   := (word >>> 5)  &&& 0x1F
  let b5   := (word >>> 10) &&& 0x1F
  -- Scale 5-bit → 8-bit: n * 255 / 31
  let r8 := r5 * 255 / 31
  let g8 := g5 * 255 / 31
  let b8 := b5 * 255 / 31
  (r8 <<< 16) ||| (g8 <<< 8) ||| b8

end CgbPaletteRam

-- ─── Palettes bundle ─────────────────────────────────────────────────────────

structure Palettes where
  bgp    : Palette       := { value := 0xFC }
  obp0   : Palette       := { value := 0xFF }
  obp1   : Palette       := { value := 0xFF }
  cgbBg  : CgbPaletteRam := {}
  cgbObj : CgbPaletteRam := {}

namespace Palettes

def readByte (ps : Palettes) (addr : UInt16) : UInt8 :=
  match addr with
  | 0xFF47 => ps.bgp.value
  | 0xFF48 => ps.obp0.value
  | 0xFF49 => ps.obp1.value
  | 0xFF68 => ps.cgbBg.readIndex
  | 0xFF69 => ps.cgbBg.readData
  | 0xFF6A => ps.cgbObj.readIndex
  | 0xFF6B => ps.cgbObj.readData
  | _      => 0xFF

def writeByte (ps : Palettes) (addr : UInt16) (v : UInt8) : Palettes :=
  match addr with
  | 0xFF47 => { ps with bgp    := { value := v } }
  | 0xFF48 => { ps with obp0   := { value := v } }
  | 0xFF49 => { ps with obp1   := { value := v } }
  | 0xFF68 => { ps with cgbBg  := ps.cgbBg.writeIndex  v }
  | 0xFF69 => { ps with cgbBg  := ps.cgbBg.writeData   v }
  | 0xFF6A => { ps with cgbObj := ps.cgbObj.writeIndex v }
  | 0xFF6B => { ps with cgbObj := ps.cgbObj.writeData  v }
  | _      => ps

end Palettes

end LeanBoy.Gpu
