import LeanBoy.Utils.Bits
/-!
# OAM (Object Attribute Memory) Table

Stores up to 40 sprites. Each entry is 4 bytes:
  Byte 0 – Y position (sprite top - 16)
  Byte 1 – X position (sprite left - 8)
  Byte 2 – Tile index
  Byte 3 – Flags:
    Bit 7 – Priority (0=above BG, 1=behind BG colors 1–3)
    Bit 6 – Y flip
    Bit 5 – X flip
    Bit 4 – Palette (0=OBP0, 1=OBP1)
-/

namespace LeanBoy.Gpu

open Utils.Bits

structure OamEntry where
  y        : UInt8 := 0x00
  x        : UInt8 := 0x00
  tileIdx  : UInt8 := 0x00
  flags    : UInt8 := 0x00
  deriving Repr

namespace OamEntry

@[inline] def priority (e : OamEntry) : Bool := testBit e.flags 7
@[inline] def yFlip    (e : OamEntry) : Bool := testBit e.flags 6
@[inline] def xFlip    (e : OamEntry) : Bool := testBit e.flags 5
@[inline] def palette  (e : OamEntry) : Bool := testBit e.flags 4  -- false=OBP0, true=OBP1

-- Screen Y of the sprite top pixel.
@[inline] def screenY (e : OamEntry) : Int := Int.ofNat e.y.toNat - 16
-- Screen X of the sprite left pixel.
@[inline] def screenX (e : OamEntry) : Int := Int.ofNat e.x.toNat - 8

end OamEntry

-- The full OAM region, mapped to 0xFE00–0xFE9F (160 bytes = 40 × 4).
structure OamTable where
  data : ByteArray := ByteArray.mk (Array.replicate 160 0)

namespace OamTable

def readByte (oam : OamTable) (addr : UInt16) : UInt8 :=
  let offset := addr.toNat - 0xFE00
  if offset < 160 then oam.data.get! offset else 0xFF

def writeByte (oam : OamTable) (addr : UInt16) (v : UInt8) : OamTable :=
  let offset := addr.toNat - 0xFE00
  if offset < 160 then { oam with data := oam.data.set! offset v } else oam

-- Get the nth sprite entry (0–39).
def getEntry (oam : OamTable) (n : Nat) : OamEntry :=
  let base := n * 4
  { y       := oam.data.get! base
    x       := oam.data.get! (base + 1)
    tileIdx := oam.data.get! (base + 2)
    flags   := oam.data.get! (base + 3) }

-- Direct DMA write of 160 bytes from a source array.
def dmaLoad (oam : OamTable) (src : ByteArray) (srcBase : Nat) : OamTable :=
  let newData := Id.run do
    let mut d := oam.data
    for i in List.range 160 do
      d := d.set! i (src.get! (srcBase + i))
    return d
  { oam with data := newData }

end OamTable

end LeanBoy.Gpu
