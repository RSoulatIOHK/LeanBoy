/-!
# ROM-Only Cartridge

No bank switching. ROM is fixed at 0x0000–0x7FFF (32 KB).
-/

namespace LeanBoy.Cartridge

structure RomOnly where
  rom : ByteArray

namespace RomOnly

def create (rom : ByteArray) : RomOnly := { rom }

def readByte (c : RomOnly) (addr : UInt16) : UInt8 :=
  if addr < 0x8000 then c.rom.get! addr.toNat else 0xFF

-- ROM-only cartridges ignore all writes.
def writeByte (c : RomOnly) (_ : UInt16) (_ : UInt8) : RomOnly := c

end RomOnly

end LeanBoy.Cartridge
