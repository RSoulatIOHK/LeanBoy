import LeanBoy.Cartridge.Header
import LeanBoy.Cartridge.RomOnly
import LeanBoy.Cartridge.Mbc1
import LeanBoy.Cartridge.Mbc2
import LeanBoy.Cartridge.Mbc3
/-!
# Cartridge

Top-level dispatcher that wraps all cartridge variants.
-/

namespace LeanBoy.Cartridge

inductive Cartridge
  | romOnly (c : RomOnly)
  | mbc1    (c : Mbc1)
  | mbc2    (c : Mbc2)
  | mbc3    (c : Mbc3)

namespace Cartridge

def readByte (cart : Cartridge) (addr : UInt16) : UInt8 :=
  match cart with
  | romOnly c => c.readByte addr
  | mbc1    c => c.readByte addr
  | mbc2    c => c.readByte addr
  | mbc3    c => c.readByte addr

def writeByte (cart : Cartridge) (addr : UInt16) (v : UInt8) : Cartridge :=
  match cart with
  | romOnly c => romOnly (c.writeByte addr v)
  | mbc1    c => mbc1    (c.writeByte addr v)
  | mbc2    c => mbc2    (c.writeByte addr v)
  | mbc3    c => mbc3    (c.writeByte addr v)

-- Returns the battery-backed RAM (empty ByteArray if none).
def getRam (cart : Cartridge) : ByteArray :=
  match cart with
  | romOnly _ => ByteArray.empty
  | mbc1    c => c.ram
  | mbc2    c => c.ram
  | mbc3    c => c.ram

-- Replace the RAM contents (used to restore a .sav file).
def withRam (cart : Cartridge) (ram : ByteArray) : Cartridge :=
  match cart with
  | romOnly c => romOnly c
  | mbc1    c => mbc1 { c with ram }
  | mbc2    c => mbc2 { c with ram }
  | mbc3    c => mbc3 { c with ram }

def hasBattery (cart : Cartridge) : Bool :=
  match cart with
  | romOnly _ => false
  | mbc1    c => c.hasBattery
  | mbc2    c => c.hasBattery
  | mbc3    c => c.hasBattery

def isRamDirty (cart : Cartridge) : Bool :=
  match cart with
  | romOnly _ => false
  | mbc1    c => c.ramDirty
  | mbc2    c => c.ramDirty
  | mbc3    c => c.ramDirty

-- Detect cartridge type and create the appropriate variant.
def detect (rom : ByteArray) : Except String Cartridge := do
  let header ← CartridgeHeader.parse rom
  let ramSize := header.ramBanks * 0x2000
  let ram := ByteArray.mk (Array.replicate ramSize 0)
  match header.cartridgeType with
  | .RomOnly         => return romOnly (RomOnly.create rom)
  | .Mbc1            => return mbc1 (Mbc1.create rom ByteArray.empty)
  | .Mbc1Ram         => return mbc1 (Mbc1.create rom ram)
  | .Mbc1RamBattery  => return mbc1 (Mbc1.create rom ram (hasBattery := true))
  | .Mbc2            => return mbc2 (Mbc2.create rom)
  | .Mbc2Battery     => return mbc2 (Mbc2.create rom (hasBattery := true))
  | .Mbc3            => return mbc3 (Mbc3.create rom ByteArray.empty)
  | .Mbc3Ram         => return mbc3 (Mbc3.create rom ram)
  | .Mbc3RamBattery  => return mbc3 (Mbc3.create rom ram (hasBattery := true))

end Cartridge

end LeanBoy.Cartridge
