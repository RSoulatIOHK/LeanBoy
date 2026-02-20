/-!
# MBC5 Memory Bank Controller

Up to 512 × 16 KB ROM banks (9-bit bank number) and 16 × 8 KB RAM banks.
Used by the majority of GBC-exclusive games.

Register writes:
  0x0000–0x1FFF – RAM enable (0x0A = enable, anything else = disable)
  0x2000–0x2FFF – ROM bank low byte (bits 7–0 of 9-bit bank)
  0x3000–0x3FFF – ROM bank high bit (bit 0 = bit 8 of bank)
  0x4000–0x5FFF – RAM bank (0x00–0x0F; bit 3 controls rumble motor if present)
-/

namespace LeanBoy.Cartridge

structure Mbc5 where
  rom        : ByteArray
  ram        : ByteArray
  romBankLo  : UInt8 := 0x01   -- bits 7–0 of ROM bank
  romBankHi  : UInt8 := 0x00   -- bit 0 = bit 8 of ROM bank
  ramBank    : UInt8 := 0x00
  ramEnabled : Bool  := false
  hasBattery : Bool  := false
  hasRumble  : Bool  := false
  ramDirty   : Bool  := false

namespace Mbc5

def create (rom ram : ByteArray) (hasBattery hasRumble : Bool := false) : Mbc5 :=
  { rom, ram, hasBattery, hasRumble }

private def romBank (m : Mbc5) : Nat :=
  m.romBankLo.toNat ||| (m.romBankHi.toNat &&& 1) <<< 8

def readByte (m : Mbc5) (addr : UInt16) : UInt8 :=
  if addr < 0x4000 then
    m.rom.get! addr.toNat
  else if addr < 0x8000 then
    let bank   := romBank m % (m.rom.size / 0x4000 |>.max 1)
    let offset := bank * 0x4000 + (addr.toNat - 0x4000)
    if offset < m.rom.size then m.rom.get! offset else 0xFF
  else if addr >= 0xA000 && addr < 0xC000 then
    if m.ramEnabled then
      let offset := m.ramBank.toNat * 0x2000 + (addr.toNat - 0xA000)
      if offset < m.ram.size then m.ram.get! offset else 0xFF
    else 0xFF
  else 0xFF

def writeByte (m : Mbc5) (addr : UInt16) (v : UInt8) : Mbc5 :=
  if addr < 0x2000 then
    { m with ramEnabled := v &&& 0x0F == 0x0A }
  else if addr < 0x3000 then
    { m with romBankLo := v }
  else if addr < 0x4000 then
    { m with romBankHi := v &&& 0x01 }
  else if addr < 0x6000 then
    -- Mask off rumble bit (bit 3) for RAM bank; keep bits 3-0 otherwise
    let bank := if m.hasRumble then v &&& 0x07 else v &&& 0x0F
    { m with ramBank := bank }
  else if addr >= 0xA000 && addr < 0xC000 then
    if m.ramEnabled then
      let offset := m.ramBank.toNat * 0x2000 + (addr.toNat - 0xA000)
      if offset < m.ram.size then
        { m with ram := m.ram.set! offset v, ramDirty := true }
      else m
    else m
  else m

end Mbc5

end LeanBoy.Cartridge
