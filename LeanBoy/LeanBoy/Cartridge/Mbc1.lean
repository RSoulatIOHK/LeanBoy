import LeanBoy.Utils.Bits
/-!
# MBC1 Memory Bank Controller

Supports up to 128 × 16 KB ROM banks and 4 × 8 KB RAM banks.

Register writes:
  0x0000–0x1FFF – RAM enable (write 0x0A to enable)
  0x2000–0x3FFF – ROM bank (bits 0–4)
  0x4000–0x5FFF – ROM upper bits / RAM bank (bits 0–1)
  0x6000–0x7FFF – Banking mode (0=ROM, 1=RAM)
-/

namespace LeanBoy.Cartridge

open Utils.Bits

structure Mbc1 where
  rom          : ByteArray
  ram          : ByteArray
  romBank      : UInt8 := 0x01   -- current ROM bank for 0x4000–0x7FFF
  ramBank      : UInt8 := 0x00
  ramEnabled   : Bool  := false
  bankingMode  : Bool  := false   -- false=ROM mode, true=RAM mode
  hasBattery   : Bool  := false
  ramDirty     : Bool  := false

namespace Mbc1

def create (rom ram : ByteArray) (hasBattery : Bool := false) : Mbc1 :=
  { rom, ram, hasBattery }

-- Compute the effective ROM bank (enforces that bank 0 maps to 1).
private def effectiveRomBank (m : Mbc1) : Nat :=
  let bank := m.romBank &&& 0x1F
  let upper := if m.bankingMode then 0 else m.ramBank &&& 0x03
  let full := (upper.toNat <<< 5) ||| (if bank == 0 then 1 else bank.toNat)
  let bankCount := m.rom.size / 0x4000
  full % (if bankCount == 0 then 1 else bankCount)

private def effectiveRamBank (m : Mbc1) : Nat :=
  if m.bankingMode then m.ramBank.toNat &&& 0x03 else 0

def readByte (m : Mbc1) (addr : UInt16) : UInt8 :=
  if addr < 0x4000 then
    m.rom.get! addr.toNat
  else if addr < 0x8000 then
    let offset := (effectiveRomBank m) * 0x4000 + (addr.toNat - 0x4000)
    if offset < m.rom.size then m.rom.get! offset else 0xFF
  else if addr >= 0xA000 && addr < 0xC000 then
    if m.ramEnabled && !m.ram.isEmpty then
      let offset := (effectiveRamBank m) * 0x2000 + (addr.toNat - 0xA000)
      if offset < m.ram.size then m.ram.get! offset else 0xFF
    else 0xFF
  else 0xFF

def writeByte (m : Mbc1) (addr : UInt16) (v : UInt8) : Mbc1 :=
  if addr < 0x2000 then
    { m with ramEnabled := v &&& 0x0F == 0x0A }
  else if addr < 0x4000 then
    let bank := v &&& 0x1F
    { m with romBank := if bank == 0 then 1 else bank }
  else if addr < 0x6000 then
    { m with ramBank := v &&& 0x03 }
  else if addr < 0x8000 then
    { m with bankingMode := v &&& 0x01 != 0 }
  else if addr >= 0xA000 && addr < 0xC000 then
    if m.ramEnabled && !m.ram.isEmpty then
      let offset := (effectiveRamBank m) * 0x2000 + (addr.toNat - 0xA000)
      if offset < m.ram.size then
        { m with ram := m.ram.set! offset v, ramDirty := true }
      else m
    else m
  else m

end Mbc1

end LeanBoy.Cartridge
