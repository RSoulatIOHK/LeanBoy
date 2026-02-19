/-!
# MBC2 Memory Bank Controller

Simpler than MBC1: up to 16 × 16 KB ROM banks and built-in 512 × 4-bit RAM.
Only the lower nibble of each RAM byte is used.

Register writes:
  0x0000–0x3FFF – If bit 8 of address is clear: RAM enable; else ROM bank select.
-/

namespace LeanBoy.Cartridge

structure Mbc2 where
  rom        : ByteArray
  ram        : ByteArray              -- 512 bytes (lower nibble only)
  romBank    : UInt8 := 0x01
  ramEnabled : Bool  := false
  hasBattery : Bool  := false
  ramDirty   : Bool  := false

namespace Mbc2

def create (rom : ByteArray) (hasBattery : Bool := false) : Mbc2 :=
  { rom, ram := ByteArray.mk (Array.replicate 512 0), hasBattery }

def readByte (m : Mbc2) (addr : UInt16) : UInt8 :=
  if addr < 0x4000 then
    m.rom.get! addr.toNat
  else if addr < 0x8000 then
    let bank := m.romBank.toNat % (m.rom.size / 0x4000 |>.max 1)
    let offset := bank * 0x4000 + (addr.toNat - 0x4000)
    if offset < m.rom.size then m.rom.get! offset else 0xFF
  else if addr >= 0xA000 && addr < 0xA200 then
    if m.ramEnabled then
      (m.ram.get! (addr.toNat - 0xA000)) ||| 0xF0  -- upper nibble reads as 1
    else 0xFF
  else 0xFF

def writeByte (m : Mbc2) (addr : UInt16) (v : UInt8) : Mbc2 :=
  if addr < 0x4000 then
    -- Bit 8 of address selects between RAM enable (0) and ROM bank (1)
    if addr &&& 0x0100 == 0 then
      { m with ramEnabled := v &&& 0x0F == 0x0A }
    else
      let bank := v &&& 0x0F
      { m with romBank := if bank == 0 then 1 else bank }
  else if addr >= 0xA000 && addr < 0xA200 then
    if m.ramEnabled then
      { m with ram := m.ram.set! (addr.toNat - 0xA000) (v &&& 0x0F), ramDirty := true }
    else m
  else m

end Mbc2

end LeanBoy.Cartridge
