/-!
# MBC3 Memory Bank Controller

Up to 128 × 16 KB ROM banks and 4 × 8 KB RAM banks. No RTC support (yet).

Register writes:
  0x0000–0x1FFF – RAM/timer enable
  0x2000–0x3FFF – ROM bank (0x00→0x01, range 0x01–0x7F)
  0x4000–0x5FFF – RAM bank (0x00–0x03) or RTC register (0x08–0x0C)
  0x6000–0x7FFF – Latch clock data (RTC; not yet implemented)
-/

namespace LeanBoy.Cartridge

structure Mbc3 where
  rom        : ByteArray
  ram        : ByteArray
  romBank    : UInt8 := 0x01
  ramBank    : UInt8 := 0x00
  ramEnabled : Bool  := false
  hasBattery : Bool  := false
  ramDirty   : Bool  := false

namespace Mbc3

def create (rom ram : ByteArray) (hasBattery : Bool := false) : Mbc3 :=
  { rom, ram, hasBattery }

def readByte (m : Mbc3) (addr : UInt16) : UInt8 :=
  if addr < 0x4000 then
    m.rom.get! addr.toNat
  else if addr < 0x8000 then
    let bank := m.romBank.toNat % (m.rom.size / 0x4000 |>.max 1)
    let offset := bank * 0x4000 + (addr.toNat - 0x4000)
    if offset < m.rom.size then m.rom.get! offset else 0xFF
  else if addr >= 0xA000 && addr < 0xC000 then
    if m.ramEnabled then
      if m.ramBank <= 0x03 then
        let offset := m.ramBank.toNat * 0x2000 + (addr.toNat - 0xA000)
        if offset < m.ram.size then m.ram.get! offset else 0xFF
      else 0xFF  -- RTC registers: return 0xFF (unimplemented)
    else 0xFF
  else 0xFF

def writeByte (m : Mbc3) (addr : UInt16) (v : UInt8) : Mbc3 :=
  if addr < 0x2000 then
    { m with ramEnabled := v &&& 0x0F == 0x0A }
  else if addr < 0x4000 then
    let bank := v &&& 0x7F
    { m with romBank := if bank == 0 then 1 else bank }
  else if addr < 0x6000 then
    { m with ramBank := v }
  else if addr < 0x8000 then
    m  -- RTC latch not yet implemented
  else if addr >= 0xA000 && addr < 0xC000 then
    if m.ramEnabled && m.ramBank <= 0x03 then
      let offset := m.ramBank.toNat * 0x2000 + (addr.toNat - 0xA000)
      if offset < m.ram.size then
        { m with ram := m.ram.set! offset v, ramDirty := true }
      else m
    else m
  else m

end Mbc3

end LeanBoy.Cartridge
