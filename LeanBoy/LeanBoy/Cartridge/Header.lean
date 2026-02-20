/-!
# Cartridge Header

Parses the cartridge type and bank counts from the ROM header bytes.
-/

namespace LeanBoy.Cartridge

inductive CartridgeType
  | RomOnly
  | Mbc1
  | Mbc1Ram
  | Mbc1RamBattery
  | Mbc2
  | Mbc2Battery
  | Mbc3
  | Mbc3Ram
  | Mbc3RamBattery
  | Mbc3TimerBattery     -- 0x0F: MBC3+TIMER+BATTERY       (RTC stub)
  | Mbc3TimerRamBattery  -- 0x10: MBC3+TIMER+RAM+BATTERY   (Pokémon G/S/C)
  | Mbc5
  | Mbc5Ram
  | Mbc5RamBattery
  | Mbc5Rumble
  | Mbc5RumbleRam
  | Mbc5RumbleRamBattery
  deriving Repr, BEq

namespace CartridgeType

def ofByte (b : UInt8) : Option CartridgeType :=
  match b with
  | 0x00 => some .RomOnly
  | 0x01 => some .Mbc1
  | 0x02 => some .Mbc1Ram
  | 0x03 => some .Mbc1RamBattery
  | 0x05 => some .Mbc2
  | 0x06 => some .Mbc2Battery
  | 0x0F => some .Mbc3TimerBattery
  | 0x10 => some .Mbc3TimerRamBattery
  | 0x11 => some .Mbc3
  | 0x12 => some .Mbc3Ram
  | 0x13 => some .Mbc3RamBattery
  | 0x19 => some .Mbc5
  | 0x1A => some .Mbc5Ram
  | 0x1B => some .Mbc5RamBattery
  | 0x1C => some .Mbc5Rumble
  | 0x1D => some .Mbc5RumbleRam
  | 0x1E => some .Mbc5RumbleRamBattery
  | _    => none

end CartridgeType

structure CartridgeHeader where
  cartridgeType : CartridgeType
  romBanks      : Nat
  ramBanks      : Nat
  deriving Repr

namespace CartridgeHeader

private def romBankCount (b : UInt8) : Nat :=
  match b with
  | 0x00 => 2  | 0x01 => 4  | 0x02 => 8   | 0x03 => 16
  | 0x04 => 32 | 0x05 => 64 | 0x06 => 128 | _    => 2

private def ramBankCount (b : UInt8) : Nat :=
  match b with
  | 0x02 => 1 | 0x03 => 4 | 0x04 => 16 | 0x05 => 8 | _ => 0

def parse (rom : ByteArray) : Except String CartridgeHeader := do
  if rom.size < 0x150 then
    throw "ROM too small to contain a valid header"
  let typeByte  := rom.get! 0x147
  let romByte   := rom.get! 0x148
  let ramByte   := rom.get! 0x149
  match CartridgeType.ofByte typeByte with
  | none    => throw s!"Unknown cartridge type: 0x{(Nat.toDigits 16 typeByte.toNat).foldl String.push ""}"
  | some ct => return { cartridgeType := ct
                        romBanks      := romBankCount romByte
                        ramBanks      := ramBankCount ramByte }

end CartridgeHeader

end LeanBoy.Cartridge
