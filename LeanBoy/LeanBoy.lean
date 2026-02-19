-- LeanBoy: A Game Boy emulator written in Lean 4.
-- Import all modules so the library builds as a single unit.

import LeanBoy.Debug
import LeanBoy.Utils.Bits
import LeanBoy.Utils.Types
import LeanBoy.Registers
import LeanBoy.InterruptController
import LeanBoy.Ram
import LeanBoy.Timer
import LeanBoy.Joypad
import LeanBoy.SerialPort
import LeanBoy.Cartridge.Header
import LeanBoy.Cartridge.RomOnly
import LeanBoy.Cartridge.Mbc1
import LeanBoy.Cartridge.Mbc2
import LeanBoy.Cartridge.Mbc3
import LeanBoy.Cartridge.Cartridge
import LeanBoy.Gpu.LcdControl
import LeanBoy.Gpu.LcdStat
import LeanBoy.Gpu.LcdPosition
import LeanBoy.Gpu.Palette
import LeanBoy.Gpu.OamTable
import LeanBoy.Gpu.Gpu
import LeanBoy.Apu
import LeanBoy.Bus
import LeanBoy.Cpu.Instruction
import LeanBoy.Cpu.FetchAndDecode
import LeanBoy.Cpu.Cpu
import LeanBoy.Emulator
