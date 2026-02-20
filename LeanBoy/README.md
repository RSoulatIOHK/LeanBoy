# LeanBoy

![Lean 4](https://img.shields.io/badge/Lean-4.24.0-purple?logo=lean)
![License](https://img.shields.io/badge/license-Apache%202.0-blue)
![Platform](https://img.shields.io/badge/platform-macOS%20%7C%20Linux-lightgrey)
![SDL2](https://img.shields.io/badge/SDL2-required-orange)
![Game Boy](https://img.shields.io/badge/Game%20Boy-DMG-8bac0f?logo=nintendogameboy&logoColor=white)
![Game Boy Color](https://img.shields.io/badge/Game%20Boy%20Color-GBC-e40034?logo=nintendogameboy&logoColor=white)

A Game Boy / Game Boy Color emulator written in [Lean 4](https://lean-lang.org/).

## Features

- Full SM83 CPU (all 256 opcodes + CB prefix)
- DMG and CGB (Game Boy Color) support
- GPU with BG/window/sprite rendering, CGB palette RAM, VRAM banking, tile attribute support
- APU with all 4 channels and SDL2 audio output
- MBC1, MBC2, MBC3, MBC5 cartridge mappers
- Battery-backed save RAM (`.sav` files auto-loaded/saved)
- SDL2 display and input

## Requirements

- [Lean 4 / Lake](https://lean-lang.org/lean4/doc/setup.html) (toolchain: `leanprover/lean4:v4.24.0`)
- SDL2 (`brew install sdl2` on macOS)

## Build

```sh
cd LeanBoy
lake build
```

## Run

```sh
.lake/build/bin/leanboy <rom.gb|rom.gbc>
```

### Options

| Flag | Description |
|------|-------------|
| `--debug N` | Verbose output (1=basic, 2=verbose, 3=trace) |

Debug level can also be set via the `LEANBOY_DEBUG` environment variable.

## Controls

| Key | Button |
|-----|--------|
| Arrow keys | D-Pad |
| Z | B |
| X | A |
| Enter | Start |
| Backspace | Select |

## Project Structure

```
LeanBoy/
├── Cpu/           # SM83 fetch/decode and execution
├── Gpu/           # PPU, LCD control, OAM, palettes
├── Cartridge/     # ROM-only, MBC1-5 mappers
├── Utils/         # Bit manipulation helpers
├── Apu.lean       # Audio processing unit
├── Bus.lean       # Memory bus, WRAM banking, HDMA
├── Emulator.lean  # Top-level emulator state
├── Timer.lean     # GB timer (DIV/TIMA/TMA/TAC)
├── Joypad.lean    # Input handling
└── Main.lean      # SDL2 frontend entry point
```
