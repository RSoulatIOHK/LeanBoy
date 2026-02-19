import LeanBoy
import LeanBoy.Sdl
import LeanBoy.Debug

/-!
# LeanBoy – Entry Point (SDL2 frontend)

Pass `--debug N` (N = 1,2,3) or set `LEANBOY_DEBUG=N` in the environment to
enable verbose output.  Higher levels are increasingly noisy:
  1 – basic:   frame counts, GPU mode transitions, crashes
  2 – verbose: per-scanline info, tile-data / palette dumps
  3 – trace:   per-instruction CPU log  (very slow)
-/

open LeanBoy LeanBoy.Sdl LeanBoy.Debug

-- ─── Key handling ────────────────────────────────────────────────────────────

def applyKeys (emu : Emulator) (prev cur : UInt32) : IO Unit := do
  let pairs : List (UInt32 × Key) := [
    (2, .Right), (4, .Left),  (8, .Up),   (16, .Down),
    (32, .A),    (64, .B),    (128, .Select), (256, .Start)
  ]
  for (bit, key) in pairs do
    let wasHeld := prev &&& bit != 0
    let isHeld  := cur  &&& bit != 0
    if !wasHeld && isHeld  then emu.pressKey key
    if wasHeld  && !isHeld then emu.releaseKey key

-- ─── Debug-level resolution ──────────────────────────────────────────────────

-- Read debug level from CLI args (`--debug N`) or env var `LEANBOY_DEBUG`.
-- CLI takes precedence.
private def resolveDebugLevel (args : List String) : IO Nat := do
  let rec scan : List String → Option Nat
    | "--debug" :: n :: _ => n.toNat?
    | _ :: rest           => scan rest
    | []                  => none
  match scan args with
  | some n => return n
  | none   =>
    match ← IO.getEnv "LEANBOY_DEBUG" with
    | some s => return s.toNat?.getD 0
    | none   => return 0

-- ─── Main ────────────────────────────────────────────────────────────────────

def main (args : List String) : IO Unit := do
  -- Separate ROM path from --debug flags
  let romArgs := args.filter (fun s =>
    let isDebugFlag := s == "--debug"
    let isNumber    := s.toNat?.isSome
    !isDebugFlag && !isNumber)
  match romArgs with
  | [] =>
    IO.eprintln "Usage: leanboy [--debug N] <rom.gb>"
    IO.Process.exit 1
  | romPath :: _ =>
    -- Set debug level from CLI or env
    let dbgLevel ← resolveDebugLevel args
    setLevel dbgLevel
    if dbgLevel > 0 then
      IO.eprintln s!"[DBG] debug level = {dbgLevel}"

    -- Derive .sav path from ROM path (e.g. "game.gb" → "game.sav")
    let savPath :=
      let stem := if romPath.endsWith ".gb" || romPath.endsWith ".gbc"
                  then romPath.dropRight 3
                  else romPath
      stem ++ ".sav"

    let romBytes ← IO.FS.readBinFile romPath
    let emu ← Emulator.create romBytes (printSerial := true)

    -- Load existing .sav if present
    let mut savLoaded := false
    if ← System.FilePath.pathExists savPath then
      let savBytes ← IO.FS.readBinFile savPath
      emu.loadSaveRam savBytes
      savLoaded := true
      IO.eprintln s!"[SAV] Loaded {savBytes.size} bytes from {savPath}"

    let rc ← init
    if rc < 0 then
      IO.eprintln "Failed to initialise SDL2"
      IO.Process.exit 1

    let mut heldKeys      : UInt32 := 0
    let mut running                 := true
    let mut totalFrames : Nat      := 0
    let mut fpsTimer    : Nat ← IO.monoNanosNow
    let frameNs         : Nat      := 16_666_667  -- ~60 fps

    while running do
      let frameStart ← IO.monoNanosNow
      let events ← pollEvents
      if events &&& 1 != 0 then
        running := false
      else
        let newKeys := events &&& 0x1FE
        applyKeys emu heldKeys newKeys
        heldKeys := newKeys

        let mut frameDone  := false
        let mut frameSteps : Nat := 0
        -- Wrap the per-frame CPU loop in try/catch so a panic prints useful info.
        try
          while !frameDone do
            frameSteps := frameSteps + 1
            -- Stuck-frame watchdog: fires when a single frame takes too many CPU steps.
            -- A normal GB frame is ~17556 M-cycles; 100 000 means something is looping.
            if frameSteps == 100000 || frameSteps == 1000000 || frameSteps == 5000000 then
              let cpu ← emu.cpuRef.get
              let gpu ← emu.bus.gpu.get
              let ic  ← emu.bus.interrupt.get
              let jp  ← emu.bus.joypad.get
              -- 8 bytes at PC so we can see what instruction it's stuck on
              let pcBytes ← (List.range 8).mapM (fun i => emu.bus.readByte (cpu.pc + i.toUInt16))
              let d ← (List.range 32).mapM (fun i => emu.bus.readByte (0xC4DA + i).toUInt16)
              IO.eprintln s!"[WDG] frame={totalFrames} steps={frameSteps} \
PC=0x{hexWord cpu.pc} SP=0x{hexWord cpu.sp} IME={cpu.ime} halted={cpu.halted}"
              IO.eprintln s!"[WDG] IE=0x{hexByte ic.ie} IF=0x{hexByte ic.ifl} \
JP.p1=0x{hexByte jp.p1} dir=0x{hexByte jp.direction} act=0x{hexByte jp.action}"
              IO.eprintln s!"[WDG] PC bytes: {hexDump (ByteArray.mk pcBytes.toArray) 0 8}"
              IO.eprintln s!"[WDG] GPU mode={reprStr gpu.mode} dots={gpu.dots} \
LY={gpu.lcdPosition.ly} STAT=0x{hexByte gpu.lcdStat.value}"
              IO.eprintln s!"[WDG] 0xC4DA+: {hexDump (ByteArray.mk d.toArray) 0 32}"
            let result ← emu.step
            match result with
            | none           => pure ()
            | some (fb, audio) =>
                presentFrame (frameBufferToBytes fb)
                queueAudio audio
                frameDone     := true
                totalFrames   := totalFrames + 1
                -- FPS counter: print every 60 frames (always)
                if totalFrames % 60 == 0 then
                  let now ← IO.monoNanosNow
                  let elapsed := now - fpsTimer
                  if elapsed > 0 then
                    IO.eprintln s!"[FPS] {60 * 1000000000 / elapsed} fps (frame {totalFrames})"
                  fpsTimer ← IO.monoNanosNow
                -- Heartbeat every 60 frames at level 1
                if dbgLevel >= 1 && totalFrames % 60 == 0 then
                  IO.eprintln s!"[EMU] {totalFrames} frames rendered"
                -- Frame limiter: sleep out the remainder of the 16.67ms budget
                let frameEnd ← IO.monoNanosNow
                let elapsed := frameEnd - frameStart
                if elapsed < frameNs then
                  IO.sleep ((frameNs - elapsed) / 1_000_000).toUInt32
        catch e =>
          IO.eprintln s!"[CRASH] after {totalFrames} frames: {e}"
          let gpu ← emu.bus.gpu.get
          let fn  ← emu.frameNo.get
          IO.eprintln s!"[CRASH] GPU mode={reprStr gpu.mode}  \
LY={gpu.lcdPosition.ly}  dots={gpu.dots}  frameNo={fn}"
          IO.eprintln s!"[CRASH] LCDC=0x{hexByte gpu.lcdControl.value}  \
BGP=0x{hexByte gpu.palettes.bgp.value}  \
SCX={gpu.lcdPosition.scx} SCY={gpu.lcdPosition.scy}"
          let cpu ← emu.cpuRef.get
          IO.eprintln s!"[CRASH] PC=0x{hexWord cpu.pc}  SP=0x{hexWord cpu.sp}  \
IME={cpu.ime}  halted={cpu.halted}"
          -- Dump tile map and first 4 tiles so we can see rendering state
          IO.eprintln s!"[CRASH] TileMap 0x9800 row0: {hexDump gpu.tileMap.data 0 32}"
          IO.eprintln s!"[CRASH] TileMap 0x9C00 row0: {hexDump gpu.tileMap.data 1024 32}"
          dumpTileData gpu.tileData.data 4 0  -- always dump (level override = 0)
          running := false

    -- Save battery-backed RAM on clean exit
    let cart ← emu.bus.cartridge.get
    if cart.hasBattery && (savLoaded || cart.isRamDirty) then
      let ram := cart.getRam
      IO.FS.writeBinFile savPath ram
      IO.eprintln s!"[SAV] Saved {ram.size} bytes to {savPath}"

    destroy
