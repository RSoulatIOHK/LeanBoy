import LeanBoy.Gpu.LcdControl
import LeanBoy.Gpu.LcdStat
import LeanBoy.Gpu.LcdPosition
import LeanBoy.Gpu.Palette
import LeanBoy.Gpu.OamTable
import LeanBoy.InterruptController
import LeanBoy.Debug
/-!
# GPU / PPU (Picture Processing Unit)

Renders the 160×144 Game Boy screen.

The GPU cycles through four modes per scanline (456 dots total):
  OAM Search    (Mode 2) – 80 dots
  Pixel Transfer (Mode 3) – 172 dots
  HBlank        (Mode 0) – 204 dots

After 144 scanlines:
  VBlank        (Mode 1) – 10 lines × 456 dots = 4560 dots

Full frame = 154 lines × 456 dots = 70224 dots.
-/

namespace LeanBoy.Gpu

open Debug

-- A 160×144 frame buffer. Each pixel is packed RGB888: 0x00RRGGBB.
abbrev FrameBuffer := Array (Array UInt32)

-- Dot counts for each mode per scanline.
private def oamDots           : Nat := 80
private def pixelTransferDots : Nat := 172
private def hblankDots        : Nat := 204
private def scanlineDots      : Nat := 456   -- = oam + pixelTransfer + hblank
private def vblankLines       : Nat := 10
private def screenHeight      : Nat := 144
private def screenWidth       : Nat := 160

def emptyFrameBuffer : FrameBuffer :=
  Array.replicate screenHeight (Array.replicate screenWidth 0x00FFFFFF)

-- Pack a DMG grayscale value into a 0x00RRGGBB pixel.
private def dmgGray (g : UInt8) : UInt32 :=
  let v := g.toUInt32
  (v <<< 16) ||| (v <<< 8) ||| v

-- White pixel constant (for priority/transparency checks).
private def whitePixel : UInt32 := 0x00FFFFFF

-- Tile data: 384 tiles × 16 bytes each (0x8000–0x97FF).
structure TileData where
  data : ByteArray := ByteArray.mk (Array.replicate (384 * 16) 0)

-- Tile map: two 32×32 areas (0x9800–0x9FFF).
structure TileMap where
  data : ByteArray := ByteArray.mk (Array.replicate 2048 0)

structure Gpu where
  mode        : GpuMode     := .OamSearch
  dots        : Nat         := 0
  lcdControl  : LcdControl  := {}
  lcdStat     : LcdStat     := {}
  lcdPosition : LcdPosition := {}
  palettes    : Palettes    := {}
  oam         : OamTable    := {}
  tileData    : TileData    := {}   -- VRAM bank 0 tile data (0x8000–0x97FF)
  tileData1   : TileData    := {}   -- VRAM bank 1 tile data (CGB only)
  tileMap     : TileMap     := {}   -- VRAM bank 0 tile map (tile indices)
  tileAttr    : TileMap     := {}   -- VRAM bank 1 tile map (CGB tile attributes)
  vramBank    : UInt8       := 0    -- 0 or 1; selected by VBK register (0xFF4F)
  frameBuffer : FrameBuffer := emptyFrameBuffer
  cgbMode     : Bool        := false

inductive RunResult
  | InFrame
  | FrameEnded (fb : FrameBuffer)

namespace Gpu

-- ─── Debug helpers ────────────────────────────────────────────────────────────

private def modeStr : GpuMode → String
  | .HBlank        => "HBlank(0)"
  | .VBlank        => "VBlank(1)"
  | .OamSearch     => "OAMSearch(2)"
  | .PixelTransfer => "PixelTransfer(3)"

-- Dump LCDC/palette/scroll/tile-map state at the start of a new frame.
private def dumpFrameStart (gpu : Gpu) (frameNo : Nat) : IO Unit := do
  let lc  := gpu.lcdControl
  let lp  := gpu.lcdPosition
  let pal := gpu.palettes
  let tdArea := if lc.tileDataArea  then "1(0x8000-area1-unsigned)" else "0(0x8800-area0-signed)"
  let bgMap  := if lc.bgTileMapArea then "1(0x9C00)" else "0(0x9800)"
  let winMap := if lc.windowTileMapArea then "1(0x9C00)" else "0(0x9800)"
  log1 s!"[GPU] ── Frame {frameNo} start ────────────────────────────"
  log1 s!"[GPU]   LCDC=0x{hexByte lc.value}  lcdEn={lc.lcdEnable}  tileData={tdArea}"
  log1 s!"[GPU]   bgMap={bgMap}  winMap={winMap}  winEn={lc.windowEnable}  sprEn={lc.spriteEnable}  bgEn={lc.bgWindowEnable}"
  log1 s!"[GPU]   SCX={lp.scx} SCY={lp.scy}  WX={lp.wx} WY={lp.wy}  LYC={lp.lyc}"
  log1 s!"[GPU]   BGP=0x{hexByte pal.bgp.value}  OBP0=0x{hexByte pal.obp0.value}  OBP1=0x{hexByte pal.obp1.value}"
  log1 s!"[GPU]   TileMap 0x9800 row0: {hexDump gpu.tileMap.data 0    32}"
  log1 s!"[GPU]   TileMap 0x9C00 row0: {hexDump gpu.tileMap.data 1024 32}"
  dumpTileData gpu.tileData.data 4 2

-- Dump non-white pixel count to confirm rendering ran.
private def dumpFrameEnd (fb : FrameBuffer) (frameNo : Nat) : IO Unit := do
  if (← getLevel) < 1 then return
  let mut nonWhite : Nat := 0
  for row in fb do
    for px in row do
      if px != whitePixel then nonWhite := nonWhite + 1
  log1 s!"[GPU] ── Frame {frameNo} end: nonWhitePixels={nonWhite}"

-- ─── Memory access ────────────────────────────────────────────────────────────

def readByte (gpu : Gpu) (addr : UInt16) : UInt8 :=
  if addr >= 0x8000 && addr < 0x9800 then
    let offset := addr.toNat - 0x8000
    let td := if gpu.vramBank == 1 then gpu.tileData1 else gpu.tileData
    if offset < td.data.size then td.data.get! offset else 0xFF
  else if addr >= 0x9800 && addr < 0xA000 then
    let offset := addr.toNat - 0x9800
    if gpu.vramBank == 1 then gpu.tileAttr.data.get! offset
    else gpu.tileMap.data.get! offset
  else if addr >= 0xFE00 && addr < 0xFEA0 then
    gpu.oam.readByte addr
  else
    match addr with
    | 0xFF40 => gpu.lcdControl.value
    | 0xFF41 => gpu.lcdStat.value
    | 0xFF42 => gpu.lcdPosition.scy
    | 0xFF43 => gpu.lcdPosition.scx
    | 0xFF44 => gpu.lcdPosition.ly
    | 0xFF45 => gpu.lcdPosition.lyc
    | 0xFF47 => gpu.palettes.bgp.value
    | 0xFF48 => gpu.palettes.obp0.value
    | 0xFF49 => gpu.palettes.obp1.value
    | 0xFF4A => gpu.lcdPosition.wy
    | 0xFF4B => gpu.lcdPosition.wx
    | 0xFF4F => gpu.vramBank ||| 0xFE   -- VBK: low bit = bank, upper bits = 1
    | 0xFF68 => gpu.palettes.readByte addr   -- BCPS
    | 0xFF69 => gpu.palettes.readByte addr   -- BCPD
    | 0xFF6A => gpu.palettes.readByte addr   -- OCPS
    | 0xFF6B => gpu.palettes.readByte addr   -- OCPD
    | _      => 0xFF

def writeByte (gpu : Gpu) (addr : UInt16) (v : UInt8) : Gpu :=
  if addr >= 0x8000 && addr < 0x9800 then
    let offset := addr.toNat - 0x8000
    if gpu.vramBank == 1 then
      { gpu with tileData1 := { data := gpu.tileData1.data.set! offset v } }
    else
      { gpu with tileData := { data := gpu.tileData.data.set! offset v } }
  else if addr >= 0x9800 && addr < 0xA000 then
    let offset := addr.toNat - 0x9800
    if gpu.vramBank == 1 then
      { gpu with tileAttr := { data := gpu.tileAttr.data.set! offset v } }
    else
      { gpu with tileMap := { data := gpu.tileMap.data.set! offset v } }
  else if addr >= 0xFE00 && addr < 0xFEA0 then
    { gpu with oam := gpu.oam.writeByte addr v }
  else
    match addr with
    | 0xFF40 =>
      let lc' : LcdControl := { value := v }
      if !lc'.lcdEnable then
        -- LCD disabled: per spec, STAT mode → HBlank (0) so CPU "wait for HBlank"
        -- loops can exit, LY is fixed at 0, and dots reset.
        -- Keep internal mode = OamSearch so that re-enable resumes correctly.
        { gpu with lcdControl  := lc'
                 , mode        := .OamSearch
                 , lcdStat     := gpu.lcdStat.setMode .HBlank
                 , lcdPosition := { gpu.lcdPosition with ly := 0 }
                 , dots        := 0 }
      else if !gpu.lcdControl.lcdEnable then
        -- Re-enabling LCD: start fresh in OamSearch from LY=0.
        { gpu with lcdControl  := lc'
                 , mode        := .OamSearch
                 , lcdStat     := gpu.lcdStat.setMode .OamSearch
                 , lcdPosition := { gpu.lcdPosition with ly := 0 }
                 , dots        := 0 }
      else
        { gpu with lcdControl := lc' }
    | 0xFF41 => { gpu with lcdStat     := gpu.lcdStat.write v }
    | 0xFF42 => { gpu with lcdPosition := gpu.lcdPosition.writeByte addr v }
    | 0xFF43 => { gpu with lcdPosition := gpu.lcdPosition.writeByte addr v }
    | 0xFF45 => { gpu with lcdPosition := gpu.lcdPosition.writeByte addr v }
    | 0xFF47 => { gpu with palettes    := gpu.palettes.writeByte addr v }
    | 0xFF48 => { gpu with palettes    := gpu.palettes.writeByte addr v }
    | 0xFF49 => { gpu with palettes    := gpu.palettes.writeByte addr v }
    | 0xFF4A => { gpu with lcdPosition := gpu.lcdPosition.writeByte addr v }
    | 0xFF4B => { gpu with lcdPosition := gpu.lcdPosition.writeByte addr v }
    | 0xFF4F => { gpu with vramBank    := v &&& 0x01 }   -- VBK: select VRAM bank 0 or 1
    | 0xFF68 => { gpu with palettes    := gpu.palettes.writeByte addr v }   -- BCPS
    | 0xFF69 => { gpu with palettes    := gpu.palettes.writeByte addr v }   -- BCPD
    | 0xFF6A => { gpu with palettes    := gpu.palettes.writeByte addr v }   -- OCPS
    | 0xFF6B => { gpu with palettes    := gpu.palettes.writeByte addr v }   -- OCPD
    | _      => gpu

-- ─── Rendering ───────────────────────────────────────────────────────────────

-- Get a tile pixel color ID from raw TileData bytes.
-- area=true  → Area1 (0x8000-based, direct unsigned index 0-255)
-- area=false → Area0 (0x8800-based, signed index: 0..127 → tiles 256..383,
--                                                 128..255 → tiles 128..255)
private def getTilePixel (td : TileData) (area : Bool) (tileIdx : UInt8) (row col : Nat) : ColorId :=
  let index : Nat :=
    if area then tileIdx.toNat
    else
      if tileIdx < 128 then tileIdx.toNat + 256
      else (tileIdx.toNat : Nat)
  let byteBase := index * 16 + row * 2
  let lo := td.data.get! byteBase
  let hi := td.data.get! (byteBase + 1)
  let loBit := (lo >>> (7 - col).toUInt8) &&& 1
  let hiBit := (hi >>> (7 - col).toUInt8) &&& 1
  ColorId.ofUInt8 (loBit ||| (hiBit <<< 1))

-- Get tile map index from TileMap.
-- useArea1=true → 0x9C00 area (byte offset 1024); false → 0x9800 area (offset 0)
private def getTileMapIndex (tm : TileMap) (useArea1 : Bool) (x y : Nat) : UInt8 :=
  let mapBase := if useArea1 then 1024 else 0
  tm.data.get! (mapBase + (y / 8) * 32 + (x / 8))

-- Render background for scanline ly into fb.
private def renderBgLine (gpu : Gpu) (ly : Nat) (fb : FrameBuffer) : FrameBuffer :=
  let lc      := gpu.lcdControl
  let lp      := gpu.lcdPosition
  let scy     := lp.scy.toNat
  let scx     := lp.scx.toNat
  let bgY     := (scy + ly) % 256
  let tileArea  := lc.tileDataArea
  let bgMapArea := lc.bgTileMapArea
  let row := fb[ly]!
  let row' := Id.run do
    let mut r := row
    for lx in List.range 160 do
      let bgX     := (scx + lx) % 256
      let tileIdx  := getTileMapIndex gpu.tileMap bgMapArea bgX bgY
      let attr     := if gpu.cgbMode then getTileMapIndex gpu.tileAttr bgMapArea bgX bgY else 0
      -- CGB tile attributes: bit6=yFlip, bit5=xFlip, bit3=vramBank, bits2-0=palette
      let yFlip    := gpu.cgbMode && (attr &&& 0x40) != 0
      let xFlip    := gpu.cgbMode && (attr &&& 0x20) != 0
      let useBank1 := gpu.cgbMode && (attr &&& 0x08) != 0
      let palIdx   := (attr &&& 0x07).toNat
      let tileRow  := if yFlip then 7 - bgY % 8 else bgY % 8
      let tileCol  := if xFlip then 7 - bgX % 8 else bgX % 8
      let td       := if useBank1 then gpu.tileData1 else gpu.tileData
      let pixel    := getTilePixel td tileArea tileIdx tileRow tileCol
      let color    :=
        if gpu.cgbMode then
          gpu.palettes.cgbBg.lookupColor palIdx pixel.toUInt8.toNat
        else
          dmgGray (gpu.palettes.bgp.lookup pixel).toGray
      r := r.set! lx color
    return r
  fb.set! ly row'

-- Render window for scanline ly into fb (only if window is visible).
private def renderWindowLine (gpu : Gpu) (ly : Nat) (fb : FrameBuffer) : FrameBuffer :=
  let lc       := gpu.lcdControl
  let lp       := gpu.lcdPosition
  let wy       := lp.wy.toNat
  let wx       := lp.wx.toNat
  let wxScreen := if wx >= 7 then wx - 7 else 0
  if ly < wy || wxScreen >= 160 then fb
  else
    let yInW     := ly - wy
    let tileArea  := lc.tileDataArea
    let winMapArea := lc.windowTileMapArea
    let row := fb[ly]!
    let row' := Id.run do
      let mut r := row
      for lx in List.range 160 do
        if lx >= wxScreen then do
          let xInW    := lx - wxScreen
          let tileIdx  := getTileMapIndex gpu.tileMap winMapArea xInW yInW
          let attr     := if gpu.cgbMode then getTileMapIndex gpu.tileAttr winMapArea xInW yInW else 0
          let yFlip    := gpu.cgbMode && (attr &&& 0x40) != 0
          let xFlip    := gpu.cgbMode && (attr &&& 0x20) != 0
          let useBank1 := gpu.cgbMode && (attr &&& 0x08) != 0
          let palIdx   := (attr &&& 0x07).toNat
          let tileRow  := if yFlip then 7 - yInW % 8 else yInW % 8
          let tileCol  := if xFlip then 7 - xInW % 8 else xInW % 8
          let td       := if useBank1 then gpu.tileData1 else gpu.tileData
          let pixel    := getTilePixel td tileArea tileIdx tileRow tileCol
          let color    :=
            if gpu.cgbMode then
              gpu.palettes.cgbBg.lookupColor palIdx pixel.toUInt8.toNat
            else
              dmgGray (gpu.palettes.bgp.lookup pixel).toGray
          r := r.set! lx color
      return r
    fb.set! ly row'

-- Render sprites for scanline ly into fb.
private def renderSpriteLine (gpu : Gpu) (ly : Nat) (fb : FrameBuffer) : FrameBuffer :=
  let lc   := gpu.lcdControl
  let sprH := lc.spriteHeight
  let row  := fb[ly]!
  let row' := Id.run do
    let mut r := row
    for i in List.range 40 do
      let sprite       := gpu.oam.getEntry i
      let spriteScreenY := sprite.screenY
      let lyInt         := Int.ofNat ly
      if spriteScreenY <= lyInt && lyInt < spriteScreenY + Int.ofNat sprH then do
        let rowInSprite := (lyInt - spriteScreenY).toNat
        let drawRow     := if sprite.yFlip then sprH - rowInSprite - 1 else rowInSprite
        let tileIdx     := if sprH == 16 then sprite.tileIdx &&& 0xFE else sprite.tileIdx
        let dmgPal      := if sprite.palette then gpu.palettes.obp1 else gpu.palettes.obp0
        let spriteTd    := if gpu.cgbMode && sprite.cgbVramBank == 1 then gpu.tileData1
                           else gpu.tileData
        for col in List.range 8 do
          let drawCol := if sprite.xFlip then 7 - col else col
          let colorId  := getTilePixel spriteTd true tileIdx drawRow drawCol
          if colorId != .White then
            let lx := sprite.screenX + Int.ofNat col
            if lx >= 0 && lx < 160 then do
              let lxN   := lx.toNat
              let mapped :=
                if gpu.cgbMode then
                  gpu.palettes.cgbObj.lookupColor sprite.cgbPaletteIndex colorId.toUInt8.toNat
                else
                  dmgGray (dmgPal.lookup colorId).toGray
              if sprite.priority then
                if r[lxN]! == whitePixel then r := r.set! lxN mapped
              else
                r := r.set! lxN mapped
    return r
  fb.set! ly row'

-- Render the full scanline (BG → Window → Sprites).
private def renderLine (gpu : Gpu) (ly : Nat) : Gpu :=
  let fb  := gpu.frameBuffer
  let fb1 := if gpu.lcdControl.bgWindowEnable then renderBgLine gpu ly fb else fb
  let fb2 := if gpu.lcdControl.bgWindowEnable && gpu.lcdControl.windowEnable
              then renderWindowLine gpu ly fb1 else fb1
  let fb3 := if gpu.lcdControl.spriteEnable then renderSpriteLine gpu ly fb2 else fb2
  { gpu with frameBuffer := fb3 }

-- Check LYC=LY and update stat flag / fire interrupt.
private def handleLycLy (gpu : Gpu) (ic : InterruptController) :
    Gpu × InterruptController :=
  let ly   := gpu.lcdPosition.ly
  let lyc  := gpu.lcdPosition.lyc
  let eq   := ly == lyc
  let stat' := gpu.lcdStat.setLycLyFlag eq
  let ic'   := if eq && stat'.lycLyIntEnable then ic.request .LcdStat else ic
  ({ gpu with lcdStat := stat' }, ic')

-- Advance LY by one.
private def incrLy (gpu : Gpu) : Gpu :=
  { gpu with lcdPosition := { gpu.lcdPosition with ly := gpu.lcdPosition.ly + 1 } }

-- Reset LY to 0.
private def resetLy (gpu : Gpu) : Gpu :=
  { gpu with lcdPosition := { gpu.lcdPosition with ly := 0 } }

-- ─── Main tick ────────────────────────────────────────────────────────────────

-- Advance the GPU by `dots` T-cycles.
-- `frameNo` is used only for debug labelling.
-- Returns (newGpu, newIC, runResult, debugIO) — the caller must execute `debugIO`.
def tick (gpu : Gpu) (ic : InterruptController) (dots : Nat) (frameNo : Nat) :
    Gpu × InterruptController × RunResult × IO Unit :=
  if !gpu.lcdControl.lcdEnable then
    (gpu, ic, .InFrame, pure ())
  else
    let gpu1 := { gpu with dots := gpu.dots + dots }
    match gpu1.mode with
    | .OamSearch =>
      if gpu1.dots < oamDots then (gpu1, ic, .InFrame, pure ())
      else
        let gpu2 := { gpu1 with dots     := gpu1.dots - oamDots
                               , mode     := .PixelTransfer
                               , lcdStat  := gpu1.lcdStat.setMode .PixelTransfer }
        let ly := gpu2.lcdPosition.ly
        (gpu2, ic, .InFrame,
          log2 s!"[GPU] LY={ly} OAMSearch→PixelTransfer")

    | .PixelTransfer =>
      if gpu1.dots < pixelTransferDots then (gpu1, ic, .InFrame, pure ())
      else
        let gpu2 := { gpu1 with dots := gpu1.dots - pixelTransferDots }
        let gpu3 := renderLine gpu2 gpu2.lcdPosition.ly.toNat
        let gpu4 := { gpu3 with mode    := .HBlank
                               , lcdStat := gpu3.lcdStat.setMode .HBlank }
        let ic'  := if gpu4.lcdStat.hblankIntEnable then ic.request .LcdStat else ic
        let ly   := gpu4.lcdPosition.ly
        (gpu4, ic', .InFrame,
          log2 s!"[GPU] LY={ly} PixelTransfer→HBlank (scanline rendered)")

    | .HBlank =>
      if gpu1.dots < hblankDots then (gpu1, ic, .InFrame, pure ())
      else
        let gpu2 := { gpu1 with dots := gpu1.dots - hblankDots }
        let gpu3 := incrLy gpu2
        let ly   := gpu3.lcdPosition.ly.toNat
        if ly == screenHeight then
          -- Enter VBlank
          let gpu4 := { gpu3 with mode    := .VBlank
                                 , lcdStat := gpu3.lcdStat.setMode .VBlank }
          let ic1  := ic.request .VBlank
          let ic2  := if gpu4.lcdStat.vblankIntEnable then ic1.request .LcdStat else ic1
          let (gpu5, ic3) := handleLycLy gpu4 ic2
          (gpu5, ic3, .InFrame,
            log1 s!"[GPU] LY={ly} HBlank→VBlank (frame {frameNo} drawing done)")
        else
          -- Back to OAM search for next scanline
          let gpu4 := { gpu3 with mode    := .OamSearch
                                 , lcdStat := gpu3.lcdStat.setMode .OamSearch }
          let ic1  := if gpu4.lcdStat.oamIntEnable then ic.request .LcdStat else ic
          let (gpu5, ic2) := handleLycLy gpu4 ic1
          (gpu5, ic2, .InFrame,
            log3 s!"[GPU] LY={ly} HBlank→OAMSearch")

    | .VBlank =>
      if gpu1.dots < scanlineDots then (gpu1, ic, .InFrame, pure ())
      else
        let gpu2 := { gpu1 with dots := gpu1.dots - scanlineDots }
        let gpu3 := incrLy gpu2
        let ly   := gpu3.lcdPosition.ly.toNat
        if ly < screenHeight + vblankLines then
          let (gpu4, ic') := handleLycLy gpu3 ic
          (gpu4, ic', .InFrame,
            log2 s!"[GPU] VBlank LY={ly}")
        else
          -- Frame done: hand off framebuffer and reset for next frame
          let completedFb := gpu3.frameBuffer
          let gpu4 := resetLy gpu3
          let gpu5 := { gpu4 with mode        := .OamSearch
                                 , lcdStat     := gpu4.lcdStat.setMode .OamSearch
                                 , frameBuffer := emptyFrameBuffer }
          let ic1  := if gpu5.lcdStat.oamIntEnable then ic.request .LcdStat else ic
          let (gpu6, ic2) := handleLycLy gpu5 ic1
          let nextFrame := frameNo + 1
          (gpu6, ic2, .FrameEnded completedFb, do
            dumpFrameEnd completedFb frameNo
            dumpFrameStart gpu5 nextFrame)

end Gpu

end LeanBoy.Gpu
