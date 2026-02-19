/-!
# Debug Utilities

Global verbosity-gated logging for LeanBoy.

Set the level at runtime via:
  - Command-line flag  `--debug N`  (handled in Main.lean)
  - Environment variable `LEANBOY_DEBUG=N`

Verbosity levels:
  0 – off (default)
  1 – basic: frame counts, GPU mode transitions, palette/LCDC at frame start, crashes
  2 – verbose: per-scanline tile-map/tile-data dumps, interrupt requests
  3 – trace: per-instruction CPU log (very noisy)
-/

namespace LeanBoy.Debug

-- One global mutable cell for the current verbosity level.
initialize g_level : IO.Ref Nat ← IO.mkRef 0

def setLevel (n : Nat) : IO Unit := g_level.set n
def getLevel : IO Nat             := g_level.get

-- Emit `msg` to stderr if the current level ≥ `minLevel`.
@[inline] def log (minLevel : Nat) (msg : String) : IO Unit := do
  if (← g_level.get) >= minLevel then IO.eprintln msg

@[inline] def log1 (msg : String) : IO Unit := log 1 msg
@[inline] def log2 (msg : String) : IO Unit := log 2 msg
@[inline] def log3 (msg : String) : IO Unit := log 3 msg

-- ─── Hex formatting ──────────────────────────────────────────────────────────

private def hexChar (n : Nat) : Char :=
  if n < 10 then Char.ofNat (n + '0'.toNat)
  else           Char.ofNat (n - 10 + 'a'.toNat)

def hexByte (b : UInt8) : String :=
  s!"{hexChar ((b >>> 4).toNat)}{hexChar ((b &&& 0x0F).toNat)}"

def hexWord (w : UInt16) : String :=
  let hi : UInt8 := (w >>> 8).toUInt8
  let lo : UInt8 := w.toUInt8
  s!"{hexByte hi}{hexByte lo}"

-- Dump up to `len` bytes from a ByteArray starting at `offset` as "xx xx …".
def hexDump (data : ByteArray) (offset len : Nat) : String :=
  let stop := min (offset + len) data.size
  if stop <= offset then "<empty>"
  else
    " ".intercalate
      ((List.range (stop - offset)).map (fun i => hexByte (data.get! (offset + i))))

-- Decode a raw tile row (lo byte, hi byte) into a string of color IDs 0-3.
def tileRowPixels (lo hi : UInt8) : String :=
  (List.range 8).map (fun col =>
    let loBit := (lo >>> (7 - col).toUInt8) &&& 1
    let hiBit := (hi >>> (7 - col).toUInt8) &&& 1
    let id    := (hiBit <<< 1) ||| loBit
    Char.ofNat (id.toNat + '0'.toNat))
  |> String.mk

-- Print the first `numTiles` tiles from raw TileData bytes.
def dumpTileData (data : ByteArray) (numTiles : Nat) (minLevel : Nat) : IO Unit := do
  if (← g_level.get) < minLevel then return
  for t in List.range numTiles do
    let base := t * 16
    IO.eprintln s!"  tile[{t}]:"
    for row in List.range 8 do
      let lo := data.get! (base + row * 2)
      let hi := data.get! (base + row * 2 + 1)
      IO.eprintln s!"    row{row}: lo={hexByte lo} hi={hexByte hi}  pixels={tileRowPixels lo hi}"

-- Print the first row of a tile map (32 bytes).
def dumpTileMapRow0 (data : ByteArray) (mapBase : Nat) (minLevel : Nat) : IO Unit := do
  if (← g_level.get) < minLevel then return
  IO.eprintln s!"  tileMap row0 (32 indices): {hexDump data mapBase 32}"

end LeanBoy.Debug
