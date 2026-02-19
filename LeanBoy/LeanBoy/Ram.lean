/-!
# RAM

A simple byte-addressable RAM region backed by a `ByteArray`.
Used for WRAM, HRAM (ZRAM), VRAM, and OAM.
-/

namespace LeanBoy

structure Ram where
  start : UInt16
  stop  : UInt16       -- inclusive upper bound
  data  : ByteArray

namespace Ram

-- Create a zeroed RAM region for the given address range.
def create (start stop : UInt16) : Ram :=
  let size := stop.toNat - start.toNat + 1
  { start, stop, data := ByteArray.mk (Array.replicate size 0) }

-- Check whether an address falls within this RAM's range.
@[inline] def accepts (ram : Ram) (addr : UInt16) : Bool :=
  ram.start <= addr && addr <= ram.stop

-- Read a byte (returns 0xFF for out-of-range access).
def readByte (ram : Ram) (addr : UInt16) : UInt8 :=
  if ram.accepts addr then
    ram.data.get! (addr.toNat - ram.start.toNat)
  else 0xFF

-- Write a byte (ignored for out-of-range access).
def writeByte (ram : Ram) (addr : UInt16) (v : UInt8) : Ram :=
  if ram.accepts addr then
    { ram with data := ram.data.set! (addr.toNat - ram.start.toNat) v }
  else ram

end Ram

end LeanBoy
