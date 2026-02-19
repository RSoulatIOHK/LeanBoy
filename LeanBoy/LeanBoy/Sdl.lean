import LeanBoy.Gpu.Gpu
/-!
# SDL2 Frontend – Lean FFI bindings

Thin wrappers around the C shim in `ffi/sdl_frontend.c`.
-/

namespace LeanBoy.Sdl

-- Initialise SDL2, create window + renderer.  Returns false on failure.
@[extern "lean_sdl_init"]
opaque init : IO Int32

-- Destroy the window and shut SDL2 down.
@[extern "lean_sdl_destroy"]
opaque destroy : IO Unit

-- Blit a completed 160×144 grayscale frame to the screen.
-- The ByteArray must be exactly 160*144 = 23040 bytes, row-major.
@[extern "lean_sdl_present_frame"]
opaque presentFrame (pixels : ByteArray) : IO Unit

-- Poll SDL events. Returns a bitmask (see C source for bit layout).
-- bit 0 = quit; bits 1–8 = key held (Right/Left/Up/Down/A/B/Select/Start).
@[extern "lean_sdl_poll_events"]
opaque pollEvents : IO UInt32

-- Queue interleaved stereo S16LE audio samples to the SDL audio device.
-- Has built-in backpressure: skips if > 4 frames are already buffered.
@[extern "lean_sdl_audio_queue"]
opaque queueAudio (samples : ByteArray) : IO Unit

-- Convert a FrameBuffer (Array (Array UInt8)) to a flat ByteArray (row-major).
def frameBufferToBytes (fb : Gpu.FrameBuffer) : ByteArray :=
  let n := fb.size * (if fb.size > 0 then fb[0]!.size else 0)
  let arr := ByteArray.mk (Array.replicate n 0)
  Id.run do
    let mut a := arr
    let mut i := 0
    for row in fb do
      for px in row do
        a := a.set! i px
        i := i + 1
    return a

end LeanBoy.Sdl
