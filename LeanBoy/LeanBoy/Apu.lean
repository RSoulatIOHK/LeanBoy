/-!
# APU – Game Boy Audio Processing Unit

Implements all 4 channels:
  CH1 – Square wave with frequency sweep
  CH2 – Square wave
  CH3 – Programmable wave (Wave RAM)
  CH4 – Noise (LFSR)

Output: interleaved stereo S16LE samples at 44 100 Hz.
The `tick` function is pure (matches the GPU/Timer pattern) and appends
samples to `Apu.samples`. The frontend drains that buffer once per frame.
-/

namespace LeanBoy

-- ─── Duty table ──────────────────────────────────────────────────────────────
private def dutyTable : Array (Array UInt8) :=
  #[ #[0,0,0,0,0,0,0,1]
   , #[1,0,0,0,0,0,0,1]
   , #[1,0,0,0,1,1,1,1]
   , #[0,1,1,1,1,1,1,0] ]

-- ─── Square Channel (CH1 and CH2) ────────────────────────────────────────────

structure SquareChannel where
  enabled       : Bool   := false
  dacEnabled    : Bool   := false
  lengthCounter : UInt8  := 0
  lengthEnabled : Bool   := false
  dutyIndex     : UInt8  := 0
  dutyPos       : UInt8  := 0
  freqLow       : UInt8  := 0
  freqHigh      : UInt8  := 0
  freqTimer     : UInt16 := 0
  volume        : UInt8  := 0
  initVolume    : UInt8  := 0
  envIncrement  : Bool   := false
  envPeriod     : UInt8  := 0
  envTimer      : UInt8  := 0
  sweepEnabled  : Bool   := false
  sweepPeriod   : UInt8  := 0
  sweepTimer    : UInt8  := 0
  sweepShift    : UInt8  := 0
  sweepNegate   : Bool   := false
  sweepShadow   : UInt16 := 0

namespace SquareChannel

def freqPeriod (ch : SquareChannel) : UInt16 :=
  let freq := ch.freqLow.toUInt16 ||| (ch.freqHigh.toUInt16 <<< 8)
  (2048 - freq) * 4

def output (ch : SquareChannel) : UInt8 :=
  if !ch.enabled || !ch.dacEnabled then 0
  else
    let row := dutyTable[ch.dutyIndex.toNat]!
    row[ch.dutyPos.toNat]! * ch.volume

def trigger (ch : SquareChannel) (withSweep : Bool) : SquareChannel :=
  let freq := ch.freqLow.toUInt16 ||| (ch.freqHigh.toUInt16 <<< 8)
  let len  := if ch.lengthCounter == 0 then 64 else ch.lengthCounter
  let ch'  := { ch with
    enabled       := ch.dacEnabled
    lengthCounter := len
    freqTimer     := ch.freqPeriod
    volume        := ch.initVolume
    envTimer      := if ch.envPeriod == 0 then 8 else ch.envPeriod }
  if withSweep then
    { ch' with
      sweepShadow  := freq
      sweepTimer   := if ch'.sweepPeriod == 0 then 8 else ch'.sweepPeriod
      sweepEnabled := ch'.sweepPeriod != 0 || ch'.sweepShift != 0 }
  else ch'

def clockFreq (ch : SquareChannel) (tCycles : UInt32) : SquareChannel :=
  if !ch.enabled then ch
  else
    -- Closed-form: avoid per-iteration struct allocation.
    let timer := ch.freqTimer.toUInt32
    if tCycles < timer then
      { ch with freqTimer := (timer - tCycles).toUInt16 }
    else
      let period := ch.freqPeriod.toUInt32
      if period == 0 then ch  -- safety guard (shouldn't occur on real hardware)
      else
        let rem0 := tCycles - timer
        let k    := rem0 / period
        let rem1 := rem0 % period
        -- If rem1 = 0 the last expiry consumed exactly to zero, so reload to period.
        let newTimer : UInt16 := if rem1 == 0 then period.toUInt16 else (period - rem1).toUInt16
        let newPos := ((ch.dutyPos.toUInt32 + k + 1) % 8).toUInt8
        { ch with freqTimer := newTimer, dutyPos := newPos }

def clockLength (ch : SquareChannel) : SquareChannel :=
  if ch.lengthEnabled && ch.lengthCounter > 0 then
    let lc := ch.lengthCounter - 1
    { ch with lengthCounter := lc, enabled := ch.enabled && lc > 0 }
  else ch

def clockEnvelope (ch : SquareChannel) : SquareChannel :=
  if ch.envPeriod == 0 then ch
  else
    let t := if ch.envTimer == 0 then 0 else ch.envTimer - 1
    if t == 0 then
      let vol :=
        if ch.envIncrement then
          if ch.volume < 15 then ch.volume + 1 else 15
        else
          if ch.volume > 0 then ch.volume - 1 else 0
      { ch with envTimer := ch.envPeriod, volume := vol }
    else { ch with envTimer := t }

private def sweepCalcFreq (ch : SquareChannel) : UInt16 :=
  let delta := ch.sweepShadow >>> ch.sweepShift.toUInt16
  if ch.sweepNegate then ch.sweepShadow - delta
  else ch.sweepShadow + delta

def clockSweep (ch : SquareChannel) : SquareChannel :=
  if !ch.sweepEnabled then ch
  else
    let t := if ch.sweepTimer == 0 then 0 else ch.sweepTimer - 1
    if t == 0 then
      let t' : UInt8 := if ch.sweepPeriod == 0 then 8 else ch.sweepPeriod
      let newFreq := sweepCalcFreq ch
      if ch.sweepShift != 0 && newFreq < 2048 then
        let lo := newFreq.toUInt8
        let hi := (newFreq >>> 8).toUInt8
        let chk := sweepCalcFreq { ch with sweepShadow := newFreq }
        { ch with sweepTimer  := t'
                  sweepShadow := newFreq
                  freqLow     := lo
                  freqHigh    := hi
                  enabled     := ch.enabled && chk < 2048 }
      else
        { ch with sweepTimer := t' }
    else { ch with sweepTimer := t }

end SquareChannel

-- ─── Wave Channel (CH3) ──────────────────────────────────────────────────────

structure WaveChannel where
  enabled       : Bool      := false
  dacEnabled    : Bool      := false
  lengthCounter : UInt16    := 0
  lengthEnabled : Bool      := false
  freqLow       : UInt8     := 0
  freqHigh      : UInt8     := 0
  freqTimer     : UInt16    := 0
  volumeCode    : UInt8     := 0
  wavePos       : UInt8     := 0
  waveRam       : ByteArray := ByteArray.mk (Array.replicate 16 0)

namespace WaveChannel

def freqPeriod (ch : WaveChannel) : UInt16 :=
  let freq := ch.freqLow.toUInt16 ||| (ch.freqHigh.toUInt16 <<< 8)
  (2048 - freq) * 2

def output (ch : WaveChannel) : UInt8 :=
  if !ch.enabled || !ch.dacEnabled then 0
  else
    let byteIdx := ch.wavePos / 2
    let b := ch.waveRam.get! byteIdx.toNat
    let nibble : UInt8 := if ch.wavePos % 2 == 0 then b >>> 4 else b &&& 0x0F
    match ch.volumeCode with
    | 0 => 0
    | 1 => nibble
    | 2 => nibble >>> 1
    | _ => nibble >>> 2

def trigger (ch : WaveChannel) : WaveChannel :=
  let len := if ch.lengthCounter == 0 then 256 else ch.lengthCounter
  { ch with
    enabled       := ch.dacEnabled
    lengthCounter := len
    wavePos       := 0
    freqTimer     := ch.freqPeriod }

def clockFreq (ch : WaveChannel) (tCycles : UInt32) : WaveChannel :=
  if !ch.enabled then ch
  else
    let timer := ch.freqTimer.toUInt32
    if tCycles < timer then
      { ch with freqTimer := (timer - tCycles).toUInt16 }
    else
      let period := ch.freqPeriod.toUInt32
      if period == 0 then ch
      else
        let rem0 := tCycles - timer
        let k    := rem0 / period
        let rem1 := rem0 % period
        let newTimer : UInt16 := if rem1 == 0 then period.toUInt16 else (period - rem1).toUInt16
        let newPos := ((ch.wavePos.toUInt32 + k + 1) % 32).toUInt8
        { ch with freqTimer := newTimer, wavePos := newPos }

def clockLength (ch : WaveChannel) : WaveChannel :=
  if ch.lengthEnabled && ch.lengthCounter > 0 then
    let lc := ch.lengthCounter - 1
    { ch with lengthCounter := lc, enabled := ch.enabled && lc > 0 }
  else ch

end WaveChannel

-- ─── Noise Channel (CH4) ─────────────────────────────────────────────────────

structure NoiseChannel where
  enabled       : Bool   := false
  dacEnabled    : Bool   := false
  lengthCounter : UInt8  := 0
  lengthEnabled : Bool   := false
  volume        : UInt8  := 0
  initVolume    : UInt8  := 0
  envIncrement  : Bool   := false
  envPeriod     : UInt8  := 0
  envTimer      : UInt8  := 0
  lfsr          : UInt16 := 0x7FFF
  wideMode      : Bool   := false
  clockShift    : UInt8  := 0
  divisorCode   : UInt8  := 0
  freqTimer     : UInt16 := 0

namespace NoiseChannel

private def divisorTable : Array UInt16 :=
  #[8, 16, 32, 48, 64, 80, 96, 112]

def freqPeriod (ch : NoiseChannel) : UInt16 :=
  let d := divisorTable[ch.divisorCode.toNat % 8]!
  -- Compute in UInt32 to avoid UInt16 overflow for high clockShift values.
  -- clockShift ≥ 13 with small divisors overflows UInt16 to 0, which causes
  -- an infinite loop in clockFreq (step = min rem 0 = 0, rem never decreases).
  -- Hardware behaviour for those cases is effectively silence; cap at 0xFFFF.
  let period : UInt32 := d.toUInt32 <<< ch.clockShift.toUInt32
  (min period 0xFFFF).toUInt16

def output (ch : NoiseChannel) : UInt8 :=
  if !ch.enabled || !ch.dacEnabled then 0
  else (~~~ch.lfsr &&& (1 : UInt16)).toUInt8 * ch.volume

def trigger (ch : NoiseChannel) : NoiseChannel :=
  let len := if ch.lengthCounter == 0 then 64 else ch.lengthCounter
  { ch with
    enabled       := ch.dacEnabled
    lengthCounter := len
    lfsr          := 0x7FFF
    freqTimer     := ch.freqPeriod
    volume        := ch.initVolume
    envTimer      := if ch.envPeriod == 0 then 8 else ch.envPeriod }

private def stepLfsr (lfsr : UInt16) (wide : Bool) : UInt16 :=
  let xorBit : UInt16 := (lfsr &&& (1 : UInt16)) ^^^ ((lfsr >>> 1) &&& (1 : UInt16))
  let shifted := lfsr >>> 1
  let r := shifted ||| (xorBit <<< 14)
  if wide then r ||| (xorBit <<< 6) else r

def clockFreq (ch : NoiseChannel) (tCycles : UInt32) : NoiseChannel :=
  if !ch.enabled then ch
  else
    -- Closed-form timer expiry count; only the LFSR needs a sequential loop
    -- (each LFSR step depends on the previous), everything else is arithmetic.
    let timer := ch.freqTimer.toUInt32
    if tCycles < timer then
      { ch with freqTimer := (timer - tCycles).toUInt16 }
    else
      let period := ch.freqPeriod.toUInt32
      if period == 0 then ch
      else
        let rem0     := tCycles - timer
        let k        := rem0 / period
        let rem1     := rem0 % period
        let newTimer : UInt16 := if rem1 == 0 then period.toUInt16 else (period - rem1).toUInt16
        -- Step LFSR (k+1) times. Only lfsr (UInt16) is mutable — no struct alloc per step.
        let wide := ch.wideMode
        let lfsr := Id.run do
          let mut lfsr := ch.lfsr
          let mut i    := k + 1
          while i > 0 do
            lfsr := stepLfsr lfsr wide
            i    := i - 1
          return lfsr
        { ch with freqTimer := newTimer, lfsr }

def clockLength (ch : NoiseChannel) : NoiseChannel :=
  if ch.lengthEnabled && ch.lengthCounter > 0 then
    let lc := ch.lengthCounter - 1
    { ch with lengthCounter := lc, enabled := ch.enabled && lc > 0 }
  else ch

def clockEnvelope (ch : NoiseChannel) : NoiseChannel :=
  if ch.envPeriod == 0 then ch
  else
    let t := if ch.envTimer == 0 then 0 else ch.envTimer - 1
    if t == 0 then
      let vol :=
        if ch.envIncrement then
          if ch.volume < 15 then ch.volume + 1 else 15
        else
          if ch.volume > 0 then ch.volume - 1 else 0
      { ch with envTimer := ch.envPeriod, volume := vol }
    else { ch with envTimer := t }

end NoiseChannel

-- ─── APU ─────────────────────────────────────────────────────────────────────

structure Apu where
  enabled        : Bool         := false
  ch1            : SquareChannel := {}
  ch2            : SquareChannel := {}
  ch3            : WaveChannel   := {}
  ch4            : NoiseChannel  := {}
  masterVolLeft  : UInt8        := 7
  masterVolRight : UInt8        := 7
  panLeft        : UInt8        := 0xF
  panRight       : UInt8        := 0xF
  frameSeqTimer  : UInt32       := 0
  frameSeqStep   : UInt8        := 0
  sampleAcc      : UInt32       := 0
  -- Pre-allocated sample buffer (800 stereo S16LE samples = 3200 bytes).
  -- Using indexed writes instead of push to avoid O(n) copies per sample.
  samples        : ByteArray    := ByteArray.mk (Array.replicate 3200 0)
  sampleCount    : UInt32       := 0
  -- High-pass filter state (capacitor simulation, one per stereo channel).
  -- Stored as plain Int in the same ±32767 scale as raw samples (no floats needed).
  capLeft        : Int          := 0
  capRight       : Int          := 0

namespace Apu

-- Encode an S16LE sample (v : Int in [-32768, 32767]) as two bytes in `buf` at `off`.
-- Two's complement trick: (65536 + v).toNat.toUInt16 wraps correctly for both signs.
private def writeS16i (buf : ByteArray) (off : Nat) (v : Int) : ByteArray :=
  let bits : UInt16 := ((65536 + v).toNat).toUInt16
  (buf.set! off bits.toUInt8).set! (off + 1) (bits >>> 8).toUInt8

private def emitSample (apu : Apu) : Apu :=
  let c1 := apu.ch1.output.toNat
  let c2 := apu.ch2.output.toNat
  let c3 := apu.ch3.output.toNat
  let c4 := apu.ch4.output.toNat
  let lp := apu.panLeft
  let rp := apu.panRight
  let sumL : Nat :=
    (if lp &&& 1 != 0 then c1 else 0) +
    (if lp &&& 2 != 0 then c2 else 0) +
    (if lp &&& 4 != 0 then c3 else 0) +
    (if lp &&& 8 != 0 then c4 else 0)
  let sumR : Nat :=
    (if rp &&& 1 != 0 then c1 else 0) +
    (if rp &&& 2 != 0 then c2 else 0) +
    (if rp &&& 4 != 0 then c3 else 0) +
    (if rp &&& 8 != 0 then c4 else 0)
  -- Scale to [0, 32767] (integer; max sumL=60, masterVol+1≤8, product ≤ 15.7M, fits Int)
  let rawL : Int := Int.ofNat (sumL * (apu.masterVolLeft.toNat  + 1) * 32767 / (60 * 8))
  let rawR : Int := Int.ofNat (sumR * (apu.masterVolRight.toNat + 1) * 32767 / (60 * 8))
  -- First-order HPF: α ≈ 0.998 → cap' = (cap*998 + raw*2) / 1000
  -- All values in [-32767, 32767]; max intermediate = 32767*998 ≈ 32.7M — fits Int without GMP.
  let capL' := (apu.capLeft  * 998 + rawL * 2) / 1000
  let capR' := (apu.capRight * 998 + rawR * 2) / 1000
  let vL    := rawL - capL'
  let vR    := rawR - capR'
  let outL  : Int := if vL < -32768 then -32768 else if vL > 32767 then 32767 else vL
  let outR  : Int := if vR < -32768 then -32768 else if vR > 32767 then 32767 else vR
  let off := apu.sampleCount.toNat * 4
  let buf := writeS16i (writeS16i apu.samples off outL) (off + 2) outR
  { apu with
    capLeft     := capL'
    capRight    := capR'
    samples     := buf
    sampleCount := apu.sampleCount + 1 }

private def clockLength (apu : Apu) : Apu :=
  { apu with
    ch1 := apu.ch1.clockLength
    ch2 := apu.ch2.clockLength
    ch3 := apu.ch3.clockLength
    ch4 := apu.ch4.clockLength }

private def clockEnvelope (apu : Apu) : Apu :=
  { apu with
    ch1 := apu.ch1.clockEnvelope
    ch2 := apu.ch2.clockEnvelope
    ch4 := apu.ch4.clockEnvelope }

private def clockSweep (apu : Apu) : Apu :=
  { apu with ch1 := apu.ch1.clockSweep }

private def frameSeqClock (apu : Apu) (step : UInt8) : Apu :=
  match step % 8 with
  | 0 => clockLength apu
  | 2 => clockSweep (clockLength apu)
  | 4 => clockLength apu
  | 6 => clockSweep (clockLength apu)
  | 7 => clockEnvelope apu
  | _ => apu

-- ─── Main tick ────────────────────────────────────────────────────────────────

private def tickSampleAcc (apu : Apu) (tCycles : UInt32) : Apu :=
  let sampleRate : UInt32 := 44100
  let gbClock    : UInt32 := 4194304
  Id.run do
    let mut a := { apu with sampleAcc := apu.sampleAcc + tCycles * sampleRate }
    while a.sampleAcc >= gbClock do
      a := emitSample a
      a := { a with sampleAcc := a.sampleAcc - gbClock }
    return a

def tick (apu : Apu) (tCycles : UInt32) : Apu :=
  if !apu.enabled then
    -- APU disabled: still advance sample clock to avoid SDL queue starvation.
    -- emitSample with all channels disabled outputs 0 (after HPF settles).
    tickSampleAcc apu tCycles
  else Id.run do
    let mut a := apu
    let newFst := a.frameSeqTimer + tCycles
    let steps  := newFst / 8192
    let mut step := a.frameSeqStep
    let mut iSteps := steps
    while iSteps > 0 do
      a := frameSeqClock a step
      step := (step + 1) % 8
      iSteps := iSteps - 1
    a := { a with frameSeqTimer := newFst % 8192, frameSeqStep := step }
    a := { a with
      ch1 := a.ch1.clockFreq tCycles
      ch2 := a.ch2.clockFreq tCycles
      ch3 := a.ch3.clockFreq tCycles
      ch4 := a.ch4.clockFreq tCycles }
    return tickSampleAcc a tCycles

-- ─── Register reads ──────────────────────────────────────────────────────────

def readByte (apu : Apu) (addr : UInt16) : UInt8 :=
  match addr with
  | 0xFF10 =>
    let ch := apu.ch1
    (if ch.sweepNegate then (0x08 : UInt8) else 0) |||
    (ch.sweepShift &&& (0x07 : UInt8)) |||
    ((ch.sweepPeriod &&& (0x07 : UInt8)) <<< 4) ||| 0x80
  | 0xFF11 => ((apu.ch1.dutyIndex &&& 0x03) <<< 6) ||| 0x3F
  | 0xFF12 =>
    let ch := apu.ch1
    (ch.initVolume <<< 4) |||
    (if ch.envIncrement then (0x08 : UInt8) else 0) |||
    (ch.envPeriod &&& 0x07)
  | 0xFF13 => 0xFF
  | 0xFF14 => (if apu.ch1.lengthEnabled then (0x40 : UInt8) else 0) ||| 0xBF
  | 0xFF15 => 0xFF
  | 0xFF16 => ((apu.ch2.dutyIndex &&& 0x03) <<< 6) ||| 0x3F
  | 0xFF17 =>
    let ch := apu.ch2
    (ch.initVolume <<< 4) |||
    (if ch.envIncrement then (0x08 : UInt8) else 0) |||
    (ch.envPeriod &&& 0x07)
  | 0xFF18 => 0xFF
  | 0xFF19 => (if apu.ch2.lengthEnabled then (0x40 : UInt8) else 0) ||| 0xBF
  | 0xFF1A => (if apu.ch3.dacEnabled then (0x80 : UInt8) else 0) ||| 0x7F
  | 0xFF1B => 0xFF
  | 0xFF1C => ((apu.ch3.volumeCode &&& 0x03) <<< 5) ||| 0x9F
  | 0xFF1D => 0xFF
  | 0xFF1E => (if apu.ch3.lengthEnabled then (0x40 : UInt8) else 0) ||| 0xBF
  | 0xFF1F => 0xFF
  | 0xFF20 => 0xFF
  | 0xFF21 =>
    let ch := apu.ch4
    (ch.initVolume <<< 4) |||
    (if ch.envIncrement then (0x08 : UInt8) else 0) |||
    (ch.envPeriod &&& 0x07)
  | 0xFF22 =>
    let ch := apu.ch4
    (ch.clockShift <<< 4) |||
    (if ch.wideMode then (0x08 : UInt8) else 0) |||
    (ch.divisorCode &&& 0x07)
  | 0xFF23 => (if apu.ch4.lengthEnabled then (0x40 : UInt8) else 0) ||| 0xBF
  | 0xFF24 => ((apu.masterVolLeft &&& 0x07) <<< 4) ||| (apu.masterVolRight &&& 0x07)
  | 0xFF25 => (apu.panLeft <<< 4) ||| apu.panRight
  | 0xFF26 =>
    let status : UInt8 :=
      (if apu.ch1.enabled then (0x01 : UInt8) else 0) |||
      (if apu.ch2.enabled then (0x02 : UInt8) else 0) |||
      (if apu.ch3.enabled then (0x04 : UInt8) else 0) |||
      (if apu.ch4.enabled then (0x08 : UInt8) else 0)
    (if apu.enabled then (0x80 : UInt8) else 0) ||| status ||| 0x70
  | addr' =>
    if addr' >= 0xFF30 && addr' < 0xFF40 then
      apu.ch3.waveRam.get! (addr' - 0xFF30).toNat
    else 0xFF

-- ─── Register writes ─────────────────────────────────────────────────────────

def writeByte (apu : Apu) (addr : UInt16) (v : UInt8) : Apu :=
  if addr >= 0xFF30 && addr < 0xFF40 then
    let i := (addr - 0xFF30).toNat
    { apu with ch3 := { apu.ch3 with waveRam := apu.ch3.waveRam.set! i v } }
  else if addr == 0xFF26 then
    let en := v &&& 0x80 != 0
    if en then { apu with enabled := true }
    else
      { apu with
        enabled := false
        ch1     := {}
        ch2     := {}
        ch3     := { dacEnabled := false, waveRam := apu.ch3.waveRam }
        ch4     := {} }
  else if !apu.enabled then
    match addr with
    | 0xFF11 => { apu with ch1 := { apu.ch1 with lengthCounter := 64 - (v &&& 0x3F) } }
    | 0xFF16 => { apu with ch2 := { apu.ch2 with lengthCounter := 64 - (v &&& 0x3F) } }
    | 0xFF1B => { apu with ch3 := { apu.ch3 with lengthCounter := 256 - v.toUInt16 } }
    | 0xFF20 => { apu with ch4 := { apu.ch4 with lengthCounter := 64 - (v &&& 0x3F) } }
    | _      => apu
  else
  match addr with
  | 0xFF10 =>
    { apu with ch1 := { apu.ch1 with
        sweepPeriod := (v >>> 4) &&& 0x07
        sweepNegate := v &&& 0x08 != 0
        sweepShift  := v &&& 0x07 } }
  | 0xFF11 =>
    { apu with ch1 := { apu.ch1 with
        dutyIndex     := (v >>> 6) &&& 0x03
        lengthCounter := 64 - (v &&& 0x3F) } }
  | 0xFF12 =>
    let dacOn := v &&& 0xF8 != 0
    { apu with ch1 := { apu.ch1 with
        initVolume   := (v >>> 4) &&& 0x0F
        envIncrement := v &&& 0x08 != 0
        envPeriod    := v &&& 0x07
        dacEnabled   := dacOn
        enabled      := apu.ch1.enabled && dacOn } }
  | 0xFF13 => { apu with ch1 := { apu.ch1 with freqLow := v } }
  | 0xFF14 =>
    let ch' := { apu.ch1 with freqHigh := v &&& 0x07, lengthEnabled := v &&& 0x40 != 0 }
    { apu with ch1 := if v &&& 0x80 != 0 then ch'.trigger true else ch' }
  | 0xFF15 => apu
  | 0xFF16 =>
    { apu with ch2 := { apu.ch2 with
        dutyIndex     := (v >>> 6) &&& 0x03
        lengthCounter := 64 - (v &&& 0x3F) } }
  | 0xFF17 =>
    let dacOn := v &&& 0xF8 != 0
    { apu with ch2 := { apu.ch2 with
        initVolume   := (v >>> 4) &&& 0x0F
        envIncrement := v &&& 0x08 != 0
        envPeriod    := v &&& 0x07
        dacEnabled   := dacOn
        enabled      := apu.ch2.enabled && dacOn } }
  | 0xFF18 => { apu with ch2 := { apu.ch2 with freqLow := v } }
  | 0xFF19 =>
    let ch' := { apu.ch2 with freqHigh := v &&& 0x07, lengthEnabled := v &&& 0x40 != 0 }
    { apu with ch2 := if v &&& 0x80 != 0 then ch'.trigger false else ch' }
  | 0xFF1A =>
    let dacOn := v &&& 0x80 != 0
    { apu with ch3 := { apu.ch3 with
        dacEnabled := dacOn
        enabled    := apu.ch3.enabled && dacOn } }
  | 0xFF1B => { apu with ch3 := { apu.ch3 with lengthCounter := 256 - v.toUInt16 } }
  | 0xFF1C => { apu with ch3 := { apu.ch3 with volumeCode := (v >>> 5) &&& 0x03 } }
  | 0xFF1D => { apu with ch3 := { apu.ch3 with freqLow := v } }
  | 0xFF1E =>
    let ch' := { apu.ch3 with freqHigh := v &&& 0x07, lengthEnabled := v &&& 0x40 != 0 }
    { apu with ch3 := if v &&& 0x80 != 0 then ch'.trigger else ch' }
  | 0xFF1F => apu
  | 0xFF20 => { apu with ch4 := { apu.ch4 with lengthCounter := 64 - (v &&& 0x3F) } }
  | 0xFF21 =>
    let dacOn := v &&& 0xF8 != 0
    { apu with ch4 := { apu.ch4 with
        initVolume   := (v >>> 4) &&& 0x0F
        envIncrement := v &&& 0x08 != 0
        envPeriod    := v &&& 0x07
        dacEnabled   := dacOn
        enabled      := apu.ch4.enabled && dacOn } }
  | 0xFF22 =>
    { apu with ch4 := { apu.ch4 with
        clockShift  := (v >>> 4) &&& 0x0F
        wideMode    := v &&& 0x08 != 0
        divisorCode := v &&& 0x07 } }
  | 0xFF23 =>
    let ch' := { apu.ch4 with lengthEnabled := v &&& 0x40 != 0 }
    { apu with ch4 := if v &&& 0x80 != 0 then ch'.trigger else ch' }
  | 0xFF24 =>
    { apu with
      masterVolLeft  := (v >>> 4) &&& 0x07
      masterVolRight := v &&& 0x07 }
  | 0xFF25 =>
    { apu with
      panLeft  := (v >>> 4) &&& 0x0F
      panRight := v &&& 0x0F }
  | _ => apu

end Apu

end LeanBoy
