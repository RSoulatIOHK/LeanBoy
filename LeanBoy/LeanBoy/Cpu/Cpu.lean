import LeanBoy.Registers
import LeanBoy.Bus
import LeanBoy.Cpu.Instruction
import LeanBoy.Cpu.FetchAndDecode
import LeanBoy.Debug
/-!
# CPU

The Sharp SM83 (Game Boy) CPU.

Holds registers, program counter, stack pointer, halt flag, and
interrupt master enable (IME). Execution is driven by `runInstruction`.
-/

namespace LeanBoy.Cpu

open LeanBoy

structure Cpu where
  registers : Registers := {}
  pc        : UInt16    := 0x0100   -- post-boot ROM start
  sp        : UInt16    := 0xFFFE
  halted    : Bool      := false
  ime       : Bool      := false    -- interrupt master enable
  imeNext   : Bool      := false    -- delayed EI effect
  prevPc    : UInt16    := 0x0000   -- PC of the last executed instruction (for crash diagnostics)
  prevPc2   : UInt16    := 0x0000   -- PC before prevPc
  bus       : Bus

namespace Cpu

-- Push a 16-bit value onto the stack.
def push (cpu : Cpu) (v : UInt16) : IO Cpu := do
  let sp := cpu.sp - 2
  cpu.bus.writeWord sp v
  return { cpu with sp }

-- Pop a 16-bit value from the stack.
def pop (cpu : Cpu) : IO (UInt16 × Cpu) := do
  let v ← cpu.bus.readWord cpu.sp
  return (v, { cpu with sp := cpu.sp + 2 })

-- Test a condition against the current flags.
def testCondition (cpu : Cpu) (cond : Condition) : Bool :=
  match cond with
  | .Always => true
  | .Z      => cpu.registers.zeroFlag
  | .NZ     => !cpu.registers.zeroFlag
  | .CY     => cpu.registers.carryFlag
  | .NC     => !cpu.registers.carryFlag

-- Read from an 8-bit register.
def readReg8 (cpu : Cpu) (r : Reg8) : UInt8 :=
  match r with
  | .A => cpu.registers.a
  | .B => cpu.registers.b
  | .C => cpu.registers.c
  | .D => cpu.registers.d
  | .E => cpu.registers.e
  | .H => cpu.registers.h
  | .L => cpu.registers.l

-- Write to an 8-bit register.
def writeReg8 (cpu : Cpu) (r : Reg8) (v : UInt8) : Cpu :=
  match r with
  | .A => { cpu with registers := { cpu.registers with a := v } }
  | .B => { cpu with registers := { cpu.registers with b := v } }
  | .C => { cpu with registers := { cpu.registers with c := v } }
  | .D => { cpu with registers := { cpu.registers with d := v } }
  | .E => { cpu with registers := { cpu.registers with e := v } }
  | .H => { cpu with registers := { cpu.registers with h := v } }
  | .L => { cpu with registers := { cpu.registers with l := v } }

-- Read from a 16-bit register pair.
def readReg16 (cpu : Cpu) (r : Reg16) : UInt16 :=
  match r with
  | .AF => cpu.registers.af
  | .BC => cpu.registers.bc
  | .DE => cpu.registers.de
  | .HL => cpu.registers.hl
  | .SP => cpu.sp

-- Write to a 16-bit register pair.
def writeReg16 (cpu : Cpu) (r : Reg16) (v : UInt16) : Cpu :=
  match r with
  | .AF => { cpu with registers := cpu.registers.setAf v }
  | .BC => { cpu with registers := cpu.registers.setBc v }
  | .DE => { cpu with registers := cpu.registers.setDe v }
  | .HL => { cpu with registers := cpu.registers.setHl v }
  | .SP => { cpu with sp := v }

-- Handle pending interrupts. Returns (cycles, cpu) where cycles > 0 if
-- an interrupt was serviced (or HALT was exited).
def handleInterrupts (cpu : Cpu) : IO (Nat × Cpu) := do
  let ic ← cpu.bus.interrupt.get
  match ic.nextPending with
  | none => return (0, cpu)
  | some t =>
    if !cpu.ime then
      -- IME=0: interrupt not serviced, but HALT still exits (GB hardware quirk)
      if cpu.halted then return (1, { cpu with halted := false })
      else return (0, cpu)
    else
      -- Clear the interrupt flag
      cpu.bus.interrupt.modify (fun i => i.clear t)
      -- Disable IME, clear HALT, push PC, jump to vector
      -- Log ALL interrupts when SP is in the crash range (app SP 0xe440-0xe44e)
      if cpu.sp >= 0xe440 && cpu.sp <= 0xe44e then do
        let words ← (List.range 16).mapM (fun i => cpu.bus.readWord (cpu.sp + (2 * i).toUInt16))
        let stackStr := String.intercalate " " (words.map (fun w => "0x" ++ Debug.hexWord w))
        IO.eprintln s!"[INT_FIRE_CRASH] pushing pc=0x{Debug.hexWord cpu.pc}  int={reprStr t}  \
SP=0x{Debug.hexWord cpu.sp}  prevPc=0x{Debug.hexWord cpu.prevPc}  stack: {stackStr}"
      let cpu' ← push { cpu with ime := false, halted := false } cpu.pc
      return (5, { cpu' with pc := InterruptController.vector t })

-- ──────────────────────────────────────────────────────────────────────────────
-- Private ALU helpers
-- ──────────────────────────────────────────────────────────────────────────────

-- 8-bit add: returns new registers with A = a + b and Z/N/H/C set.
private def aluAdd (regs : Registers) (a b : UInt8) : Registers :=
  let av := a.toNat; let bv := b.toNat
  let r := av + bv
  let res := r.toUInt8
  { (regs.setFlags (res == 0) false ((av &&& 0xF) + (bv &&& 0xF) > 0xF) (r > 0xFF)) with a := res }

-- 8-bit add with carry.
private def aluAdc (regs : Registers) (a b : UInt8) : Registers :=
  let carry := if regs.carryFlag then 1 else 0
  let av := a.toNat; let bv := b.toNat
  let r := av + bv + carry
  let res := r.toUInt8
  { (regs.setFlags (res == 0) false ((av &&& 0xF) + (bv &&& 0xF) + carry > 0xF) (r > 0xFF)) with a := res }

-- 8-bit subtract: returns new registers with A = a - b.
private def aluSub (regs : Registers) (a b : UInt8) : Registers :=
  let av := a.toNat; let bv := b.toNat
  let res := a - b
  { (regs.setFlags (res == 0) true ((av &&& 0xF) < (bv &&& 0xF)) (av < bv)) with a := res }

-- 8-bit subtract with borrow.
private def aluSbc (regs : Registers) (a b : UInt8) : Registers :=
  let carry := if regs.carryFlag then 1 else 0
  let av := a.toNat; let bv := b.toNat
  let res := a - b - carry.toUInt8
  { (regs.setFlags (res == 0) true ((av &&& 0xF) < (bv &&& 0xF) + carry) (av < bv + carry)) with a := res }

-- Bitwise AND: Z set, N=0, H=1, C=0.
private def aluAnd (regs : Registers) (a b : UInt8) : Registers :=
  let res := a &&& b
  { (regs.setFlags (res == 0) false true false) with a := res }

-- Bitwise OR: Z set, N=0, H=0, C=0.
private def aluOr (regs : Registers) (a b : UInt8) : Registers :=
  let res := a ||| b
  { (regs.setFlags (res == 0) false false false) with a := res }

-- Bitwise XOR: Z set, N=0, H=0, C=0.
private def aluXor (regs : Registers) (a b : UInt8) : Registers :=
  let res := a ^^^ b
  { (regs.setFlags (res == 0) false false false) with a := res }

-- Compare (like SUB but A unchanged).
private def aluCp (regs : Registers) (a b : UInt8) : Registers :=
  let av := a.toNat; let bv := b.toNat
  regs.setFlags ((a - b) == 0) true ((av &&& 0xF) < (bv &&& 0xF)) (av < bv)

-- INC: flags Z,N=0,H set; C unchanged.
private def aluInc (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let res := v + 1
  (res, regs.setFlags (res == 0) false ((v &&& 0x0F) == 0x0F) regs.carryFlag)

-- DEC: flags Z,N=1,H set; C unchanged.
private def aluDec (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let res := v - 1
  (res, regs.setFlags (res == 0) true ((v &&& 0x0F) == 0x00) regs.carryFlag)

-- Rotate left circular (RLC): C = bit7, result = (v << 1) | C.
private def rlc8 (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let carry := (v &&& 0x80) != 0
  let res := (v <<< 1) ||| (if carry then 1 else 0)
  (res, regs.setFlags (res == 0) false false carry)

-- Rotate right circular (RRC): C = bit0, result = (v >> 1) | (C << 7).
private def rrc8 (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let carry := (v &&& 0x01) != 0
  let res := (v >>> 1) ||| (if carry then 0x80 else 0)
  (res, regs.setFlags (res == 0) false false carry)

-- Rotate left through carry (RL): new_C = bit7, result = (v << 1) | old_C.
private def rl8 (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let oldC : UInt8 := if regs.carryFlag then 1 else 0
  let newC := (v &&& 0x80) != 0
  let res := (v <<< 1) ||| oldC
  (res, regs.setFlags (res == 0) false false newC)

-- Rotate right through carry (RR): new_C = bit0, result = (v >> 1) | (old_C << 7).
private def rr8 (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let oldC : UInt8 := if regs.carryFlag then 0x80 else 0
  let newC := (v &&& 0x01) != 0
  let res := (v >>> 1) ||| oldC
  (res, regs.setFlags (res == 0) false false newC)

-- Shift left arithmetic (SLA): C = bit7, result = v << 1.
private def sla8 (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let carry := (v &&& 0x80) != 0
  let res := v <<< 1
  (res, regs.setFlags (res == 0) false false carry)

-- Shift right arithmetic (SRA): C = bit0, MSB preserved.
private def sra8 (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let carry := (v &&& 0x01) != 0
  let res := (v >>> 1) ||| (v &&& 0x80)
  (res, regs.setFlags (res == 0) false false carry)

-- Shift right logical (SRL): C = bit0, MSB = 0.
private def srl8 (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let carry := (v &&& 0x01) != 0
  let res := v >>> 1
  (res, regs.setFlags (res == 0) false false carry)

-- Swap nibbles: Z set, N=0, H=0, C=0.
private def swap8 (regs : Registers) (v : UInt8) : UInt8 × Registers :=
  let res := (v <<< 4) ||| (v >>> 4)
  (res, regs.setFlags (res == 0) false false false)

-- Sign-extend a UInt8 offset to UInt16 (for SP ± n arithmetic).
private def signExt8to16 (b : UInt8) : UInt16 :=
  if b < 0x80 then b.toUInt16 else 0xFF00 ||| b.toUInt16

-- ──────────────────────────────────────────────────────────────────────────────
-- Instruction execution
-- ──────────────────────────────────────────────────────────────────────────────

def executeInstruction (cpu : Cpu) (decoded : DecodedInst) : IO (Nat × Cpu) := do
  let cy  := decoded.cyclesTaken
  let cy2 := decoded.cyclesNotTaken
  match decoded.inst with
  -- ── Control ───────────────────────────────────────────────────────────────
  | .NOP  => return (cy, cpu)
  | .HALT => return (cy, { cpu with halted := true })
  | .STOP => return (cy, cpu)
  | .DI   => return (cy, { cpu with ime := false, imeNext := false })
  | .EI   => return (cy, { cpu with imeNext := true })
  -- ── 8-bit loads: register ↔ register ─────────────────────────────────────
  | .LD_rr dst src =>
    return (cy, cpu.writeReg8 dst (cpu.readReg8 src))
  | .LD_r_n dst n =>
    return (cy, cpu.writeReg8 dst n)
  | .LD_r_HL dst => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, cpu.writeReg8 dst v)
  | .LD_HL_r src => do
    cpu.bus.writeByte cpu.registers.hl (cpu.readReg8 src)
    return (cy, cpu)
  | .LD_HL_n n => do
    cpu.bus.writeByte cpu.registers.hl n
    return (cy, cpu)
  -- ── 8-bit loads: (BC)/(DE) indirect ──────────────────────────────────────
  | .LD_A_BC => do
    let v ← cpu.bus.readByte cpu.registers.bc
    return (cy, { cpu with registers := { cpu.registers with a := v } })
  | .LD_A_DE => do
    let v ← cpu.bus.readByte cpu.registers.de
    return (cy, { cpu with registers := { cpu.registers with a := v } })
  | .LD_BC_A => do
    cpu.bus.writeByte cpu.registers.bc cpu.registers.a
    return (cy, cpu)
  | .LD_DE_A => do
    cpu.bus.writeByte cpu.registers.de cpu.registers.a
    return (cy, cpu)
  -- ── 8-bit loads: absolute address ─────────────────────────────────────────
  | .LD_A_nn addr => do
    let v ← cpu.bus.readByte addr
    return (cy, { cpu with registers := { cpu.registers with a := v } })
  | .LD_nn_A addr => do
    cpu.bus.writeByte addr cpu.registers.a
    return (cy, cpu)
  -- ── 8-bit loads: 0xFF00 + offset ──────────────────────────────────────────
  | .LD_A_n_ind offset => do
    let v ← cpu.bus.readByte (0xFF00 + offset.toUInt16)
    return (cy, { cpu with registers := { cpu.registers with a := v } })
  | .LD_n_A_ind offset => do
    cpu.bus.writeByte (0xFF00 + offset.toUInt16) cpu.registers.a
    return (cy, cpu)
  | .LD_A_C_ind => do
    let v ← cpu.bus.readByte (0xFF00 + cpu.registers.c.toUInt16)
    return (cy, { cpu with registers := { cpu.registers with a := v } })
  | .LD_C_A_ind => do
    cpu.bus.writeByte (0xFF00 + cpu.registers.c.toUInt16) cpu.registers.a
    return (cy, cpu)
  -- ── 8-bit loads: HL post-increment / post-decrement ──────────────────────
  | .LD_A_HLi => do
    let hl := cpu.registers.hl
    let v  ← cpu.bus.readByte hl
    let regs := (cpu.registers.setHl (hl + 1))
    return (cy, { cpu with registers := { regs with a := v } })
  | .LD_A_HLd => do
    let hl := cpu.registers.hl
    let v  ← cpu.bus.readByte hl
    let regs := (cpu.registers.setHl (hl - 1))
    return (cy, { cpu with registers := { regs with a := v } })
  | .LD_HLi_A => do
    let hl := cpu.registers.hl
    cpu.bus.writeByte hl cpu.registers.a
    return (cy, { cpu with registers := cpu.registers.setHl (hl + 1) })
  | .LD_HLd_A => do
    let hl := cpu.registers.hl
    cpu.bus.writeByte hl cpu.registers.a
    return (cy, { cpu with registers := cpu.registers.setHl (hl - 1) })
  -- ── 16-bit loads ──────────────────────────────────────────────────────────
  | .LD_rr_nn dst nn =>
    return (cy, cpu.writeReg16 dst nn)
  | .LD_SP_HL =>
    return (cy, { cpu with sp := cpu.registers.hl })
  | .LD_HL_SP_n offset =>
    let spv := cpu.sp.toNat
    let ov  := offset.toNat
    let h    := (spv &&& 0xF) + (ov &&& 0xF) > 0xF
    let c    := (spv &&& 0xFF) + (ov &&& 0xFF) > 0xFF
    let regs := cpu.registers.setFlags false false h c
    let newHl := cpu.sp + signExt8to16 offset
    return (cy, { cpu with registers := regs.setHl newHl })
  | .LD_nn_SP addr => do
    cpu.bus.writeWord addr cpu.sp
    return (cy, cpu)
  -- ── Stack: PUSH / POP ─────────────────────────────────────────────────────
  | .PUSH src => do
    let cpu' ← push cpu (cpu.readReg16 src)
    return (cy, cpu')
  | .POP dst => do
    let (v, cpu') ← pop cpu
    return (cy, cpu'.writeReg16 dst v)
  -- ── 8-bit ALU: ADD ────────────────────────────────────────────────────────
  | .ADD_r src =>
    return (cy, { cpu with registers := aluAdd cpu.registers cpu.registers.a (cpu.readReg8 src) })
  | .ADD_n imm =>
    return (cy, { cpu with registers := aluAdd cpu.registers cpu.registers.a imm })
  | .ADD_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, { cpu with registers := aluAdd cpu.registers cpu.registers.a v })
  -- ── 8-bit ALU: ADC ────────────────────────────────────────────────────────
  | .ADC_r src =>
    return (cy, { cpu with registers := aluAdc cpu.registers cpu.registers.a (cpu.readReg8 src) })
  | .ADC_n imm =>
    return (cy, { cpu with registers := aluAdc cpu.registers cpu.registers.a imm })
  | .ADC_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, { cpu with registers := aluAdc cpu.registers cpu.registers.a v })
  -- ── 8-bit ALU: SUB ────────────────────────────────────────────────────────
  | .SUB_r src =>
    return (cy, { cpu with registers := aluSub cpu.registers cpu.registers.a (cpu.readReg8 src) })
  | .SUB_n imm =>
    return (cy, { cpu with registers := aluSub cpu.registers cpu.registers.a imm })
  | .SUB_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, { cpu with registers := aluSub cpu.registers cpu.registers.a v })
  -- ── 8-bit ALU: SBC ────────────────────────────────────────────────────────
  | .SBC_r src =>
    return (cy, { cpu with registers := aluSbc cpu.registers cpu.registers.a (cpu.readReg8 src) })
  | .SBC_n imm =>
    return (cy, { cpu with registers := aluSbc cpu.registers cpu.registers.a imm })
  | .SBC_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, { cpu with registers := aluSbc cpu.registers cpu.registers.a v })
  -- ── 8-bit ALU: AND ────────────────────────────────────────────────────────
  | .AND_r src =>
    return (cy, { cpu with registers := aluAnd cpu.registers cpu.registers.a (cpu.readReg8 src) })
  | .AND_n imm =>
    return (cy, { cpu with registers := aluAnd cpu.registers cpu.registers.a imm })
  | .AND_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, { cpu with registers := aluAnd cpu.registers cpu.registers.a v })
  -- ── 8-bit ALU: OR ─────────────────────────────────────────────────────────
  | .OR_r src =>
    return (cy, { cpu with registers := aluOr cpu.registers cpu.registers.a (cpu.readReg8 src) })
  | .OR_n imm =>
    return (cy, { cpu with registers := aluOr cpu.registers cpu.registers.a imm })
  | .OR_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, { cpu with registers := aluOr cpu.registers cpu.registers.a v })
  -- ── 8-bit ALU: XOR ────────────────────────────────────────────────────────
  | .XOR_r src =>
    return (cy, { cpu with registers := aluXor cpu.registers cpu.registers.a (cpu.readReg8 src) })
  | .XOR_n imm =>
    return (cy, { cpu with registers := aluXor cpu.registers cpu.registers.a imm })
  | .XOR_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, { cpu with registers := aluXor cpu.registers cpu.registers.a v })
  -- ── 8-bit ALU: CP ─────────────────────────────────────────────────────────
  | .CP_r src =>
    return (cy, { cpu with registers := aluCp cpu.registers cpu.registers.a (cpu.readReg8 src) })
  | .CP_n imm =>
    return (cy, { cpu with registers := aluCp cpu.registers cpu.registers.a imm })
  | .CP_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    return (cy, { cpu with registers := aluCp cpu.registers cpu.registers.a v })
  -- ── 8-bit ALU: INC / DEC register ────────────────────────────────────────
  | .INC_r dst =>
    let (res, regs) := aluInc cpu.registers (cpu.readReg8 dst)
    return (cy, { cpu with registers := regs }.writeReg8 dst res)
  | .DEC_r dst =>
    let (res, regs) := aluDec cpu.registers (cpu.readReg8 dst)
    return (cy, { cpu with registers := regs }.writeReg8 dst res)
  -- ── 8-bit ALU: INC / DEC (HL) ────────────────────────────────────────────
  | .INC_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := aluInc cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  | .DEC_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := aluDec cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  -- ── 16-bit ALU: ADD HL, rr ───────────────────────────────────────────────
  | .ADD_HL_rr src =>
    let hl := cpu.registers.hl.toNat
    let rr := (cpu.readReg16 src).toNat
    let r  := hl + rr
    let h  := (hl &&& 0x0FFF) + (rr &&& 0x0FFF) > 0x0FFF
    let c  := r > 0xFFFF
    -- Z flag is unchanged
    let regs := cpu.registers.setFlags cpu.registers.zeroFlag false h c
    return (cy, { cpu with registers := regs.setHl r.toUInt16 })
  -- ── 16-bit ALU: ADD SP, n ────────────────────────────────────────────────
  | .ADD_SP_n offset =>
    let spv := cpu.sp.toNat
    let ov  := offset.toNat
    let h    := (spv &&& 0xF) + (ov &&& 0xF) > 0xF
    let c    := (spv &&& 0xFF) + (ov &&& 0xFF) > 0xFF
    let regs := cpu.registers.setFlags false false h c
    let newSp := cpu.sp + signExt8to16 offset
    return (cy, { cpu with registers := regs, sp := newSp })
  -- ── 16-bit ALU: INC / DEC rr ─────────────────────────────────────────────
  | .INC_rr dst =>
    return (cy, cpu.writeReg16 dst (cpu.readReg16 dst + 1))
  | .DEC_rr dst =>
    return (cy, cpu.writeReg16 dst (cpu.readReg16 dst - 1))
  -- ── Accumulator rotates (no Z flag) ──────────────────────────────────────
  | .RLCA =>
    let a := cpu.registers.a
    let carry := (a &&& 0x80) != 0
    let res := (a <<< 1) ||| (if carry then 1 else 0)
    let regs := cpu.registers.setFlags false false false carry
    return (cy, { cpu with registers := { regs with a := res } })
  | .RLA =>
    let a := cpu.registers.a
    let oldC : UInt8 := if cpu.registers.carryFlag then 1 else 0
    let newC := (a &&& 0x80) != 0
    let res := (a <<< 1) ||| oldC
    let regs := cpu.registers.setFlags false false false newC
    return (cy, { cpu with registers := { regs with a := res } })
  | .RRCA =>
    let a := cpu.registers.a
    let carry := (a &&& 0x01) != 0
    let res := (a >>> 1) ||| (if carry then 0x80 else 0)
    let regs := cpu.registers.setFlags false false false carry
    return (cy, { cpu with registers := { regs with a := res } })
  | .RRA =>
    let a := cpu.registers.a
    let oldC : UInt8 := if cpu.registers.carryFlag then 0x80 else 0
    let newC := (a &&& 0x01) != 0
    let res := (a >>> 1) ||| oldC
    let regs := cpu.registers.setFlags false false false newC
    return (cy, { cpu with registers := { regs with a := res } })
  -- ── CB-prefix: rotate/shift on register ──────────────────────────────────
  | .RLC_r r =>
    let (res, regs) := rlc8 cpu.registers (cpu.readReg8 r)
    return (cy, { cpu with registers := regs }.writeReg8 r res)
  | .RRC_r r =>
    let (res, regs) := rrc8 cpu.registers (cpu.readReg8 r)
    return (cy, { cpu with registers := regs }.writeReg8 r res)
  | .RL_r r =>
    let (res, regs) := rl8 cpu.registers (cpu.readReg8 r)
    return (cy, { cpu with registers := regs }.writeReg8 r res)
  | .RR_r r =>
    let (res, regs) := rr8 cpu.registers (cpu.readReg8 r)
    return (cy, { cpu with registers := regs }.writeReg8 r res)
  | .SLA_r r =>
    let (res, regs) := sla8 cpu.registers (cpu.readReg8 r)
    return (cy, { cpu with registers := regs }.writeReg8 r res)
  | .SRA_r r =>
    let (res, regs) := sra8 cpu.registers (cpu.readReg8 r)
    return (cy, { cpu with registers := regs }.writeReg8 r res)
  | .SRL_r r =>
    let (res, regs) := srl8 cpu.registers (cpu.readReg8 r)
    return (cy, { cpu with registers := regs }.writeReg8 r res)
  | .SWAP_r r =>
    let (res, regs) := swap8 cpu.registers (cpu.readReg8 r)
    return (cy, { cpu with registers := regs }.writeReg8 r res)
  -- ── CB-prefix: rotate/shift on (HL) ──────────────────────────────────────
  | .RLC_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := rlc8 cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  | .RRC_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := rrc8 cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  | .RL_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := rl8 cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  | .RR_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := rr8 cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  | .SLA_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := sla8 cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  | .SRA_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := sra8 cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  | .SRL_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := srl8 cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  | .SWAP_HL => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let (res, regs) := swap8 cpu.registers v
    cpu.bus.writeByte cpu.registers.hl res
    return (cy, { cpu with registers := regs })
  -- ── CB-prefix: BIT / RES / SET on register ───────────────────────────────
  | .BIT_r bit r =>
    let v  := cpu.readReg8 r
    let z  := (v &&& (1 <<< bit.toUInt8)) == 0
    let regs := cpu.registers.setFlags z false true cpu.registers.carryFlag
    return (cy, { cpu with registers := regs })
  | .RES_r bit r =>
    let v   := cpu.readReg8 r
    let res := v &&& ~~~(1 <<< bit.toUInt8)
    return (cy, cpu.writeReg8 r res)
  | .SET_r bit r =>
    let v   := cpu.readReg8 r
    let res := v ||| (1 <<< bit.toUInt8)
    return (cy, cpu.writeReg8 r res)
  -- ── CB-prefix: BIT / RES / SET on (HL) ───────────────────────────────────
  | .BIT_HL bit => do
    let v ← cpu.bus.readByte cpu.registers.hl
    let z := (v &&& (1 <<< bit.toUInt8)) == 0
    let regs := cpu.registers.setFlags z false true cpu.registers.carryFlag
    return (cy, { cpu with registers := regs })
  | .RES_HL bit => do
    let v ← cpu.bus.readByte cpu.registers.hl
    cpu.bus.writeByte cpu.registers.hl (v &&& ~~~(1 <<< bit.toUInt8))
    return (cy, cpu)
  | .SET_HL bit => do
    let v ← cpu.bus.readByte cpu.registers.hl
    cpu.bus.writeByte cpu.registers.hl (v ||| (1 <<< bit.toUInt8))
    return (cy, cpu)
  -- ── Misc ──────────────────────────────────────────────────────────────────
  | .DAA =>
    let nFlag := cpu.registers.subtractFlag
    let cFlag := cpu.registers.carryFlag
    let hFlag := cpu.registers.halfCarryFlag
    let a := cpu.registers.a
    let (a', c') :=
      if !nFlag then
        -- After an addition
        let (a1, c1) := if cFlag || a > 0x99 then (a + 0x60, true) else (a, false)
        let a2 := if hFlag || (a1 &&& 0x0F) > 0x09 then a1 + 0x06 else a1
        (a2, c1 || cFlag)
      else
        -- After a subtraction
        let a1 := if cFlag then a - 0x60 else a
        let a2 := if hFlag then a1 - 0x06 else a1
        (a2, cFlag)
    let regs := cpu.registers.setFlags (a' == 0) nFlag false c'
    return (cy, { cpu with registers := { regs with a := a' } })
  | .CPL =>
    let a    := cpu.registers.a ^^^ 0xFF
    let regs := cpu.registers.setFlags cpu.registers.zeroFlag true true cpu.registers.carryFlag
    return (cy, { cpu with registers := { regs with a } })
  | .SCF =>
    let regs := cpu.registers.setFlags cpu.registers.zeroFlag false false true
    return (cy, { cpu with registers := regs })
  | .CCF =>
    let regs := cpu.registers.setFlags cpu.registers.zeroFlag false false (!cpu.registers.carryFlag)
    return (cy, { cpu with registers := regs })
  -- ── Jumps / Branches ──────────────────────────────────────────────────────
  | .JR cond offset =>
    if cpu.testCondition cond then
      -- Use UInt16 wrapping arithmetic: sign-extend offset to 16 bits and add.
      return (cy, { cpu with pc := cpu.pc + signExt8to16 offset })
    else
      return (cy2, cpu)
  | .JP cond addr =>
    if cpu.testCondition cond then
      return (cy, { cpu with pc := addr })
    else
      return (cy2, cpu)
  | .JP_HL =>
    return (cy, { cpu with pc := cpu.registers.hl })
  | .CALL cond addr =>
    if cpu.testCondition cond then
      let cpu' ← push cpu cpu.pc
      return (cy, { cpu' with pc := addr })
    else
      return (cy2, cpu)
  | .RET cond =>
    if cpu.testCondition cond then
      let (addr, cpu') ← pop cpu
      return (cy, { cpu' with pc := addr })
    else
      return (cy2, cpu)
  | .RETI => do
    let (addr, cpu') ← pop cpu
    return (cy, { cpu' with pc := addr, ime := true })
  | .RST vec => do
    let cpu' ← push cpu cpu.pc
    return (cy, { cpu' with pc := vec.toUInt16 })

-- Execute one instruction. Returns m-cycles consumed.
def runInstruction (cpuRef : IO.Ref Cpu) : IO Nat := do
  let cpu ← cpuRef.get
  -- Service any pending interrupt first
  let (intCycles, cpu') ← handleInterrupts cpu
  if intCycles > 0 then
    cpuRef.set cpu'
    Debug.log3 s!"[CPU] INT  pc=0x{Debug.hexWord cpu.pc}  cycles={intCycles}"
    return intCycles
  if cpu'.halted then
    Debug.log3 s!"[CPU] HALT pc=0x{Debug.hexWord cpu'.pc}"
    return 1   -- HALT: burn 1 m-cycle
  -- Apply delayed EI effect (ime enabled after the instruction following EI)
  let cpu'' :=
    if cpu'.imeNext then { cpu' with ime := true, imeNext := false }
    else cpu'
  -- Fetch & decode
  let decoded ← fetchAndDecode cpu''.bus cpu''.pc
  -- Trap: detect first entry into the bitreverse data table region (0x0010-0x001F from outside).
  if cpu''.pc >= 0x0010 && cpu''.pc < 0x0020 &&
     !(cpu''.prevPc >= 0x0010 && cpu''.prevPc < 0x0020) then do
    IO.eprintln s!"[ENTER_DATA] PC=0x{Debug.hexWord cpu''.pc}  from=0x{Debug.hexWord cpu''.prevPc}  prevPC2=0x{Debug.hexWord cpu''.prevPc2}  \
SP=0x{Debug.hexWord cpu''.sp}  HL=0x{Debug.hexWord cpu''.registers.hl}  \
BC=0x{Debug.hexWord cpu''.registers.bc}  DE=0x{Debug.hexWord cpu''.registers.de}  \
AF=0x{Debug.hexWord cpu''.registers.af}"
  -- Trap: catch when PC is 0x3c94 (F8 = LD HL SP+n byte, possible misaligned decode).
  if cpu''.pc == 0x3c94 then do
    IO.eprintln s!"[AT3C94] from=0x{Debug.hexWord cpu''.prevPc}  prevPC2=0x{Debug.hexWord cpu''.prevPc2}  \
SP=0x{Debug.hexWord cpu''.sp}  BC=0x{Debug.hexWord cpu''.registers.bc}  \
HL=0x{Debug.hexWord cpu''.registers.hl}  AF=0x{Debug.hexWord cpu''.registers.af}"
  -- Trap: dump stack at RETI (0x007D) only when near crash point (SP >= 0xe440).
  if cpu''.pc == 0x007D && cpu''.sp >= 0xe440 then do
    let retAddr ← cpu''.bus.readWord cpu''.sp
    let words ← (List.range 8).mapM (fun i => cpu''.bus.readWord (cpu''.sp + (2 * i).toUInt16))
    let stackStr := String.intercalate " " (words.map (fun w => "0x" ++ Debug.hexWord w))
    IO.eprintln s!"[RETI007D] SP=0x{Debug.hexWord cpu''.sp}  [SP]=0x{Debug.hexWord retAddr}  stack: {stackStr}"
  -- Trap: detect first entry into the 0x3c80-0x3cbc region from outside.
  if cpu''.pc >= 0x3c80 && cpu''.pc <= 0x3cbc &&
     !(cpu''.prevPc >= 0x3c80 && cpu''.prevPc <= 0x3cbc) then do
    let sp := cpu''.sp
    let retAddr ← cpu''.bus.readWord sp
    IO.eprintln s!"[ENTER3C80] PC=0x{Debug.hexWord cpu''.pc}  from=0x{Debug.hexWord cpu''.prevPc}  \
SP=0x{Debug.hexWord sp}  [SP]=0x{Debug.hexWord retAddr}  \
BC=0x{Debug.hexWord cpu''.registers.bc}  HL=0x{Debug.hexWord cpu''.registers.hl}  \
AF=0x{Debug.hexWord cpu''.registers.af}"
  -- Trap: log when PC enters 0x3cb6 (the copy loop) — reveal the return address on stack.
  if cpu''.pc == 0x3cb6 then do
    let sp := cpu''.sp
    let retAddr ← cpu''.bus.readWord sp   -- what CALL/trampoline pushed as return address
    IO.eprintln s!"[ENTER3CB6] from=0x{Debug.hexWord cpu''.prevPc}  SP=0x{Debug.hexWord sp}  \
retAddr=[SP]=0x{Debug.hexWord retAddr}  \
BC=0x{Debug.hexWord cpu''.registers.bc}  HL=0x{Debug.hexWord cpu''.registers.hl}"
  -- Trap: log every RST 0x38 at level 1 with first occurrence getting a full stack dump.
  if let .RST 0x38 := decoded.inst then do
    let sp := cpu''.sp
    IO.eprintln s!"[RST38] PC=0x{Debug.hexWord cpu''.pc}  prevPC=0x{Debug.hexWord cpu''.prevPc}  prevPC2=0x{Debug.hexWord cpu''.prevPc2}  \
SP=0x{Debug.hexWord sp}  HL=0x{Debug.hexWord cpu''.registers.hl}  \
BC=0x{Debug.hexWord cpu''.registers.bc}  DE=0x{Debug.hexWord cpu''.registers.de}  \
AF=0x{Debug.hexWord cpu''.registers.af}"
    -- On the FIRST RST38 (sp is far from 0xFFFE), dump the top 12 stack words to trace the call chain.
    if sp < 0xFF00 then do
      let words ← (List.range 12).mapM (fun i => cpu''.bus.readWord (sp + (2 * i).toUInt16))
      IO.eprintln s!"[RST38] Stack@SP: {words.map (fun w => "0x" ++ Debug.hexWord w)}"
      let d ← (List.range 32).mapM (fun i => cpu''.bus.readByte (0xC4DA + i).toUInt16)
      IO.eprintln s!"[RST38] DispTbl@0xC4DA: {Debug.hexDump (ByteArray.mk d.toArray) 0 32}"
  -- Advance PC past this instruction; record prevPc for diagnostics
  let cpu''' := { cpu'' with pc := cpu''.pc + decoded.length.toUInt16
                           , prevPc  := cpu''.pc
                           , prevPc2 := cpu''.prevPc }
  -- Execute
  let (cycles, cpu4) ← executeInstruction cpu''' decoded
  -- Trap: dump stack at dispatch loop exit (0x0079) when near crash point.
  if cpu''.pc == 0x0079 && cpu''.sp >= 0xe43c then do
    let words ← (List.range 8).mapM (fun i => cpu''.bus.readWord (cpu''.sp + (2 * i).toUInt16))
    let stackStr := String.intercalate " " (words.map (fun w => "0x" ++ Debug.hexWord w))
    IO.eprintln s!"[EXIT_DISPATCH] SP=0x{Debug.hexWord cpu''.sp}  stack: {stackStr}"
  -- Trap: catch ODD SP transitions near crash range (SP even→odd means +1/-1 SP change).
  -- Only watch once SP is near the crash range (>= 0xe430).
  if cpu''.sp >= 0xe430 && cpu4.sp % 2 == 1 && cpu''.sp % 2 == 0 then do
    let words ← (List.range 8).mapM (fun i => cpu4.bus.readWord (cpu4.sp + (2 * i).toUInt16))
    let stackStr := String.intercalate " " (words.map (fun w => "0x" ++ Debug.hexWord w))
    IO.eprintln s!"[ODD_SP] at PC=0x{Debug.hexWord cpu''.pc}  prevPC=0x{Debug.hexWord cpu''.prevPc}  op={reprStr decoded.inst}  \
SP=0x{Debug.hexWord cpu''.sp}→0x{Debug.hexWord cpu4.sp}  stack: {stackStr}"
  -- Trap: catch the instruction that makes PC become 0x3c94 (suspicious mid-instr address).
  if cpu4.pc == 0x3c94 then do
    IO.eprintln s!"[PRODUCE3C94] instr at 0x{Debug.hexWord cpu''.pc}  op={reprStr decoded.inst}  \
cpu4.pc=0x{Debug.hexWord cpu4.pc}  SP=0x{Debug.hexWord cpu4.sp}  \
BC=0x{Debug.hexWord cpu4.registers.bc}  HL=0x{Debug.hexWord cpu4.registers.hl}"
  cpuRef.set cpu4
  Debug.log3 s!"[CPU] pc=0x{Debug.hexWord cpu''.pc}  op={reprStr decoded.inst}  \
cy={cycles}  AF={Debug.hexWord cpu4.registers.af}  \
BC={Debug.hexWord cpu4.registers.bc}  \
DE={Debug.hexWord cpu4.registers.de}  \
HL={Debug.hexWord cpu4.registers.hl}  \
SP=0x{Debug.hexWord cpu4.sp}"
  return cycles

end Cpu

end LeanBoy.Cpu
