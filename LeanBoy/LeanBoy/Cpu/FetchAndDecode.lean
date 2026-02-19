import LeanBoy.Cpu.Instruction
import LeanBoy.Bus
/-!
# Instruction Fetch and Decode

Reads the next instruction from the bus at the given program counter and
decodes it into an `Instruction` plus its byte length and m-cycle count.

The full 256-opcode match is split into four private helpers (one per
64-opcode quadrant) to keep the generated C nesting depth under clang's
default limit of 256.
-/

namespace LeanBoy.Cpu

open LeanBoy

-- Fetch one byte at the given address.
private def fetch (bus : Bus) (pc : UInt16) : IO UInt8 :=
  bus.readByte pc

-- A decoded instruction with its metadata.
structure DecodedInst where
  inst           : Instruction
  length         : Nat    -- number of bytes consumed (1–3)
  cyclesTaken    : Nat    -- m-cycles if branch taken / always
  cyclesNotTaken : Nat    -- m-cycles if branch not taken
  deriving Repr

private def mk1 (inst : Instruction) (c : Nat) : DecodedInst :=
  { inst, length := 1, cyclesTaken := c, cyclesNotTaken := c }

private def mk2 (inst : Instruction) (c : Nat) : DecodedInst :=
  { inst, length := 2, cyclesTaken := c, cyclesNotTaken := c }

private def mk3 (inst : Instruction) (c : Nat) : DecodedInst :=
  { inst, length := 3, cyclesTaken := c, cyclesNotTaken := c }

private def mkBranch1 (inst : Instruction) (taken notTaken : Nat) : DecodedInst :=
  { inst, length := 1, cyclesTaken := taken, cyclesNotTaken := notTaken }

private def mkBranch2 (inst : Instruction) (taken notTaken : Nat) : DecodedInst :=
  { inst, length := 2, cyclesTaken := taken, cyclesNotTaken := notTaken }

private def mkBranch3 (inst : Instruction) (taken notTaken : Nat) : DecodedInst :=
  { inst, length := 3, cyclesTaken := taken, cyclesNotTaken := notTaken }

/-- Decode a CB-prefixed instruction. `pc` already points past the 0xCB byte. -/
def fetchAndDecodeCB (bus : Bus) (pc : UInt16) : IO DecodedInst := do
  let opcode ← fetch bus pc
  let r8 : Nat → Reg8
    | 0 => .B | 1 => .C | 2 => .D | 3 => .E
    | 4 => .H | 5 => .L | 7 => .A | _ => .A
  let col   := (opcode &&& 0x07).toNat
  let row   := (opcode >>> 3).toNat
  let isHL  := col == 6
  let group := if row < 8 then 0 else (row - 8) / 8
  let cycles : Nat := if isHL then (if group == 1 then 3 else 4) else 2
  let inst : Instruction :=
    if row < 8 then
      match row with
      | 0 => if isHL then .RLC_HL else .RLC_r (r8 col)
      | 1 => if isHL then .RRC_HL else .RRC_r (r8 col)
      | 2 => if isHL then .RL_HL  else .RL_r  (r8 col)
      | 3 => if isHL then .RR_HL  else .RR_r  (r8 col)
      | 4 => if isHL then .SLA_HL else .SLA_r (r8 col)
      | 5 => if isHL then .SRA_HL else .SRA_r (r8 col)
      | 6 => if isHL then .SWAP_HL else .SWAP_r (r8 col)
      | _ => if isHL then .SRL_HL else .SRL_r (r8 col)
    else
      let bit := row - 8
      match bit / 8 with
      | 0 => if isHL then .BIT_HL (bit % 8) else .BIT_r (bit % 8) (r8 col)
      | 1 => if isHL then .RES_HL (bit % 8) else .RES_r (bit % 8) (r8 col)
      | _ => if isHL then .SET_HL (bit % 8) else .SET_r (bit % 8) (r8 col)
  return { inst, length := 2, cyclesTaken := cycles, cyclesNotTaken := cycles }

-- Decode the 0x40-0x7F LD block algorithmically (avoids 64 match arms).
-- Register encoding: 0=B 1=C 2=D 3=E 4=H 5=L 6=(HL) 7=A
private def decodeLoadBlock (opcode : UInt8) : DecodedInst :=
  let r8 : Nat → Reg8
    | 0 => .B | 1 => .C | 2 => .D | 3 => .E
    | 4 => .H | 5 => .L | 7 => .A | _ => .A
  let src := (opcode &&& 0x07).toNat
  let dst := ((opcode - 0x40) >>> 3).toNat
  if dst == 6 && src == 6 then mk1 .HALT 1        -- 0x76
  else if dst == 6         then mk1 (.LD_HL_r (r8 src)) 2
  else if src == 6         then mk1 (.LD_r_HL (r8 dst)) 2
  else                          mk1 (.LD_rr   (r8 dst) (r8 src)) 1

-- Opcodes 0x00–0x3F
private def decodeOpLo (bus : Bus) (pc : UInt16) (opcode : UInt8) : IO DecodedInst := do
  let imm8  := fun () => fetch bus (pc + 1)
  let imm16 := fun () => do
    let lo ← fetch bus (pc + 1); let hi ← fetch bus (pc + 2)
    return (hi.toUInt16 <<< 8) ||| lo.toUInt16
  match opcode with
  | 0x00 => return mk1 .NOP 1
  | 0x01 => do let nn ← imm16 (); return mk3 (.LD_rr_nn .BC nn) 3
  | 0x02 => return mk1 .LD_BC_A 2
  | 0x03 => return mk1 (.INC_rr .BC) 2
  | 0x04 => return mk1 (.INC_r .B) 1
  | 0x05 => return mk1 (.DEC_r .B) 1
  | 0x06 => do let n ← imm8 (); return mk2 (.LD_r_n .B n) 2
  | 0x07 => return mk1 .RLCA 1
  | 0x08 => do let nn ← imm16 (); return mk3 (.LD_nn_SP nn) 5
  | 0x09 => return mk1 (.ADD_HL_rr .BC) 2
  | 0x0A => return mk1 .LD_A_BC 2
  | 0x0B => return mk1 (.DEC_rr .BC) 2
  | 0x0C => return mk1 (.INC_r .C) 1
  | 0x0D => return mk1 (.DEC_r .C) 1
  | 0x0E => do let n ← imm8 (); return mk2 (.LD_r_n .C n) 2
  | 0x0F => return mk1 .RRCA 1
  | 0x10 => return mk2 .STOP 1
  | 0x11 => do let nn ← imm16 (); return mk3 (.LD_rr_nn .DE nn) 3
  | 0x12 => return mk1 .LD_DE_A 2
  | 0x13 => return mk1 (.INC_rr .DE) 2
  | 0x14 => return mk1 (.INC_r .D) 1
  | 0x15 => return mk1 (.DEC_r .D) 1
  | 0x16 => do let n ← imm8 (); return mk2 (.LD_r_n .D n) 2
  | 0x17 => return mk1 .RLA 1
  | 0x18 => do let n ← imm8 (); return mk2 (.JR .Always n) 3
  | 0x19 => return mk1 (.ADD_HL_rr .DE) 2
  | 0x1A => return mk1 .LD_A_DE 2
  | 0x1B => return mk1 (.DEC_rr .DE) 2
  | 0x1C => return mk1 (.INC_r .E) 1
  | 0x1D => return mk1 (.DEC_r .E) 1
  | 0x1E => do let n ← imm8 (); return mk2 (.LD_r_n .E n) 2
  | 0x1F => return mk1 .RRA 1
  | 0x20 => do let n ← imm8 (); return mkBranch2 (.JR .NZ n) 3 2
  | 0x21 => do let nn ← imm16 (); return mk3 (.LD_rr_nn .HL nn) 3
  | 0x22 => return mk1 .LD_HLi_A 2
  | 0x23 => return mk1 (.INC_rr .HL) 2
  | 0x24 => return mk1 (.INC_r .H) 1
  | 0x25 => return mk1 (.DEC_r .H) 1
  | 0x26 => do let n ← imm8 (); return mk2 (.LD_r_n .H n) 2
  | 0x27 => return mk1 .DAA 1
  | 0x28 => do let n ← imm8 (); return mkBranch2 (.JR .Z n) 3 2
  | 0x29 => return mk1 (.ADD_HL_rr .HL) 2
  | 0x2A => return mk1 .LD_A_HLi 2
  | 0x2B => return mk1 (.DEC_rr .HL) 2
  | 0x2C => return mk1 (.INC_r .L) 1
  | 0x2D => return mk1 (.DEC_r .L) 1
  | 0x2E => do let n ← imm8 (); return mk2 (.LD_r_n .L n) 2
  | 0x2F => return mk1 .CPL 1
  | 0x30 => do let n ← imm8 (); return mkBranch2 (.JR .NC n) 3 2
  | 0x31 => do let nn ← imm16 (); return mk3 (.LD_rr_nn .SP nn) 3
  | 0x32 => return mk1 .LD_HLd_A 2
  | 0x33 => return mk1 (.INC_rr .SP) 2
  | 0x34 => return mk1 .INC_HL 3
  | 0x35 => return mk1 .DEC_HL 3
  | 0x36 => do let n ← imm8 (); return mk2 (.LD_HL_n n) 3
  | 0x37 => return mk1 .SCF 1
  | 0x38 => do let n ← imm8 (); return mkBranch2 (.JR .CY n) 3 2
  | 0x39 => return mk1 (.ADD_HL_rr .SP) 2
  | 0x3A => return mk1 .LD_A_HLd 2
  | 0x3B => return mk1 (.DEC_rr .SP) 2
  | 0x3C => return mk1 (.INC_r .A) 1
  | 0x3D => return mk1 (.DEC_r .A) 1
  | 0x3E => do let n ← imm8 (); return mk2 (.LD_r_n .A n) 2
  | 0x3F => return mk1 .CCF 1
  | _    => return mk1 .NOP 1

-- Opcodes 0x80–0xBF  (all 8-bit ALU)
private def decodeOpAlu (opcode : UInt8) : DecodedInst :=
  let r8 : Nat → Reg8
    | 0 => .B | 1 => .C | 2 => .D | 3 => .E
    | 4 => .H | 5 => .L | 7 => .A | _ => .A
  let src   := (opcode &&& 0x07).toNat
  let isHL  := src == 6
  let reg   := r8 src
  match (opcode >>> 3) &&& 0x07 with
  | 0 => if isHL then mk1 .ADD_HL 2 else mk1 (.ADD_r reg) 1
  | 1 => if isHL then mk1 .ADC_HL 2 else mk1 (.ADC_r reg) 1
  | 2 => if isHL then mk1 .SUB_HL 2 else mk1 (.SUB_r reg) 1
  | 3 => if isHL then mk1 .SBC_HL 2 else mk1 (.SBC_r reg) 1
  | 4 => if isHL then mk1 .AND_HL 2 else mk1 (.AND_r reg) 1
  | 5 => if isHL then mk1 .XOR_HL 2 else mk1 (.XOR_r reg) 1
  | 6 => if isHL then mk1 .OR_HL  2 else mk1 (.OR_r  reg) 1
  | _ => if isHL then mk1 .CP_HL  2 else mk1 (.CP_r  reg) 1

-- Opcodes 0xC0–0xFF  (control, PUSH/POP, misc)
private def decodeOpHi (bus : Bus) (pc : UInt16) (opcode : UInt8) : IO DecodedInst := do
  let imm8  := fun () => fetch bus (pc + 1)
  let imm16 := fun () => do
    let lo ← fetch bus (pc + 1); let hi ← fetch bus (pc + 2)
    return (hi.toUInt16 <<< 8) ||| lo.toUInt16
  match opcode with
  | 0xC0 => return mkBranch1 (.RET .NZ) 5 2
  | 0xC1 => return mk1 (.POP .BC) 3
  | 0xC2 => do let nn ← imm16 (); return mkBranch3 (.JP .NZ nn) 4 3
  | 0xC3 => do let nn ← imm16 (); return mk3 (.JP .Always nn) 4
  | 0xC4 => do let nn ← imm16 (); return mkBranch3 (.CALL .NZ nn) 6 3
  | 0xC5 => return mk1 (.PUSH .BC) 4
  | 0xC6 => do let n ← imm8 (); return mk2 (.ADD_n n) 2
  | 0xC7 => return mk1 (.RST 0x00) 4
  | 0xC8 => return mkBranch1 (.RET .Z) 5 2
  | 0xC9 => return mk1 (.RET .Always) 4
  | 0xCA => do let nn ← imm16 (); return mkBranch3 (.JP .Z nn) 4 3
  | 0xCB => fetchAndDecodeCB bus (pc + 1)
  | 0xCC => do let nn ← imm16 (); return mkBranch3 (.CALL .Z nn) 6 3
  | 0xCD => do let nn ← imm16 (); return mk3 (.CALL .Always nn) 6
  | 0xCE => do let n ← imm8 (); return mk2 (.ADC_n n) 2
  | 0xCF => return mk1 (.RST 0x08) 4
  | 0xD0 => return mkBranch1 (.RET .NC) 5 2
  | 0xD1 => return mk1 (.POP .DE) 3
  | 0xD2 => do let nn ← imm16 (); return mkBranch3 (.JP .NC nn) 4 3
  | 0xD3 => return mk1 .NOP 1
  | 0xD4 => do let nn ← imm16 (); return mkBranch3 (.CALL .NC nn) 6 3
  | 0xD5 => return mk1 (.PUSH .DE) 4
  | 0xD6 => do let n ← imm8 (); return mk2 (.SUB_n n) 2
  | 0xD7 => return mk1 (.RST 0x10) 4
  | 0xD8 => return mkBranch1 (.RET .CY) 5 2
  | 0xD9 => return mk1 .RETI 4
  | 0xDA => do let nn ← imm16 (); return mkBranch3 (.JP .CY nn) 4 3
  | 0xDB => return mk1 .NOP 1
  | 0xDC => do let nn ← imm16 (); return mkBranch3 (.CALL .CY nn) 6 3
  | 0xDD => return mk1 .NOP 1
  | 0xDE => do let n ← imm8 (); return mk2 (.SBC_n n) 2
  | 0xDF => return mk1 (.RST 0x18) 4
  | 0xE0 => do let n ← imm8 (); return mk2 (.LD_n_A_ind n) 3
  | 0xE1 => return mk1 (.POP .HL) 3
  | 0xE2 => return mk1 .LD_C_A_ind 2
  | 0xE3 => return mk1 .NOP 1
  | 0xE4 => return mk1 .NOP 1
  | 0xE5 => return mk1 (.PUSH .HL) 4
  | 0xE6 => do let n ← imm8 (); return mk2 (.AND_n n) 2
  | 0xE7 => return mk1 (.RST 0x20) 4
  | 0xE8 => do let n ← imm8 (); return mk2 (.ADD_SP_n n) 4
  | 0xE9 => return mk1 .JP_HL 1
  | 0xEA => do let nn ← imm16 (); return mk3 (.LD_nn_A nn) 4
  | 0xEB => return mk1 .NOP 1
  | 0xEC => return mk1 .NOP 1
  | 0xED => return mk1 .NOP 1
  | 0xEE => do let n ← imm8 (); return mk2 (.XOR_n n) 2
  | 0xEF => return mk1 (.RST 0x28) 4
  | 0xF0 => do let n ← imm8 (); return mk2 (.LD_A_n_ind n) 3
  | 0xF1 => return mk1 (.POP .AF) 3
  | 0xF2 => return mk1 .LD_A_C_ind 2
  | 0xF3 => return mk1 .DI 1
  | 0xF4 => return mk1 .NOP 1
  | 0xF5 => return mk1 (.PUSH .AF) 4
  | 0xF6 => do let n ← imm8 (); return mk2 (.OR_n n) 2
  | 0xF7 => return mk1 (.RST 0x30) 4
  | 0xF8 => do let n ← imm8 (); return mk2 (.LD_HL_SP_n n) 3
  | 0xF9 => return mk1 .LD_SP_HL 2
  | 0xFA => do let nn ← imm16 (); return mk3 (.LD_A_nn nn) 4
  | 0xFB => return mk1 .EI 1
  | 0xFC => return mk1 .NOP 1
  | 0xFD => return mk1 .NOP 1
  | 0xFE => do let n ← imm8 (); return mk2 (.CP_n n) 2
  | 0xFF => return mk1 (.RST 0x38) 4
  | _    => return mk1 .NOP 1

/-- Fetch and decode the instruction at address `pc`. -/
def fetchAndDecode (bus : Bus) (pc : UInt16) : IO DecodedInst := do
  let opcode ← fetch bus pc
  -- Dispatch by top 2 bits to keep each quadrant under 64 cases
  if opcode < 0x40 then decodeOpLo bus pc opcode
  else if opcode < 0x80 then return (decodeLoadBlock opcode)
  else if opcode < 0xC0 then return (decodeOpAlu opcode)
  else decodeOpHi bus pc opcode

end LeanBoy.Cpu
