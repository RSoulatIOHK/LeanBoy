/-!
# CPU Instructions

Type definitions for the Sharp SM83 (Game Boy) instruction set.
All ~500 instructions (256 main + 256 CB-prefix) are represented here.
-/

namespace LeanBoy.Cpu

-- The eight 8-bit registers.
inductive Reg8
  | A | B | C | D | E | H | L
  deriving Repr, BEq

-- The four 16-bit register pairs (AF used mainly for push/pop).
inductive Reg16
  | AF | BC | DE | HL | SP
  deriving Repr, BEq

-- Condition codes for conditional jumps/calls/returns.
inductive Condition
  | Always | Z | NZ | CY | NC
  deriving Repr, BEq

-- The full instruction set.
inductive Instruction
  -- Load/Store (8-bit)
  | LD_rr   (dst src : Reg8)
  | LD_r_n  (dst : Reg8) (imm : UInt8)
  | LD_r_HL (dst : Reg8)
  | LD_HL_r (src : Reg8)
  | LD_HL_n (imm : UInt8)
  | LD_A_BC | LD_A_DE
  | LD_BC_A | LD_DE_A
  | LD_A_nn (addr : UInt16)
  | LD_nn_A (addr : UInt16)
  | LD_A_C_ind   -- LD A, (0xFF00+C)
  | LD_C_A_ind   -- LD (0xFF00+C), A
  | LD_A_n_ind  (offset : UInt8)   -- LD A, (0xFF00+n)
  | LD_n_A_ind  (offset : UInt8)   -- LD (0xFF00+n), A
  | LD_A_HLi | LD_A_HLd           -- LD A, (HL+) / LD A, (HL-)
  | LD_HLi_A | LD_HLd_A           -- LD (HL+), A / LD (HL-), A
  -- Load/Store (16-bit)
  | LD_rr_nn (dst : Reg16) (imm : UInt16)
  | LD_SP_HL
  | LD_HL_SP_n (offset : UInt8)   -- LD HL, SP+n
  | LD_nn_SP   (addr : UInt16)    -- LD (nn), SP
  -- Stack
  | PUSH (src : Reg16)
  | POP  (dst : Reg16)
  -- Arithmetic (8-bit)
  | ADD_r  (src : Reg8)
  | ADD_n  (imm : UInt8)
  | ADD_HL
  | ADC_r  (src : Reg8)
  | ADC_n  (imm : UInt8)
  | ADC_HL
  | SUB_r  (src : Reg8)
  | SUB_n  (imm : UInt8)
  | SUB_HL
  | SBC_r  (src : Reg8)
  | SBC_n  (imm : UInt8)
  | SBC_HL
  | AND_r  (src : Reg8)
  | AND_n  (imm : UInt8)
  | AND_HL
  | OR_r   (src : Reg8)
  | OR_n   (imm : UInt8)
  | OR_HL
  | XOR_r  (src : Reg8)
  | XOR_n  (imm : UInt8)
  | XOR_HL
  | CP_r   (src : Reg8)
  | CP_n   (imm : UInt8)
  | CP_HL
  | INC_r  (dst : Reg8)
  | INC_HL
  | DEC_r  (dst : Reg8)
  | DEC_HL
  -- Arithmetic (16-bit)
  | ADD_HL_rr (src : Reg16)
  | ADD_SP_n  (offset : UInt8)
  | INC_rr    (dst : Reg16)
  | DEC_rr    (dst : Reg16)
  -- Rotate/Shift (A-register fast versions)
  | RLCA | RLA | RRCA | RRA
  -- CB-prefix rotate/shift/bit (apply to reg or (HL))
  | RLC_r (r : Reg8) | RLC_HL
  | RRC_r (r : Reg8) | RRC_HL
  | RL_r  (r : Reg8) | RL_HL
  | RR_r  (r : Reg8) | RR_HL
  | SLA_r (r : Reg8) | SLA_HL
  | SRA_r (r : Reg8) | SRA_HL
  | SWAP_r (r : Reg8) | SWAP_HL
  | SRL_r (r : Reg8) | SRL_HL
  | BIT_r (bit : Nat) (r : Reg8) | BIT_HL (bit : Nat)
  | RES_r (bit : Nat) (r : Reg8) | RES_HL (bit : Nat)
  | SET_r (bit : Nat) (r : Reg8) | SET_HL (bit : Nat)
  -- Misc
  | DAA | CPL | SCF | CCF
  | NOP | HALT | STOP
  | DI | EI
  -- Jumps / Calls / Returns
  | JP   (cond : Condition) (addr : UInt16)
  | JP_HL
  | JR   (cond : Condition) (offset : UInt8)
  | CALL (cond : Condition) (addr : UInt16)
  | RET  (cond : Condition)
  | RETI
  | RST  (vec : UInt8)
  deriving Repr

end LeanBoy.Cpu
