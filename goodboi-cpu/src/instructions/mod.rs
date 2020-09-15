//! Data types to represent the Game Boy CPU (Sharp LR35902) instruction set.
//!
//! This module also contains methods of decoding byte sequences to CPU instructions.

mod decode;

pub use decode::decode;

use std::fmt::Display;

/// 8-bit register and register-like locations.
///
/// Register `F` (flags register) is not included since it cannot be accessed directly by most operations.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    /// `HLIndirect` (the byte pointed to by the address in register pair `HL`) is frequently grouped with the 8-bit registers in the instruction set.
    HLIndirect,
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Register::*;
        let name = match self {
            A => "A",
            B => "B",
            C => "C",
            D => "D",
            E => "E",
            H => "H",
            L => "L",
            HLIndirect => "(HL)",
        };
        write!(f, "{}", name)
    }
}

/// 16-bit registers formed by pairs of 8-bit `Register`s, as well as the 16-bit stack pointer `SP`.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RegisterPair {
    AF,
    BC,
    DE,
    HL,
    SP,
}

impl Display for RegisterPair {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RegisterPair::*;
        let name = match self {
            AF => "AF",
            BC => "BC",
            DE => "DE",
            HL => "HL",
            SP => "SP",
        };
        write!(f, "{}", name)
    }
}

/// Test conditions for branching instructions.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Condition {
    /// Branch if zero flag is set.
    Zero,
    /// Branch if zero flag is not set.
    NZero,
    /// Branch if carry flag is set.
    Carry,
    /// Branch if carry flag is not set.
    NCarry,
}

impl Display for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Condition::*;
        let name = match self {
            Zero => "Z",
            NZero => "NZ",
            Carry => "C",
            NCarry => "NC",
        };
        write!(f, "{}", name)
    }
}

/// The Game Boy has a 16-bit address bus.
pub type Address = u16;

/// Operations that an instruction can perform.
///
/// Abbreviations used in the documentation for each variant:
/// * r8  - 8-bit register (A, B, C, D, E, H, L)
/// * r16 - 16-bit register pairs (BC, DE, HL)
/// * n8  - 8-bit integer constant
/// * n16 - 16-bit integer constant
/// * r8  - 8-bit signed offset
/// * u3  - 3-bit unsigned integer constant
/// * cc  - condition codes (Z, NZ, C, NC)
/// * vec - one of the reset vectors (0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, 0x38)
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operation {
    /// ADD A, r8
    Add(Register),
    /// ADC A, r8
    AddCarry(Register),
    /// ADC A, n8
    AddCarryImmediate(u8),
    /// ADD A, n8
    AddImmediate(u8),
    /// ADD SP, e8
    AddToSP(i8),
    /// ADD HL, r16
    AddToHL(RegisterPair),
    /// AND A, r8
    And(Register),
    /// AND A, n8
    AndImmediate(u8),
    /// BIT u3, r8
    Bit(u8, Register),
    /// CALL n16
    Call(Address),
    /// CALL cc, n16
    CallCond(Condition, Address),
    /// CP A, r8
    Compare(Register),
    /// CP A, n8
    CompareImmediate(u8),
    /// CPL
    Complement,
    /// CCF
    ComplementCarry,
    /// DAA
    DecimalAdjust,
    /// DEC r8 / DEC (HL)
    Decrement(Register),
    /// DEC r16
    DecrementPair(RegisterPair),
    /// DI
    DisableInterrupts,
    /// EI
    EnableInterrupts,
    /// HALT
    Halt,
    /// INC r8 / INC (HL)
    Increment(Register),
    /// INC r16
    IncrementPair(RegisterPair),
    /// Invalid instruction.
    Invalid,
    /// JP n16
    Jump(Address),
    /// JP cc, n16
    JumpCond(Condition, Address),
    /// JP HL
    JumpHL,
    /// JR e8
    JumpRel(i8),
    /// JR cc, e8
    JumpRelCond(Condition, i8),
    /// LD r8, r8 / LD r8, (HL) / LD (HL), r8
    Load(Register, Register),
    /// LD (r16), A
    LoadAToPair(RegisterPair),
    /// LD (HL-), A
    LoadAToHLD,
    /// LD (HL+), A
    LoadAToHLI,
    /// LD (n16), A
    LoadAToIndirect(Address),
    /// LD ($FF00+C), A
    LoadAToIO,
    /// LD ($FF00+n8), A
    LoadAToIOOffset(u8),
    /// LD A, (r16)
    LoadPairToA(RegisterPair),
    /// LD A, (HL-)
    LoadHLDToA,
    /// LD A, (HL+)
    LoadHLIToA,
    /// LD SP, HL
    LoadHLToSP,
    /// LD A, (n16)
    LoadIndirectToA(Address),
    /// LD A, ($FF00+n8)
    LoadIOOffsetToA(u8),
    /// LD A, ($FF00+C)
    LoadIOToA,
    /// LD r8, n8
    LoadImmediate(Register, u8),
    /// LD r16, n16
    LoadPairImmediate(RegisterPair, u16),
    /// LD HL, SP+e8
    LoadSPOffsetToHL(i8),
    /// LD (n16), SP
    LoadSPToIndirect(Address),
    /// NOP
    NoOp,
    /// OR A, r8
    Or(Register),
    /// OR A, n8
    OrImmediate(u8),
    /// POP r16
    Pop(RegisterPair),
    /// PUSH r16
    Push(RegisterPair),
    /// RST vec
    Reset(u8),
    /// RES u3, r8
    ResetBit(u8, Register),
    /// RET
    Return,
    /// RET cc
    ReturnCond(Condition),
    /// RETI
    ReturnInterrupt,
    /// RL r8
    RotateLeft(Register),
    /// RLA
    RotateLeftA,
    /// RLC r8
    RotateLeftCircular(Register),
    /// RLCA
    RotateLeftCircularA,
    // RR r8
    RotateRight(Register),
    /// RRA
    RotateRightA,
    /// RRC r8
    RotateRightCircular(Register),
    /// RRCA
    RotateRightCircularA,
    /// SET u3, r8
    SetBit(u8, Register),
    /// SCF
    SetCarry,
    /// SLA r8
    ShiftLeft(Register),
    /// SRA r8
    ShiftRight(Register),
    /// SRL r8
    ShiftRightLogic(Register),
    /// STOP
    Stop,
    /// SUB A, r8
    Subtract(Register),
    /// SBC A, r8
    SubtractCarry(Register),
    /// SBC A, n8
    SubtractCarryImmediate(u8),
    /// SUB A, n8
    SubtractImmediate(u8),
    // SWAP r8
    Swap(Register),
    /// XOR A, r8
    Xor(Register),
    /// XOR A, n8
    XorImmediate(u8),
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Operation::*;
        match self {
            Add(reg) => write!(f, "ADD A, {}", reg),
            AddCarry(reg) => write!(f, "ADC A, {}", reg),
            AddCarryImmediate(n) => write!(f, "ADC A, ${:02X}", n),
            AddImmediate(n) => write!(f, "ADD A, ${:02X}", n),
            AddToSP(offset) => write!(f, "ADD SP, ${:02X}", offset),
            AddToHL(pair) => write!(f, "ADD HL, {}", pair),
            And(reg) => write!(f, "AND A, {}", reg),
            AndImmediate(n) => write!(f, "AND A, ${:02X}", n),
            Bit(pos, reg) => write!(f, "BIT {}, {}", pos, reg),
            Call(addr) => write!(f, "CALL ${:04X}", addr),
            CallCond(cond, addr) => write!(f, "CALL {}, ${:04X}", cond, addr),
            Compare(reg) => write!(f, "CP A, {}", reg),
            CompareImmediate(n) => write!(f, "CP A, ${:02X}", n),
            Complement => write!(f, "CPL"),
            ComplementCarry => write!(f, "CCF"),
            DecimalAdjust => write!(f, "DAA"),
            Decrement(reg) => write!(f, "DEC {}", reg),
            DecrementPair(pair) => write!(f, "DEC {}", pair),
            DisableInterrupts => write!(f, "DI"),
            EnableInterrupts => write!(f, "EI"),
            Halt => write!(f, "HALT"),
            Increment(reg) => write!(f, "INC {}", reg),
            IncrementPair(pair) => write!(f, "INC {}", pair),
            Invalid => write!(f, "INVALID"),
            Jump(addr) => write!(f, "JP ${:04X}", addr),
            JumpCond(cond, addr) => write!(f, "JP {}, ${:04X}", cond, addr),
            JumpHL => write!(f, "JP HL"),
            JumpRel(offset) => write!(f, "JR ${:02X}", offset),
            JumpRelCond(cond, offset) => write!(f, "JR {}, ${:02X}", cond, offset),
            Load(dst, src) => write!(f, "LD {}, {}", dst, src),
            LoadAToHLD => write!(f, "LD (HL-), A"),
            LoadAToHLI => write!(f, "LD (HL+), A"),
            LoadAToIndirect(addr) => write!(f, "LD (${:04X}), A", addr),
            LoadAToIO => write!(f, "LD ($FF00+C), A"),
            LoadAToIOOffset(offset) => write!(f, "LD ($FF00+${:02X}), A", offset),
            LoadAToPair(pair) => write!(f, "LD ({}), A", pair),
            LoadHLDToA => write!(f, "LD A, (HL-)"),
            LoadHLIToA => write!(f, "LD A, (HL+)"),
            LoadHLToSP => write!(f, "LD SP, HL"),
            LoadIndirectToA(addr) => write!(f, "LD A, (${:04X})", addr),
            LoadIOOffsetToA(offset) => write!(f, "LD A, ($FF00+${:02X})", offset),
            LoadIOToA => write!(f, "LD A, ($FF00+C)"),
            LoadImmediate(reg, n) => write!(f, "LD {}, ${:02X}", reg, n),
            LoadPairImmediate(pair, n) => write!(f, "LD {}, ${:04X}", pair, n),
            LoadPairToA(pair) => write!(f, "LD A, ({})", pair),
            LoadSPOffsetToHL(offset) => write!(f, "LD HL, SP+${:02X}", offset),
            LoadSPToIndirect(addr) => write!(f, "LD (${:04X}), SP", addr),
            NoOp => write!(f, "NOP"),
            Or(reg) => write!(f, "OR A, {}", reg),
            OrImmediate(n) => write!(f, "OR A, ${:02X}", n),
            Pop(pair) => write!(f, "POP {}", pair),
            Push(pair) => write!(f, "PUSH {}", pair),
            Reset(addr) => write!(f, "RST ${:02X}", addr),
            ResetBit(pos, reg) => write!(f, "RES {}, {}", pos, reg),
            Return => write!(f, "RET"),
            ReturnCond(cond) => write!(f, "RET {}", cond),
            ReturnInterrupt => write!(f, "RETI"),
            RotateLeft(reg) => write!(f, "RL {}", reg),
            RotateLeftA => write!(f, "RLA"),
            RotateLeftCircular(reg) => write!(f, "RLC {}", reg),
            RotateLeftCircularA => write!(f, "RLCA"),
            RotateRight(reg) => write!(f, "RR {}", reg),
            RotateRightA => write!(f, "RRA"),
            RotateRightCircular(reg) => write!(f, "RRC {}", reg),
            RotateRightCircularA => write!(f, "RRCA"),
            SetBit(pos, reg) => write!(f, "SET {}, {}", pos, reg),
            SetCarry => write!(f, "SCF"),
            ShiftLeft(reg) => write!(f, "SLA {}", reg),
            ShiftRight(reg) => write!(f, "SRA {}", reg),
            ShiftRightLogic(reg) => write!(f, "SRL {}", reg),
            Stop => write!(f, "STOP"),
            Subtract(reg) => write!(f, "SUB A, {}", reg),
            SubtractCarry(reg) => write!(f, "SBC A, {}", reg),
            SubtractCarryImmediate(n) => write!(f, "SBC A, ${:02X}", n),
            SubtractImmediate(n) => write!(f, "SUB A, ${:02X}", n),
            Swap(reg) => write!(f, "SWAP {}", reg),
            Xor(reg) => write!(f, "XOR A, {}", reg),
            XorImmediate(n) => write!(f, "XOR A, ${:02X}", n),
        }
    }
}

/// Specification of a Game Boy CPU instruction.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Instruction {
    /// The operation that the instruction performs.
    pub operation: Operation,
    /// The length of the instruction in bytes when encoded. This includes the opcode and any operands.
    pub bytes: u32,
    /// How many clock cycles the instruction takes to execute.
    /// For branching instructions, this is when the branch is not taken.
    pub cycles: u32,

    /// For branching instruction, how many clock cycles the instruction takes to execute when the branch is taken.
    pub cycles_taken: Option<u32>,
}

impl Instruction {
    pub const INVALID: Instruction = Self::new(Operation::Invalid, 1, 4);

    const fn new(operation: Operation, bytes: u32, cycles: u32) -> Self {
        Self {
            operation,
            bytes,
            cycles,
            cycles_taken: None,
        }
    }

    const fn branching(operation: Operation, bytes: u32, cycles: u32, cycles_taken: u32) -> Self {
        Self {
            operation,
            bytes,
            cycles,
            cycles_taken: Some(cycles_taken),
        }
    }
}

/// Iterator adapter that decodes an iterator of bytes into `Instruction`s.
///
/// # Example
///
/// Basic usage:
/// ```
/// # use goodboi_cpu::instructions::{Instructions, Instruction, Operation::{LoadImmediate, NoOp}, Register::B};
///
/// // Create a decoding iterator.
/// let mut instructions: Instructions<_> = vec![0x00, 0x06, 0x12].into_iter().into();
///
/// // next() decodes the next instruction.
/// assert_eq!(Some(Instruction { operation: NoOp, bytes: 1, cycles: 4, cycles_taken: None }), instructions.next());
/// // Some instructions will consume multiple bytes from the iterator.
/// assert_eq!(Some(Instruction { operation: LoadImmediate(B, 0x12), bytes: 2, cycles: 8, cycles_taken: None }), instructions.next());
/// // `None` is returned when there are not enough bytes left to decode a valid instruction.
/// assert_eq!(None, instructions.next());
/// ```
pub struct Instructions<I: Iterator<Item = u8>> {
    iter: I,
}

impl<I> Instructions<I>
where
    I: Iterator<Item = u8>,
{
    pub fn new(iter: I) -> Self {
        Self { iter }
    }
}

impl<I> Iterator for Instructions<I>
where
    I: Iterator<Item = u8>,
{
    type Item = Instruction;

    fn next(&mut self) -> Option<Self::Item> {
        decode(&mut self.iter)
    }
}

impl<I> From<I> for Instructions<I>
where
    I: Iterator<Item = u8>,
{
    fn from(iter: I) -> Self {
        Instructions { iter }
    }
}
