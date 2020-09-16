//! Provides functions for decoding bytes to CPU [`Instruction`](Instruction)s.

use super::{Condition, Instruction, Operation, Register, RegisterPair};

fn next_word(iter: &mut impl Iterator<Item = u8>) -> Option<u16> {
    let low = iter.next()? as u16;
    let high = iter.next()? as u16;
    Some(high << 8 | low)
}

/// Decodes a byte sequence from an iterator to an [`Instruction`](Instruction).
/// Returns `None` if there are not enough bytes left in the `Iterator` to decode a valid instruction.
pub fn decode(bytes: &mut impl Iterator<Item = u8>) -> Option<Instruction> {
    let opcode = bytes.next()?;
    if opcode == 0xCB {
        decode_cbprefixed(bytes)
    } else {
        decode_unprefixed(opcode, bytes)
    }
}

fn decode_unprefixed(opcode: u8, bytes: &mut impl Iterator<Item = u8>) -> Option<Instruction> {
    use self::{Condition::*, Operation::*, Register::*, RegisterPair::*};
    Some(match opcode {
        0x00 => Instruction::new(NoOp, 1, 4),
        0x01 => Instruction::new(LoadPairImmediate(BC, next_word(bytes)?), 3, 12),
        0x02 => Instruction::new(LoadAToPair(BC), 1, 8),
        0x03 => Instruction::new(IncrementPair(BC), 1, 8),
        0x04 => Instruction::new(Increment(B), 1, 4),
        0x05 => Instruction::new(Decrement(B), 1, 4),
        0x06 => Instruction::new(LoadImmediate(B, bytes.next()?), 2, 8),
        0x07 => Instruction::new(RotateLeftCircularA, 1, 4),
        0x08 => Instruction::new(LoadSPToIndirect(next_word(bytes)?), 3, 20),
        0x09 => Instruction::new(AddToHL(BC), 1, 8),
        0x0A => Instruction::new(LoadPairToA(BC), 1, 8),
        0x0B => Instruction::new(DecrementPair(BC), 1, 8),
        0x0C => Instruction::new(Increment(C), 1, 4),
        0x0D => Instruction::new(Decrement(C), 1, 4),
        0x0E => Instruction::new(LoadImmediate(C, bytes.next()?), 2, 8),
        0x0F => Instruction::new(RotateRightCircularA, 1, 4),
        0x10 => Instruction::new(Stop, 1, 4),
        0x11 => Instruction::new(LoadPairImmediate(DE, next_word(bytes)?), 3, 12),
        0x12 => Instruction::new(LoadAToPair(DE), 1, 8),
        0x13 => Instruction::new(IncrementPair(DE), 1, 8),
        0x14 => Instruction::new(Increment(D), 1, 4),
        0x15 => Instruction::new(Decrement(D), 1, 4),
        0x16 => Instruction::new(LoadImmediate(D, bytes.next()?), 2, 8),
        0x17 => Instruction::new(RotateLeftA, 1, 4),
        0x18 => Instruction::new(JumpRel(bytes.next()? as i8), 2, 12),
        0x19 => Instruction::new(AddToHL(DE), 1, 8),
        0x1A => Instruction::new(LoadPairToA(DE), 1, 8),
        0x1B => Instruction::new(DecrementPair(DE), 1, 8),
        0x1C => Instruction::new(Increment(E), 1, 4),
        0x1D => Instruction::new(Decrement(E), 1, 4),
        0x1E => Instruction::new(LoadImmediate(E, bytes.next()?), 2, 8),
        0x1F => Instruction::new(RotateRightA, 1, 4),
        0x20 => Instruction::branching(JumpRelCond(NZero, bytes.next()? as i8), 2, 8, 12),
        0x21 => Instruction::new(LoadPairImmediate(HL, next_word(bytes)?), 3, 12),
        0x22 => Instruction::new(LoadAToHLI, 1, 8),
        0x23 => Instruction::new(IncrementPair(HL), 1, 8),
        0x24 => Instruction::new(Increment(H), 1, 4),
        0x25 => Instruction::new(Decrement(H), 1, 4),
        0x26 => Instruction::new(LoadImmediate(H, bytes.next()?), 2, 8),
        0x27 => Instruction::new(DecimalAdjust, 1, 4),
        0x28 => Instruction::branching(JumpRelCond(Zero, bytes.next()? as i8), 2, 8, 12),
        0x29 => Instruction::new(AddToHL(HL), 1, 8),
        0x2A => Instruction::new(LoadHLIToA, 1, 8),
        0x2B => Instruction::new(DecrementPair(HL), 1, 8),
        0x2C => Instruction::new(Increment(L), 1, 4),
        0x2D => Instruction::new(Decrement(L), 1, 4),
        0x2E => Instruction::new(LoadImmediate(L, bytes.next()?), 2, 8),
        0x2F => Instruction::new(Complement, 1, 4),
        0x30 => Instruction::branching(JumpRelCond(NCarry, bytes.next()? as i8), 2, 8, 12),
        0x31 => Instruction::new(LoadPairImmediate(SP, next_word(bytes)?), 3, 12),
        0x32 => Instruction::new(LoadAToHLD, 1, 8),
        0x33 => Instruction::new(IncrementPair(SP), 1, 8),
        0x34 => Instruction::new(Increment(HLIndirect), 1, 12),
        0x35 => Instruction::new(Decrement(HLIndirect), 1, 12),
        0x36 => Instruction::new(LoadImmediate(HLIndirect, bytes.next()?), 2, 12),
        0x37 => Instruction::new(SetCarry, 1, 4),
        0x38 => Instruction::branching(JumpRelCond(Carry, bytes.next()? as i8), 2, 8, 12),
        0x39 => Instruction::new(AddToHL(SP), 1, 8),
        0x3A => Instruction::new(LoadHLDToA, 1, 8),
        0x3B => Instruction::new(DecrementPair(SP), 1, 8),
        0x3C => Instruction::new(Increment(A), 1, 4),
        0x3D => Instruction::new(Decrement(A), 1, 4),
        0x3E => Instruction::new(LoadImmediate(A, bytes.next()?), 2, 8),
        0x3F => Instruction::new(ComplementCarry, 1, 4),
        0x40 => Instruction::new(Load(B, B), 1, 4),
        0x41 => Instruction::new(Load(B, C), 1, 4),
        0x42 => Instruction::new(Load(B, D), 1, 4),
        0x43 => Instruction::new(Load(B, E), 1, 4),
        0x44 => Instruction::new(Load(B, H), 1, 4),
        0x45 => Instruction::new(Load(B, L), 1, 4),
        0x46 => Instruction::new(Load(B, HLIndirect), 1, 8),
        0x47 => Instruction::new(Load(B, A), 1, 4),
        0x48 => Instruction::new(Load(C, B), 1, 4),
        0x49 => Instruction::new(Load(C, C), 1, 4),
        0x4A => Instruction::new(Load(C, D), 1, 4),
        0x4B => Instruction::new(Load(C, E), 1, 4),
        0x4C => Instruction::new(Load(C, H), 1, 4),
        0x4D => Instruction::new(Load(C, L), 1, 4),
        0x4E => Instruction::new(Load(C, HLIndirect), 1, 8),
        0x4F => Instruction::new(Load(C, A), 1, 4),
        0x50 => Instruction::new(Load(D, B), 1, 4),
        0x51 => Instruction::new(Load(D, C), 1, 4),
        0x52 => Instruction::new(Load(D, D), 1, 4),
        0x53 => Instruction::new(Load(D, E), 1, 4),
        0x54 => Instruction::new(Load(D, H), 1, 4),
        0x55 => Instruction::new(Load(D, L), 1, 4),
        0x56 => Instruction::new(Load(D, HLIndirect), 1, 8),
        0x57 => Instruction::new(Load(D, A), 1, 4),
        0x58 => Instruction::new(Load(E, B), 1, 4),
        0x59 => Instruction::new(Load(E, C), 1, 4),
        0x5A => Instruction::new(Load(E, D), 1, 4),
        0x5B => Instruction::new(Load(E, E), 1, 4),
        0x5C => Instruction::new(Load(E, H), 1, 4),
        0x5D => Instruction::new(Load(E, L), 1, 4),
        0x5E => Instruction::new(Load(E, HLIndirect), 1, 8),
        0x5F => Instruction::new(Load(E, A), 1, 4),
        0x60 => Instruction::new(Load(H, B), 1, 4),
        0x61 => Instruction::new(Load(H, C), 1, 4),
        0x62 => Instruction::new(Load(H, D), 1, 4),
        0x63 => Instruction::new(Load(H, E), 1, 4),
        0x64 => Instruction::new(Load(H, H), 1, 4),
        0x65 => Instruction::new(Load(H, L), 1, 4),
        0x66 => Instruction::new(Load(H, HLIndirect), 1, 8),
        0x67 => Instruction::new(Load(H, A), 1, 4),
        0x68 => Instruction::new(Load(L, B), 1, 4),
        0x69 => Instruction::new(Load(L, C), 1, 4),
        0x6A => Instruction::new(Load(L, D), 1, 4),
        0x6B => Instruction::new(Load(L, E), 1, 4),
        0x6C => Instruction::new(Load(L, H), 1, 4),
        0x6D => Instruction::new(Load(L, L), 1, 4),
        0x6E => Instruction::new(Load(L, HLIndirect), 1, 8),
        0x6F => Instruction::new(Load(L, A), 1, 4),
        0x70 => Instruction::new(Load(HLIndirect, B), 1, 8),
        0x71 => Instruction::new(Load(HLIndirect, C), 1, 8),
        0x72 => Instruction::new(Load(HLIndirect, D), 1, 8),
        0x73 => Instruction::new(Load(HLIndirect, E), 1, 8),
        0x74 => Instruction::new(Load(HLIndirect, H), 1, 8),
        0x75 => Instruction::new(Load(HLIndirect, L), 1, 8),
        0x76 => Instruction::new(Halt, 1, 4),
        0x77 => Instruction::new(Load(HLIndirect, A), 1, 8),
        0x78 => Instruction::new(Load(A, B), 1, 4),
        0x79 => Instruction::new(Load(A, C), 1, 4),
        0x7A => Instruction::new(Load(A, D), 1, 4),
        0x7B => Instruction::new(Load(A, E), 1, 4),
        0x7C => Instruction::new(Load(A, H), 1, 4),
        0x7D => Instruction::new(Load(A, L), 1, 4),
        0x7E => Instruction::new(Load(A, HLIndirect), 1, 8),
        0x7F => Instruction::new(Load(A, A), 1, 4),
        0x80 => Instruction::new(Add(B), 1, 4),
        0x81 => Instruction::new(Add(C), 1, 4),
        0x82 => Instruction::new(Add(D), 1, 4),
        0x83 => Instruction::new(Add(E), 1, 4),
        0x84 => Instruction::new(Add(H), 1, 4),
        0x85 => Instruction::new(Add(L), 1, 4),
        0x86 => Instruction::new(Add(HLIndirect), 1, 8),
        0x87 => Instruction::new(Add(A), 1, 4),
        0x88 => Instruction::new(AddCarry(B), 1, 4),
        0x89 => Instruction::new(AddCarry(C), 1, 4),
        0x8A => Instruction::new(AddCarry(D), 1, 4),
        0x8B => Instruction::new(AddCarry(E), 1, 4),
        0x8C => Instruction::new(AddCarry(H), 1, 4),
        0x8D => Instruction::new(AddCarry(L), 1, 4),
        0x8E => Instruction::new(AddCarry(HLIndirect), 1, 8),
        0x8F => Instruction::new(AddCarry(A), 1, 4),
        0x90 => Instruction::new(Subtract(B), 1, 4),
        0x91 => Instruction::new(Subtract(C), 1, 4),
        0x92 => Instruction::new(Subtract(D), 1, 4),
        0x93 => Instruction::new(Subtract(E), 1, 4),
        0x94 => Instruction::new(Subtract(H), 1, 4),
        0x95 => Instruction::new(Subtract(L), 1, 4),
        0x96 => Instruction::new(Subtract(HLIndirect), 1, 8),
        0x97 => Instruction::new(Subtract(A), 1, 4),
        0x98 => Instruction::new(SubtractCarry(B), 1, 4),
        0x99 => Instruction::new(SubtractCarry(C), 1, 4),
        0x9A => Instruction::new(SubtractCarry(D), 1, 4),
        0x9B => Instruction::new(SubtractCarry(E), 1, 4),
        0x9C => Instruction::new(SubtractCarry(H), 1, 4),
        0x9D => Instruction::new(SubtractCarry(L), 1, 4),
        0x9E => Instruction::new(SubtractCarry(HLIndirect), 1, 8),
        0x9F => Instruction::new(SubtractCarry(A), 1, 4),
        0xA0 => Instruction::new(And(B), 1, 4),
        0xA1 => Instruction::new(And(C), 1, 4),
        0xA2 => Instruction::new(And(D), 1, 4),
        0xA3 => Instruction::new(And(E), 1, 4),
        0xA4 => Instruction::new(And(H), 1, 4),
        0xA5 => Instruction::new(And(L), 1, 4),
        0xA6 => Instruction::new(And(HLIndirect), 1, 8),
        0xA7 => Instruction::new(And(A), 1, 4),
        0xA8 => Instruction::new(Xor(B), 1, 4),
        0xA9 => Instruction::new(Xor(C), 1, 4),
        0xAA => Instruction::new(Xor(D), 1, 4),
        0xAB => Instruction::new(Xor(E), 1, 4),
        0xAC => Instruction::new(Xor(H), 1, 4),
        0xAD => Instruction::new(Xor(L), 1, 4),
        0xAE => Instruction::new(Xor(HLIndirect), 1, 8),
        0xAF => Instruction::new(Xor(A), 1, 4),
        0xB0 => Instruction::new(Or(B), 1, 4),
        0xB1 => Instruction::new(Or(C), 1, 4),
        0xB2 => Instruction::new(Or(D), 1, 4),
        0xB3 => Instruction::new(Or(E), 1, 4),
        0xB4 => Instruction::new(Or(H), 1, 4),
        0xB5 => Instruction::new(Or(L), 1, 4),
        0xB6 => Instruction::new(Or(HLIndirect), 1, 8),
        0xB7 => Instruction::new(Or(A), 1, 4),
        0xB8 => Instruction::new(Compare(B), 1, 4),
        0xB9 => Instruction::new(Compare(C), 1, 4),
        0xBA => Instruction::new(Compare(D), 1, 4),
        0xBB => Instruction::new(Compare(E), 1, 4),
        0xBC => Instruction::new(Compare(H), 1, 4),
        0xBD => Instruction::new(Compare(L), 1, 4),
        0xBE => Instruction::new(Compare(HLIndirect), 1, 8),
        0xBF => Instruction::new(Compare(A), 1, 4),
        0xC0 => Instruction::branching(ReturnCond(NZero), 1, 8, 20),
        0xC1 => Instruction::new(Pop(BC), 1, 12),
        0xC2 => Instruction::branching(JumpCond(NZero, next_word(bytes)?), 3, 12, 16),
        0xC3 => Instruction::new(Jump(next_word(bytes)?), 3, 16),
        0xC4 => Instruction::branching(CallCond(NZero, next_word(bytes)?), 3, 12, 24),
        0xC5 => Instruction::new(Push(BC), 1, 16),
        0xC6 => Instruction::new(AddImmediate(bytes.next()?), 2, 8),
        0xC7 => Instruction::new(Reset(0x00), 1, 16),
        0xC8 => Instruction::branching(ReturnCond(Zero), 1, 8, 20),
        0xC9 => Instruction::new(Return, 1, 16),
        0xCA => Instruction::branching(JumpCond(Zero, next_word(bytes)?), 3, 12, 16),
        0xCC => Instruction::branching(CallCond(Zero, next_word(bytes)?), 3, 12, 24),
        0xCD => Instruction::new(Call(next_word(bytes)?), 3, 24),
        0xCE => Instruction::new(AddCarryImmediate(bytes.next()?), 2, 8),
        0xCF => Instruction::new(Reset(0x08), 1, 16),
        0xD0 => Instruction::branching(ReturnCond(NCarry), 1, 8, 20),
        0xD1 => Instruction::new(Pop(DE), 1, 12),
        0xD2 => Instruction::branching(JumpCond(NCarry, next_word(bytes)?), 3, 12, 16),
        0xD4 => Instruction::branching(CallCond(NCarry, next_word(bytes)?), 3, 12, 24),
        0xD5 => Instruction::new(Push(DE), 1, 16),
        0xD6 => Instruction::new(SubtractImmediate(bytes.next()?), 2, 8),
        0xD7 => Instruction::new(Reset(0x10), 1, 16),
        0xD8 => Instruction::branching(ReturnCond(Carry), 1, 8, 20),
        0xD9 => Instruction::new(ReturnInterrupt, 1, 16),
        0xDA => Instruction::branching(JumpCond(Carry, next_word(bytes)?), 3, 12, 16),
        0xDC => Instruction::branching(CallCond(Carry, next_word(bytes)?), 3, 12, 24),
        0xDE => Instruction::new(SubtractCarryImmediate(bytes.next()?), 2, 8),
        0xDF => Instruction::new(Reset(0x18), 1, 16),
        0xE0 => Instruction::new(LoadAToIOOffset(bytes.next()?), 2, 12),
        0xE1 => Instruction::new(Pop(HL), 1, 12),
        0xE2 => Instruction::new(LoadAToIO, 1, 8),
        0xE5 => Instruction::new(Push(HL), 1, 16),
        0xE6 => Instruction::new(AndImmediate(bytes.next()?), 2, 8),
        0xE7 => Instruction::new(Reset(0x20), 1, 16),
        0xE8 => Instruction::new(AddToSP(bytes.next()? as i8), 2, 16),
        0xE9 => Instruction::new(JumpHL, 1, 4),
        0xEA => Instruction::new(LoadAToIndirect(next_word(bytes)?), 3, 16),
        0xEE => Instruction::new(XorImmediate(bytes.next()?), 2, 8),
        0xEF => Instruction::new(Reset(0x28), 1, 16),
        0xF0 => Instruction::new(LoadIOOffsetToA(bytes.next()?), 2, 12),
        0xF1 => Instruction::new(Pop(AF), 1, 12),
        0xF2 => Instruction::new(LoadIOToA, 1, 8),
        0xF3 => Instruction::new(DisableInterrupts, 1, 4),
        0xF5 => Instruction::new(Push(AF), 1, 16),
        0xF6 => Instruction::new(OrImmediate(bytes.next()?), 2, 8),
        0xF7 => Instruction::new(Reset(0x30), 1, 16),
        0xF8 => Instruction::new(LoadSPOffsetToHL(bytes.next()? as i8), 2, 12),
        0xF9 => Instruction::new(LoadHLToSP, 1, 8),
        0xFA => Instruction::new(LoadIndirectToA(next_word(bytes)?), 3, 16),
        0xFB => Instruction::new(EnableInterrupts, 1, 4),
        0xFE => Instruction::new(CompareImmediate(bytes.next()?), 2, 8),
        0xFF => Instruction::new(Reset(0x38), 1, 16),
        _ => Instruction::INVALID,
    })
}

pub fn decode_cbprefixed(bytes: &mut impl Iterator<Item = u8>) -> Option<Instruction> {
    use super::{Operation::*, Register::*};
    let opcode = bytes.next()?;
    let x = opcode >> 6;
    let y = (opcode >> 3) & 0b111;
    let register = decode_register(opcode);
    Some(match x {
        0 => {
            let operation = match y {
                0 => RotateLeftCircular(register),
                1 => RotateRightCircular(register),
                2 => RotateLeft(register),
                3 => RotateRight(register),
                4 => ShiftLeft(register),
                5 => ShiftRight(register),
                6 => Swap(register),
                7 => ShiftRightLogic(register),
                _ => unreachable!(),
            };
            let cycles = if register == HLIndirect { 16 } else { 8 };
            Instruction::new(operation, 2, cycles)
        }
        1 => Instruction::new(
            Bit(y, register),
            2,
            if register == HLIndirect { 12 } else { 8 },
        ),
        2 => Instruction::new(
            ResetBit(y, register),
            2,
            if register == HLIndirect { 16 } else { 8 },
        ),
        3 => Instruction::new(
            SetBit(y, register),
            2,
            if register == HLIndirect { 16 } else { 8 },
        ),
        _ => unreachable!(),
    })
}

fn decode_register(value: u8) -> Register {
    use Register::*;
    match value & 0b111 {
        0 => B,
        1 => C,
        2 => D,
        3 => E,
        4 => H,
        5 => L,
        6 => HLIndirect,
        7 => A,
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_decode(bytes: Vec<u8>, expected: Instruction) {
        let instruction = decode(&mut bytes.into_iter());

        assert!(instruction.is_some());
        assert_eq!(instruction.unwrap(), expected);
    }

    #[test]
    fn decodes_instructions_with_no_operands() {
        test_decode(
            vec![0x00],
            Instruction {
                operation: Operation::NoOp,
                bytes: 1,
                cycles: 4,
                cycles_taken: None,
            },
        );
    }

    #[test]
    fn decodes_instructions_with_one_operand() {
        test_decode(
            vec![0x01, 0x34, 0x12],
            Instruction {
                operation: Operation::LoadPairImmediate(RegisterPair::BC, 0x1234),
                bytes: 3,
                cycles: 12,
                cycles_taken: None,
            },
        )
    }

    #[test]
    fn returns_none_when_not_enough_bytes() {
        let bytes = vec![0x01, 0x34];

        let instruction = decode(&mut bytes.into_iter());

        assert!(instruction.is_none());
    }
}
