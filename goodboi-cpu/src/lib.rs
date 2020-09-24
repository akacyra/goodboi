use bitfield::bitfield;
use goodboi_memory::MemoryMapped;
use instructions::{Address, Condition, Instruction, Operation, Register};

pub mod instructions;

bitfield! {
    #[derive(Copy, Clone, PartialEq)]
    pub struct RegisterPair(u16);
    impl Debug;
    u8;
    high, set_high: 15, 8;
    low, set_low: 7, 0;
}

bitfield! {
    #[derive(Copy, Clone, PartialEq)]
    pub struct RegisterAndFlags(u16);
    impl Debug;
    u8;
    high, set_high: 15, 8;
    zero, set_zero: 7;
    subtract, set_subtract: 6;
    half_carry, set_half_carry: 5;
    carry, set_carry: 4;
}

impl RegisterAndFlags {
    pub fn clear_flags(&mut self) {
        self.set_zero(false);
        self.set_subtract(false);
        self.set_half_carry(false);
        self.set_carry(false);
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum State {
    Running,
    Stopped,
    Halted,
}

type ProgramCounter = u16;
type Cycles = u32;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct CPU {
    pub af: RegisterAndFlags,
    pub bc: RegisterPair,
    pub de: RegisterPair,
    pub hl: RegisterPair,
    pub pc: ProgramCounter,
    pub sp: Address,
    pub ime: bool,
    pub state: State,
}

impl CPU {
    pub fn new() -> Self {
        Self {
            af: RegisterAndFlags(0),
            bc: RegisterPair(0),
            de: RegisterPair(0),
            hl: RegisterPair(0),
            pc: 0,
            sp: 0,
            ime: true,
            state: State::Running,
        }
    }

    pub fn pc_iter<'a>(&self, memory: &'a impl MemoryMapped) -> impl Iterator<Item = u8> + 'a {
        let mut pc = self.pc;
        std::iter::from_fn(move || {
            let value = memory.read_byte(pc);
            pc = pc.wrapping_add(1);
            Some(value)
        })
    }

    pub fn step(&mut self, memory: &mut impl MemoryMapped) -> Cycles {
        if self.state == State::Running {
            let instruction = {
                let mut pc_iter = self.pc_iter(memory);
                instructions::decode(&mut pc_iter).expect("Failed to decode instruction")
            };
            //println!("PC={:04X}, Op={}", self.pc, instruction.operation);
            let (next_pc, cycles) = self.execute(instruction, memory);
            self.pc = next_pc;
            cycles
        } else {
            0
        }
    }

    fn execute(
        &mut self,
        instruction: Instruction,
        memory: &mut impl MemoryMapped,
    ) -> (ProgramCounter, Cycles) {
        use Operation::*;
        match instruction.operation {
            Add(reg) => self.add(self.get_register(reg, memory), false),
            AddCarry(reg) => self.add(self.get_register(reg, memory), true),
            AddCarryImmediate(n) => self.add(n, true),
            AddImmediate(n) => self.add(n, false),
            AddToSP(offset) => self.sp = self.add_to_sp(offset),
            AddToHL(pair) => self.add_to_hl(pair),
            And(reg) => self.and(self.get_register(reg, memory)),
            AndImmediate(n) => self.and(n),
            Bit(pos, reg) => self.bit(pos, reg, memory),
            Call(addr) => return self.call(addr, instruction, memory),
            CallCond(cond, addr) => return self.call_cond(cond, addr, instruction, memory),
            Compare(reg) => self.compare(self.get_register(reg, memory)),
            CompareImmediate(n) => self.compare(n),
            Complement => self.complement(),
            ComplementCarry => self.set_carry_flag(!self.af.carry()),
            DecimalAdjust => self.decimal_adjust(),
            Decrement(reg) => self.decrement(reg, memory),
            DecrementPair(pair) => self.decrement_pair(pair),
            DisableInterrupts => self.ime = false,
            EnableInterrupts => self.ime = true, // TODO: this only should be set after the instruction following EI
            Halt => self.state = State::Halted,
            Increment(reg) => self.increment(reg, memory),
            IncrementPair(pair) => self.increment_pair(pair),
            Invalid => (),
            Jump(addr) => return (addr, instruction.cycles),
            JumpCond(cond, addr) => return self.jump_cond(cond, addr, instruction),
            JumpHL => return (self.hl.0, instruction.cycles),
            JumpRel(offset) => return self.jump_rel(offset, instruction),
            JumpRelCond(cond, offset) => return self.jump_rel_cond(cond, offset, instruction),
            Load(dst, src) => self.load(dst, src, memory),
            LoadAToPair(pair) => self.load_a_to_pair(pair, memory),
            LoadAToHLD => self.load_a_to_hld(memory),
            LoadAToHLI => self.load_a_to_hli(memory),
            LoadAToIndirect(addr) => self.load_a_to_indirect(addr, memory),
            LoadAToIO => self.load_a_to_io(self.bc.low(), memory),
            LoadAToIOOffset(offset) => self.load_a_to_io(offset, memory),
            LoadPairToA(pair) => self.load_pair_to_a(pair, memory),
            LoadHLDToA => self.load_hld_to_a(memory),
            LoadHLIToA => self.load_hli_to_a(memory),
            LoadHLToSP => self.sp = self.hl.0,
            LoadIndirectToA(addr) => self.load_indirect_to_a(addr, memory),
            LoadIOOffsetToA(offset) => self.load_io_to_a(offset, memory),
            LoadIOToA => self.load_io_to_a(self.bc.low(), memory),
            LoadImmediate(reg, n) => self.set_register(reg, n, memory),
            LoadPairImmediate(pair, n) => self.set_register_pair(pair, n),
            LoadSPOffsetToHL(offset) => self.hl.0 = self.add_to_sp(offset),
            LoadSPToIndirect(addr) => memory.write_word(addr, self.sp),
            NoOp => (),
            Or(reg) => self.or(self.get_register(reg, memory)),
            OrImmediate(n) => self.or(n),
            Pop(pair) => self.pop_register_pair(pair, memory),
            Push(pair) => self.push_register_pair(pair, memory),
            Reset(addr) => return self.call(addr as u16, instruction, memory),
            ResetBit(pos, reg) => self.reset_bit(pos, reg, memory),
            Return => return self.ret(instruction, false, memory),
            ReturnCond(cond) => return self.ret_cond(cond, instruction, memory),
            ReturnInterrupt => return self.ret(instruction, true, memory),
            RotateLeft(reg) => self.rotate_left(reg, memory, true),
            RotateLeftA => self.rotate_left(Register::A, memory, false),
            RotateLeftCircular(reg) => self.rotate_left_circular(reg, memory, true),
            RotateLeftCircularA => self.rotate_left_circular(Register::A, memory, false),
            RotateRight(reg) => self.rotate_right(reg, memory, true),
            RotateRightA => self.rotate_right(Register::A, memory, false),
            RotateRightCircular(reg) => self.rotate_right_circular(reg, memory, true),
            RotateRightCircularA => self.rotate_right_circular(Register::A, memory, false),
            SetBit(pos, reg) => self.set_bit(pos, reg, memory),
            SetCarry => self.set_carry_flag(true),
            ShiftLeft(reg) => self.shift_left(reg, memory),
            ShiftRight(reg) => self.shift_right(reg, memory),
            ShiftRightLogic(reg) => self.shift_right_logic(reg, memory),
            Stop => self.state = State::Stopped,
            Subtract(reg) => self.subtract(self.get_register(reg, memory), false),
            SubtractCarry(reg) => self.subtract(self.get_register(reg, memory), true),
            SubtractCarryImmediate(n) => self.subtract(n, true),
            SubtractImmediate(n) => self.subtract(n, false),
            Swap(reg) => self.swap(reg, memory),
            Xor(reg) => self.xor(self.get_register(reg, memory)),
            XorImmediate(n) => self.xor(n),
        }
        (
            self.pc.wrapping_add(instruction.bytes as u16),
            instruction.cycles,
        )
    }

    fn add(&mut self, value: u8, with_carry: bool) {
        let values = [value, if with_carry && self.af.carry() { 1 } else { 0 }];
        let init = (self.af.high(), false, false);
        let (new_value, carry, half_carry) =
            values
                .iter()
                .fold(init, |(value_acc, carry_acc, half_carry_acc), &value| {
                    let (new_value, carry) = value_acc.overflowing_add(value);
                    let half_carry = ((value_acc & 0xF) + (value & 0xF)) > 0xF;
                    (new_value, carry_acc || carry, half_carry_acc || half_carry)
                });
        self.af.set_high(new_value);
        self.af.set_zero(new_value == 0);
        self.af.set_subtract(false);
        self.af.set_half_carry(half_carry);
        self.af.set_carry(carry);
    }

    fn subtract(&mut self, value: u8, with_carry: bool) {
        let values = [value, if with_carry && self.af.carry() { 1 } else { 0 }];
        let init = (self.af.high(), false, false);
        let (new_value, borrow, half_borrow) =
            values
                .iter()
                .fold(init, |(value_acc, borrow_acc, half_borrow_acc), &value| {
                    let (new_value, borrow) = value_acc.overflowing_sub(value);
                    let half_borrow = (value & 0xF) > (value_acc & 0xF);
                    (
                        new_value,
                        borrow_acc || borrow,
                        half_borrow_acc || half_borrow,
                    )
                });
        self.af.set_high(new_value);
        self.af.set_zero(new_value == 0);
        self.af.set_subtract(true);
        self.af.set_half_carry(half_borrow);
        self.af.set_carry(borrow);
    }

    fn and(&mut self, value: u8) {
        let new_value = self.af.high() & value;
        self.af.set_high(new_value);
        self.af.clear_flags();
        self.af.set_zero(new_value == 0);
        self.af.set_half_carry(true);
    }

    fn or(&mut self, value: u8) {
        let new_value = self.af.high() | value;
        self.af.set_high(new_value);
        self.af.clear_flags();
        self.af.set_zero(new_value == 0);
    }

    fn xor(&mut self, value: u8) {
        let new_value = self.af.high() ^ value;
        self.af.set_high(new_value);
        self.af.clear_flags();
        self.af.set_zero(new_value == 0);
    }

    fn compare(&mut self, value: u8) {
        let a = self.af.high();
        let new_value = a.wrapping_sub(value);
        self.af.set_zero(new_value == 0);
        self.af.set_subtract(true);
        self.af.set_half_carry((value & 0xF) > (a & 0xF));
        self.af.set_carry(value > a);
    }

    fn increment(&mut self, register: Register, memory: &mut impl MemoryMapped) {
        let value = self.get_register(register, memory);
        let new_value = value.wrapping_add(1);
        self.set_register(register, new_value, memory);
        self.af.set_zero(new_value == 0);
        self.af.set_subtract(false);
        self.af.set_half_carry(value & 0xF == 0xF);
    }

    fn decrement(&mut self, register: Register, memory: &mut impl MemoryMapped) {
        let value = self.get_register(register, memory);
        let new_value = value.wrapping_sub(1);
        self.set_register(register, new_value, memory);
        self.af.set_zero(new_value == 0);
        self.af.set_subtract(true);
        self.af.set_half_carry(value & 0xF == 0);
    }

    fn set_carry_flag(&mut self, value: bool) {
        self.af.set_subtract(false);
        self.af.set_half_carry(false);
        self.af.set_carry(value);
    }

    fn complement(&mut self) {
        self.af.set_high(!self.af.high());
        self.af.set_subtract(true);
        self.af.set_half_carry(true);
    }

    fn load(&mut self, dest: Register, source: Register, memory: &mut impl MemoryMapped) {
        self.set_register(dest, self.get_register(source, memory), memory);
    }

    fn load_a_to_pair(
        &self,
        register_pair: instructions::RegisterPair,
        memory: &mut impl MemoryMapped,
    ) {
        memory.write_byte(self.get_register_pair(register_pair), self.af.high());
    }

    fn load_pair_to_a(
        &mut self,
        register_pair: instructions::RegisterPair,
        memory: &impl MemoryMapped,
    ) {
        self.af
            .set_high(memory.read_byte(self.get_register_pair(register_pair)));
    }

    fn load_a_to_hld(&mut self, memory: &mut impl MemoryMapped) {
        let addr = self.hl.0;
        memory.write_byte(addr, self.af.high());
        self.hl.0 = addr.wrapping_sub(1);
    }

    fn load_a_to_hli(&mut self, memory: &mut impl MemoryMapped) {
        let addr = self.hl.0;
        memory.write_byte(addr, self.af.high());
        self.hl.0 = addr.wrapping_add(1);
    }

    fn load_hld_to_a(&mut self, memory: &impl MemoryMapped) {
        let addr = self.hl.0;
        self.af.set_high(memory.read_byte(addr));
        self.hl.0 = addr.wrapping_sub(1);
    }

    fn load_hli_to_a(&mut self, memory: &impl MemoryMapped) {
        let addr = self.hl.0;
        self.af.set_high(memory.read_byte(addr));
        self.hl.0 = addr.wrapping_add(1);
    }

    fn load_a_to_io(&self, offset: u8, memory: &mut impl MemoryMapped) {
        self.load_a_to_indirect(0xFF00 + offset as u16, memory);
    }

    fn load_io_to_a(&mut self, offset: u8, memory: &impl MemoryMapped) {
        self.load_indirect_to_a(0xFF00 + offset as u16, memory)
    }

    fn load_a_to_indirect(&self, address: u16, memory: &mut impl MemoryMapped) {
        memory.write_byte(address, self.af.high());
    }

    fn load_indirect_to_a(&mut self, address: u16, memory: &impl MemoryMapped) {
        self.af.set_high(memory.read_byte(address));
    }

    fn add_to_sp(&mut self, offset: i8) -> u16 {
        self.af.clear_flags();
        // Carry flags for E8 and F8 are based on low byte of SP added to UNSIGNED offset
        let half_carry = ((self.sp & 0xF) + ((offset as u8) as u16 & 0xF)) > 0xF;
        let carry = ((self.sp & 0xFF) + (offset as u8) as u16) > 0xFF;
        self.af.set_half_carry(half_carry);
        self.af.set_carry(carry);
        if offset > 0 {
            self.sp.wrapping_add(offset as u16)
        } else {
            self.sp.wrapping_sub(-offset as u16)
        }
    }

    fn push(&mut self, value: u16, memory: &mut impl MemoryMapped) {
        self.sp = self.sp.wrapping_sub(2);
        memory.write_word(self.sp, value);
    }

    fn pop(&mut self, memory: &impl MemoryMapped) -> u16 {
        let value = memory.read_word(self.sp);
        self.sp = self.sp.wrapping_add(2);
        value
    }

    fn push_register_pair(
        &mut self,
        register_pair: instructions::RegisterPair,
        memory: &mut impl MemoryMapped,
    ) {
        self.push(self.get_register_pair(register_pair), memory);
    }

    fn pop_register_pair(
        &mut self,
        register_pair: instructions::RegisterPair,
        memory: &impl MemoryMapped,
    ) {
        let value = self.pop(memory);
        self.set_register_pair(register_pair, value);
    }

    fn eval_condition(&self, condition: Condition) -> bool {
        match condition {
            Condition::Zero => self.af.zero(),
            Condition::NZero => !self.af.zero(),
            Condition::Carry => self.af.carry(),
            Condition::NCarry => !self.af.carry(),
        }
    }

    fn ret(
        &mut self,
        instruction: Instruction,
        enable_interrupts: bool,
        memory: &impl MemoryMapped,
    ) -> (ProgramCounter, Cycles) {
        if enable_interrupts {
            self.ime = true;
        }
        (self.pop(memory), instruction.cycles)
    }

    fn ret_cond(
        &mut self,
        condition: Condition,
        instruction: Instruction,
        memory: &impl MemoryMapped,
    ) -> (ProgramCounter, Cycles) {
        if self.eval_condition(condition) {
            (self.pop(memory), instruction.cycles_taken.unwrap())
        } else {
            (
                self.pc.wrapping_add(instruction.bytes as u16),
                instruction.cycles,
            )
        }
    }

    fn call(
        &mut self,
        address: Address,
        instruction: Instruction,
        memory: &mut impl MemoryMapped,
    ) -> (ProgramCounter, Cycles) {
        let next_pc = self.pc.wrapping_add(instruction.bytes as u16);
        self.push(next_pc, memory);
        (address, instruction.cycles)
    }

    fn call_cond(
        &mut self,
        condition: Condition,
        address: Address,
        instruction: Instruction,
        memory: &mut impl MemoryMapped,
    ) -> (ProgramCounter, Cycles) {
        let next_pc = self.pc.wrapping_add(instruction.bytes as u16);
        if self.eval_condition(condition) {
            self.push(next_pc, memory);
            (address, instruction.cycles_taken.unwrap())
        } else {
            (next_pc, instruction.cycles)
        }
    }

    fn jump_cond(
        &self,
        condition: Condition,
        address: Address,
        instruction: Instruction,
    ) -> (ProgramCounter, Cycles) {
        if self.eval_condition(condition) {
            (address, instruction.cycles_taken.unwrap())
        } else {
            (
                self.pc.wrapping_add(instruction.bytes as u16),
                instruction.cycles,
            )
        }
    }

    fn jump_rel(&self, offset: i8, instruction: Instruction) -> (ProgramCounter, Cycles) {
        let next_pc = if offset > 0 {
            self.pc.wrapping_add(offset as u16)
        } else {
            self.pc.wrapping_sub(-offset as u16)
        };
        (
            next_pc.wrapping_add(instruction.bytes as u16),
            instruction.cycles,
        )
    }

    fn jump_rel_cond(
        &self,
        condition: Condition,
        offset: i8,
        instruction: Instruction,
    ) -> (ProgramCounter, Cycles) {
        let next_pc = self.pc.wrapping_add(instruction.bytes as u16);
        if self.eval_condition(condition) {
            let next_pc = if offset > 0 {
                next_pc.wrapping_add(offset as u16)
            } else {
                next_pc.wrapping_sub(-offset as u16)
            };
            (next_pc, instruction.cycles_taken.unwrap())
        } else {
            (next_pc, instruction.cycles)
        }
    }

    fn increment_pair(&mut self, register_pair: instructions::RegisterPair) {
        let new_value = self.get_register_pair(register_pair).wrapping_add(1);
        self.set_register_pair(register_pair, new_value);
    }

    fn decrement_pair(&mut self, register_pair: instructions::RegisterPair) {
        let new_value = self.get_register_pair(register_pair).wrapping_sub(1);
        self.set_register_pair(register_pair, new_value);
    }

    fn add_to_hl(&mut self, register_pair: instructions::RegisterPair) {
        let value = self.get_register_pair(register_pair);
        let (new_value, carry) = self.hl.0.overflowing_add(value);
        let half_carry = ((self.hl.0 & 0xFFF) + (value & 0xFFF)) > 0xFFF;
        self.hl.0 = new_value;
        self.af.set_subtract(false);
        self.af.set_half_carry(half_carry);
        self.af.set_carry(carry);
    }

    fn bit(&mut self, position: u8, register: Register, memory: &impl MemoryMapped) {
        let value = self.get_register(register, memory);
        self.af.set_zero(value & (1 << position) == 0);
        self.af.set_subtract(false);
        self.af.set_half_carry(true);
    }

    fn reset_bit(&mut self, position: u8, register: Register, memory: &mut impl MemoryMapped) {
        let value = self.get_register(register, memory);
        let new_value = value & !(1 << position);
        self.set_register(register, new_value, memory);
    }

    fn swap(&mut self, register: Register, memory: &mut impl MemoryMapped) {
        let value = self.get_register(register, memory);
        let new_value = (value & 0xF) << 4 | (value & 0xF0) >> 4;
        self.set_register(register, new_value, memory);
        self.af.clear_flags();
        self.af.set_zero(new_value == 0);
    }

    fn set_bit(&mut self, position: u8, register: Register, memory: &mut impl MemoryMapped) {
        let value = self.get_register(register, memory);
        let new_value = value | (1 << position);
        self.set_register(register, new_value, memory);
    }

    fn rotate_left(
        &mut self,
        register: Register,
        memory: &mut impl MemoryMapped,
        set_zero_flag: bool,
    ) {
        let value = self.get_register(register, memory);
        let new_value = value << 1 | self.af.carry() as u8;
        self.set_register(register, new_value, memory);
        self.af.clear_flags();
        self.af.set_zero(set_zero_flag && new_value == 0);
        self.af.set_carry(value & 0x80 > 0);
    }

    fn rotate_left_circular(
        &mut self,
        register: Register,
        memory: &mut impl MemoryMapped,
        set_zero_flag: bool,
    ) {
        let value = self.get_register(register, memory);
        let new_value = value.rotate_left(1);
        self.set_register(register, new_value, memory);
        self.af.clear_flags();
        self.af.set_zero(set_zero_flag && new_value == 0);
        self.af.set_carry(value & 0x80 > 0);
    }

    fn rotate_right(
        &mut self,
        register: Register,
        memory: &mut impl MemoryMapped,
        set_zero_flag: bool,
    ) {
        let value = self.get_register(register, memory);
        let new_value = value >> 1 | (self.af.carry() as u8) << 7;
        self.set_register(register, new_value, memory);
        self.af.clear_flags();
        self.af.set_zero(set_zero_flag && new_value == 0);
        self.af.set_carry(value & 0x1 > 0);
    }

    fn rotate_right_circular(
        &mut self,
        register: Register,
        memory: &mut impl MemoryMapped,
        set_zero_flag: bool,
    ) {
        let value = self.get_register(register, memory);
        let new_value = value.rotate_right(1);
        self.set_register(register, new_value, memory);
        self.af.clear_flags();
        self.af.set_zero(set_zero_flag && new_value == 0);
        self.af.set_carry(value & 0x1 > 0);
    }

    fn shift_left(&mut self, register: Register, memory: &mut impl MemoryMapped) {
        let value = self.get_register(register, memory);
        let new_value = value << 1;
        self.set_register(register, new_value, memory);
        self.af.clear_flags();
        self.af.set_zero(new_value == 0);
        self.af.set_carry(value & 0x80 > 0);
    }

    fn shift_right(&mut self, register: Register, memory: &mut impl MemoryMapped) {
        let value = self.get_register(register, memory);
        let new_value = value >> 1 | (value & 0x80);
        self.set_register(register, new_value, memory);
        self.af.clear_flags();
        self.af.set_zero(new_value == 0);
        self.af.set_carry(value & 0x1 > 0);
    }

    fn shift_right_logic(&mut self, register: Register, memory: &mut impl MemoryMapped) {
        let value = self.get_register(register, memory);
        let new_value = value >> 1;
        self.set_register(register, new_value, memory);
        self.af.clear_flags();
        self.af.set_zero(new_value == 0);
        self.af.set_carry(value & 0x1 > 0);
    }

    fn decimal_adjust(&mut self) {
        let mut new_value = self.af.high();
        if self.af.subtract() {
            if self.af.carry() {
                new_value = new_value.wrapping_sub(0x60);
            }
            if self.af.half_carry() {
                new_value = new_value.wrapping_sub(0x6);
            }
        } else {
            if self.af.carry() || new_value > 0x99 {
                new_value = new_value.wrapping_add(0x60);
                self.af.set_carry(true);
            }
            if self.af.half_carry() || (new_value & 0xF) > 0x9 {
                new_value = new_value.wrapping_add(0x6);
            }
        }
        self.af.set_high(new_value);
        self.af.set_zero(new_value == 0);
        self.af.set_half_carry(false);
    }

    fn get_register(&self, register: Register, memory: &impl MemoryMapped) -> u8 {
        match register {
            Register::A => self.af.high(),
            Register::B => self.bc.high(),
            Register::C => self.bc.low(),
            Register::D => self.de.high(),
            Register::E => self.de.low(),
            Register::H => self.hl.high(),
            Register::L => self.hl.low(),
            Register::HLIndirect => memory.read_byte(self.hl.0),
        }
    }

    fn set_register(&mut self, register: Register, value: u8, memory: &mut impl MemoryMapped) {
        match register {
            Register::A => self.af.set_high(value),
            Register::B => self.bc.set_high(value),
            Register::C => self.bc.set_low(value),
            Register::D => self.de.set_high(value),
            Register::E => self.de.set_low(value),
            Register::H => self.hl.set_high(value),
            Register::L => self.hl.set_low(value),
            Register::HLIndirect => memory.write_byte(self.hl.0, value),
        }
    }

    fn get_register_pair(&self, register_pair: instructions::RegisterPair) -> u16 {
        match register_pair {
            instructions::RegisterPair::AF => self.af.0 & 0xFFF0,
            instructions::RegisterPair::BC => self.bc.0,
            instructions::RegisterPair::DE => self.de.0,
            instructions::RegisterPair::HL => self.hl.0,
            instructions::RegisterPair::SP => self.sp,
        }
    }

    fn set_register_pair(&mut self, register_pair: instructions::RegisterPair, value: u16) {
        match register_pair {
            instructions::RegisterPair::AF => self.af.0 = value & 0xFFF0,
            instructions::RegisterPair::BC => self.bc.0 = value,
            instructions::RegisterPair::DE => self.de.0 = value,
            instructions::RegisterPair::HL => self.hl.0 = value,
            instructions::RegisterPair::SP => self.sp = value,
        }
    }
}

impl Default for CPU {
    fn default() -> Self {
        Self::new()
    }
}
