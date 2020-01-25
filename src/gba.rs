const NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

use std::cmp::Ordering;
use std::fs;
use std::num::Wrapping;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Register {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
    SPHi,
    SPLo,
    PCHi,
    PCLo,
    Empty,
}

fn nth_register(nth: u8) -> Register {
    match nth {
        0x0 => Register::B,
        0x1 => Register::C,
        0x2 => Register::D,
        0x3 => Register::E,
        0x4 => Register::H,
        0x5 => Register::L,
        0x7 => Register::A,
        _ => Register::Empty,
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FlagBit {
    Zero,
    AddSub,
    HalfCarry,
    Carry,
}

fn flag_picker(flag: FlagBit) -> u8 {
    match flag {
        FlagBit::Zero => 1 << 7,
        FlagBit::AddSub => 1 << 6,
        FlagBit::HalfCarry => 1 << 5,
        FlagBit::Carry => 1 << 4,
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
    Noop,
    Stop,
    Halt,
    Load16(Register, Register, u16),
    Load8(Register, u8),
    LoadReg(Register, Register),
    LoadAddress(Register, u16),
    LoadAddressFromRegisters(Register, Register, Register),
    LoadRegisterIntoMemory(Register, Register, Register),
    SaveRegister(Register, u16),
    LoadHLInc(),
    SaveHLInc(),
    LoadHLDec(),
    SaveHLDec(),
    Inc(Register),
    IncPair(Register, Register),
    Dec(Register),
    DecPair(Register, Register),
    Jump(u16),
    JumpRelative(i8),
    JumpRelativeCond(FlagBit, bool, i8),
    DisableInterrupts,
    EnableInterrupts,
    Push(Register, Register),
    Pop(Register, Register),
    Call(u16),
    CallCond(FlagBit, bool, u16),
    Return,
    Or(Register),
    And(Register),
    Xor(Register),
    Cp(Register),
    Add(Register),
    AddCarry(Register),
    Sub(Register),
    SubCarry(Register),
    AndValue(u8),
    OrValue(u8),
    XorValue(u8),
    CpValue(u8),
    AddValue(u8),
    AddCarryValue(u8),
    SubValue(u8),
    SubCarryValue(u8),
    UnimplementedOpcode(u8),
}

#[derive(Debug, PartialEq, Eq)]
pub enum MemoryBankType {
    ROM,
    MBC1,
    MBC2,
    MMM01,
    MBC3,
    MBC4,
    MBC5,
}

pub struct ROM {
    content: Vec<u8>,
}

impl ROM {
    pub fn from_path(filename: &str) -> ROM {
        ROM {
            content: match fs::read(filename) {
                Ok(bytes) => bytes,
                Err(_e) => vec![],
            },
        }
    }

    pub fn from_bytes(bytes: Vec<u8>) -> ROM {
        ROM { content: bytes }
    }

    pub fn title(&self) -> String {
        let title: Vec<u8> = self.content[0x134..0x144]
            .iter()
            .take_while(|&&v| v > 0)
            .cloned()
            .collect();
        let str = String::from_utf8(title).unwrap();
        str
    }

    pub fn bytes(&self, address: usize, length: usize) -> &[u8] {
        &self.content[address..(address + length)]
    }

    pub fn opcode(&self, address: usize) -> (Opcode, u16) {
        let immediate8 = self.read_u8(address + 1);
        let relative8 = immediate8 as i8;
        let immediate16 = self.read_u16(address + 2);
        let opcode_value = self.content[address];
        match opcode_value {
            0x0 => (Opcode::Noop, 1),
            0xC3 => (Opcode::Jump(immediate16), 3),
            0x18 => (Opcode::JumpRelative(relative8), 2),
            0x20 => (Opcode::JumpRelativeCond(FlagBit::Zero, false, relative8), 2),
            0x28 => (Opcode::JumpRelativeCond(FlagBit::Zero, true, relative8), 2),
            0x30 => (
                Opcode::JumpRelativeCond(FlagBit::Carry, false, relative8),
                2,
            ),
            0x38 => (Opcode::JumpRelativeCond(FlagBit::Carry, true, relative8), 2),
            0x01 => (Opcode::Load16(Register::B, Register::C, immediate16), 3),
            0x11 => (Opcode::Load16(Register::D, Register::E, immediate16), 3),
            0x21 => (Opcode::Load16(Register::H, Register::L, immediate16), 3),
            0x31 => (
                Opcode::Load16(Register::SPHi, Register::SPLo, immediate16),
                3,
            ),
            0x06 | 0x0E | 0x16 | 0x1E | 0x26 | 0x2E | 0x36 | 0x3E => (
                Opcode::Load8(nth_register((opcode_value & 0x38) >> 3), immediate8),
                2,
            ),
            0xF3 => (Opcode::DisableInterrupts, 1),
            0xFB => (Opcode::EnableInterrupts, 1),
            0xEA => (Opcode::SaveRegister(Register::A, immediate16), 3),
            0xFA => (Opcode::LoadAddress(Register::A, immediate16), 3),
            0xE0 => (
                Opcode::LoadAddress(Register::A, 0xff00 + (immediate8 as u16)),
                2,
            ),
            0xC9 => (Opcode::Return, 1),
            0xCD => (Opcode::Call(immediate16), 3),
            0xC4 => (Opcode::CallCond(FlagBit::Zero, false, immediate16), 3),
            0xCC => (Opcode::CallCond(FlagBit::Zero, true, immediate16), 3),
            0xD4 => (Opcode::CallCond(FlagBit::Carry, false, immediate16), 3),
            0xDC => (Opcode::CallCond(FlagBit::Carry, true, immediate16), 3),
            0x76 => (Opcode::Halt, 1),
            0x0A => (
                Opcode::LoadAddressFromRegisters(Register::A, Register::B, Register::C),
                1,
            ),
            0x1A => (
                Opcode::LoadAddressFromRegisters(Register::A, Register::D, Register::E),
                1,
            ),
            0x40..=0x7F => (
                {
                    let right_register = nth_register(opcode_value & 0x7);
                    let left_register = nth_register((opcode_value & 0x38) >> 3);
                    if right_register == Register::Empty {
                        Opcode::LoadAddressFromRegisters(left_register, Register::H, Register::L)
                    } else if left_register == Register::Empty {
                        Opcode::LoadRegisterIntoMemory(right_register, Register::H, Register::L)
                    } else {
                        Opcode::LoadReg(left_register, right_register)
                    }
                },
                1,
            ),
            0xC5 => (Opcode::Push(Register::B, Register::C), 1),
            0xC1 => (Opcode::Pop(Register::B, Register::C), 1),
            0xD5 => (Opcode::Push(Register::D, Register::E), 1),
            0xD1 => (Opcode::Pop(Register::D, Register::E), 1),
            0xE5 => (Opcode::Push(Register::H, Register::L), 1),
            0xE1 => (Opcode::Pop(Register::H, Register::L), 1),
            0xF5 => (Opcode::Push(Register::A, Register::F), 1),
            0xF1 => (Opcode::Pop(Register::A, Register::F), 1),
            0x03 => (Opcode::IncPair(Register::B, Register::C), 1),
            0x13 => (Opcode::IncPair(Register::D, Register::E), 1),
            0x23 => (Opcode::IncPair(Register::H, Register::L), 1),
            0x33 => (Opcode::IncPair(Register::SPHi, Register::SPLo), 1),
            0x0B => (Opcode::DecPair(Register::B, Register::C), 1),
            0x1B => (Opcode::DecPair(Register::D, Register::E), 1),
            0x2B => (Opcode::DecPair(Register::H, Register::L), 1),
            0x3B => (Opcode::DecPair(Register::SPHi, Register::SPLo), 1),
            0x04 | 0x0C | 0x14 | 0x1C | 0x24 | 0x2C | 0x34 | 0x3C => {
                (Opcode::Inc(nth_register((opcode_value & 0x38) >> 3)), 1)
            }
            0x05 | 0x0D | 0x15 | 0x1D | 0x25 | 0x2D | 0x35 | 0x3D => {
                (Opcode::Inc(nth_register((opcode_value & 0x38) >> 3)), 1)
            }
            0x22 => (Opcode::SaveHLInc(), 1),
            0x2A => (Opcode::LoadHLInc(), 1),
            0x32 => (Opcode::SaveHLDec(), 1),
            0x3A => (Opcode::LoadHLDec(), 1),
            0xA0..=0xA7 => (Opcode::And(nth_register(opcode_value & 0x7)), 1),
            0xA8..=0xAF => (Opcode::Xor(nth_register(opcode_value & 0x7)), 1),
            0xB0..=0xB7 => (Opcode::Or(nth_register(opcode_value & 0x7)), 1),
            0xB8..=0xBF => (Opcode::Cp(nth_register(opcode_value & 0x7)), 1),
            0xE6 => (Opcode::AndValue(immediate8), 2),
            0xEE => (Opcode::XorValue(immediate8), 2),
            0xF6 => (Opcode::OrValue(immediate8), 2),
            0xFE => (Opcode::CpValue(immediate8), 2),
            0x80..=0x87 => (Opcode::Add(nth_register(opcode_value & 0x7)), 1),
            0x88..=0x8F => (Opcode::AddCarry(nth_register(opcode_value & 0x7)), 1),
            0x90..=0x97 => (Opcode::Sub(nth_register(opcode_value & 0x7)), 1),
            0x98..=0x9F => (Opcode::SubCarry(nth_register(opcode_value & 0x7)), 1),
            0xC6 => (Opcode::AddValue(immediate8), 2),
            0xCE => (Opcode::AddCarryValue(immediate8), 2),
            0xD6 => (Opcode::SubValue(immediate8), 2),
            0xDE => (Opcode::SubCarryValue(immediate8), 2),
            _ => (Opcode::UnimplementedOpcode(self.content[address]), 1),
        }
    }

    pub fn has_nintendo_logo(&self) -> bool {
        self.content[0x104..0x134].iter().cmp(NINTENDO_LOGO.iter()) == Ordering::Equal
    }

    pub fn has_valid_header_checksum(&self) -> bool {
        let checksum: Wrapping<u8> = self.content[0x134..0x14D]
            .iter()
            .cloned()
            .map(|v| Wrapping(v))
            .fold(Wrapping(0), |acc, v| acc - v - Wrapping(1));
        checksum.0 == self.content[0x14D]
    }

    pub fn cartridge_type(&self) -> MemoryBankType {
        match self.content[0x147] {
            0x00 | 0x08..=0x09 => MemoryBankType::ROM,
            0x01..=0x03 => MemoryBankType::MBC1,
            0x05..=0x06 => MemoryBankType::MBC2,
            0x0B..=0x0D => MemoryBankType::MMM01,
            0x0F..=0x13 => MemoryBankType::MBC3,
            0x15..=0x17 => MemoryBankType::MBC4,
            0x19..=0x1E => MemoryBankType::MBC5,
            _ => panic!("unknown memory bank type"),
        }
    }

    pub fn ram_size(&self) -> usize {
        match self.content[0x149] {
            0x00 => 0,
            0x01 => 2 * 1024,
            0x02 => 8 * 1024,
            0x03 => 32 * 1024,
            _ => panic!("unknown ram size"),
        }
    }

    fn read_u8(&self, address: usize) -> u8 {
        self.content[address]
    }

    fn read_u16(&self, address: usize) -> u16 {
        ((self.content[address] as u16) << 8) + (self.content[address - 1] as u16)
    }

    pub fn read_rom(&self, address: usize) -> u8 {
        self.content[address]
    }
}

struct RegisterState {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
}

struct ProgramState {
    stack_pointer: u16,
    program_counter: u16,
    cycle_count: u64,
}

pub struct Interpreter {
    rom: ROM,
    register_state: RegisterState,
    program_state: ProgramState,
    memory: Vec<u8>,
}

impl Interpreter {
    pub fn with_rom(rom: ROM) -> Interpreter {
        Interpreter {
            rom,
            register_state: RegisterState {
                a: 0x01,
                b: 0x00,
                c: 0x13,
                d: 0x00,
                e: 0xd8,
                f: 0xb0,
                h: 0x01,
                l: 0x4d,
            },
            program_state: ProgramState {
                stack_pointer: 0xfffe,
                program_counter: 0x100,
                cycle_count: 0,
            },
            memory: vec![0; 0x10000],
        }
    }

    fn run_single_instruction(&mut self) -> () {
        let current_pc = self.program_state.program_counter;
        let (opcode, opcode_size) = self.rom.opcode(current_pc as usize);

        // PC should be updated before we actually run the instruction.
        // This matters when you store return pointers on the stack.
        self.program_state.program_counter = current_pc + opcode_size;

        println!(
            "{}: 0x{:x}, got opcode: {:?}",
            self.program_state.cycle_count, current_pc, opcode
        );
        let mut jump_location: Option<u16> = None;
        match opcode {
            Opcode::Noop => (),
            Opcode::Jump(address) => jump_location = Some(address),
            Opcode::JumpRelative(relative_address) => {
                jump_location = Some(
                    ((self.program_state.program_counter as i32) + (relative_address as i32))
                        as u16,
                );
            }
            Opcode::JumpRelativeCond(flag, set, relative_address) => {
                if self.get_flag(flag) == set {
                    jump_location = Some(
                        ((self.program_state.program_counter as i32) + (relative_address as i32))
                            as u16,
                    );
                }
            }
            Opcode::DisableInterrupts => (),
            Opcode::EnableInterrupts => (),
            Opcode::Load8(register, value) => {
                self.handle_save_register(register, value);
            }
            Opcode::Load16(hi_register, lo_register, value) => {
                self.save_register_pair(hi_register, lo_register, value);
            }
            Opcode::LoadAddress(register, address) => {
                let value = self.load_address(address);
                self.handle_save_register(register, value);
            }
            Opcode::SaveRegister(register, address) => {
                let value = self.get_register_value(register);
                self.memory[address as usize] = value;
            }
            Opcode::Call(address) => jump_location = self.do_call(address),
            Opcode::CallCond(flag, set, address) => {
                if self.get_flag(flag) == set {
                    jump_location = self.do_call(address);
                }
            }
            Opcode::Return => {
                let old_sp = self.program_state.stack_pointer as usize;
                let new_pc: u16 =
                    (self.memory[old_sp] as u16) + ((self.memory[old_sp + 1] as u16) << 8);
                self.program_state.stack_pointer = self.program_state.stack_pointer + 2;
                jump_location = Some(new_pc);
            }
            Opcode::LoadReg(to_register, from_register) => {
                self.handle_save_register(to_register, self.get_register_value(from_register));
            }
            Opcode::LoadAddressFromRegisters(to_register, hi_addr, lo_addr) => {
                let address = self.register_pair_value(hi_addr, lo_addr);
                self.handle_save_register(to_register, self.load_address(address));
            }
            Opcode::LoadRegisterIntoMemory(from_register, hi_addr, lo_addr) => {
                let address = self.register_pair_value(hi_addr, lo_addr);
                self.memory[address as usize] = self.get_register_value(from_register);
            }
            Opcode::Push(hi_register, lo_register) => {
                let old_sp = self.program_state.stack_pointer as usize;
                self.memory[old_sp - 1] = self.get_register_value(hi_register);
                self.memory[old_sp - 2] = self.get_register_value(lo_register);
                self.program_state.stack_pointer -= 2;
            }
            Opcode::Pop(hi_register, lo_register) => {
                let old_sp = self.program_state.stack_pointer as usize;
                self.handle_save_register(lo_register, self.memory[old_sp]);
                self.handle_save_register(hi_register, self.memory[old_sp + 1]);
                self.program_state.stack_pointer += 2;
            }
            Opcode::IncPair(hi_register, lo_register) => {
                let value = self.register_pair_value(hi_register, lo_register) + 1;
                self.save_register_pair(hi_register, lo_register, value);
            }
            Opcode::Inc(register) => {
                let old_value = self.get_register_value(register);
                let new_value = old_value + 1;
                if new_value == 0 {
                    self.set_flag(FlagBit::Zero, true);
                }
                self.set_flag(FlagBit::AddSub, false);
                self.set_half_carry_add(old_value, 1);
                self.handle_save_register(register, new_value);
            }
            Opcode::Dec(register) => {
                let old_value = self.get_register_value(register);
                let new_value = old_value - 1;
                if new_value == 0 {
                    self.set_flag(FlagBit::Zero, true);
                }
                self.set_flag(FlagBit::AddSub, true);
                self.set_half_carry_sub(old_value, 1);
                self.handle_save_register(register, new_value);
            }
            Opcode::LoadHLInc() => {
                let address = self.register_pair_value(Register::H, Register::L);
                self.handle_save_register(Register::A, self.load_address(address));
                self.save_register_pair(Register::H, Register::L, address + 1);
            }
            Opcode::SaveHLInc() => {
                let address = self.register_pair_value(Register::H, Register::L);
                self.memory[address as usize] = self.get_register_value(Register::A);
                self.save_register_pair(Register::H, Register::L, address + 1);
            }
            Opcode::And(register) => self.do_and(self.get_register_value(register)),
            Opcode::Or(register) => self.do_or(self.get_register_value(register), false),
            Opcode::Xor(register) => self.do_or(self.get_register_value(register), true),
            Opcode::AndValue(value) => self.do_and(value),
            Opcode::OrValue(value) => self.do_or(value, false),
            Opcode::XorValue(value) => self.do_or(value, true),
            Opcode::AddValue(value) => self.do_add(value),
            Opcode::SubValue(value) => self.do_sub(value),
            Opcode::Add(register) => self.do_add(self.get_register_value(register)),
            Opcode::Sub(register) => self.do_sub(self.get_register_value(register)),
            _ => {
                println!("unhandled opcode {:?}", opcode);
                panic!();
            }
        }
        if jump_location.is_some() {
            self.program_state.program_counter = jump_location.unwrap();
        }

        self.program_state.cycle_count += 1;
    }

    fn do_and(&mut self, value: u8) -> () {
        let new_value = self.get_register_value(Register::A) & value;
        self.handle_save_register(Register::A, new_value);
        if new_value == 0 {
            self.set_flag(FlagBit::Zero, true);
        }
        self.set_flag(FlagBit::AddSub, false);
        self.set_flag(FlagBit::HalfCarry, true);
        self.set_flag(FlagBit::Carry, false);
    }

    fn do_or(&mut self, value: u8, xor: bool) -> () {
        let new_value = if xor {
            self.get_register_value(Register::A) ^ value
        } else {
            self.get_register_value(Register::A) | value
        };
        self.handle_save_register(Register::A, new_value);
        if new_value == 0 {
            self.set_flag(FlagBit::Zero, true);
        }
        self.set_flag(FlagBit::AddSub, false);
        self.set_flag(FlagBit::HalfCarry, false);
        self.set_flag(FlagBit::Carry, false);
    }

    fn do_add(&mut self, value: u8) -> () {
        let old_reg_value = self.get_register_value(Register::A);
        let (new_value, did_overflow) = old_reg_value.overflowing_add(value);
        self.handle_save_register(Register::A, new_value);
        if new_value == 0 {
            self.set_flag(FlagBit::Zero, true);
        }
        self.set_flag(FlagBit::AddSub, false);
        self.set_half_carry_add(old_reg_value, value);
        self.set_flag(FlagBit::Carry, did_overflow);
    }

    fn do_sub(&mut self, value: u8) -> () {
        let old_reg_value = self.get_register_value(Register::A);
        let (new_value, did_overflow) = old_reg_value.overflowing_sub(value);
        self.handle_save_register(Register::A, new_value);
        if new_value == 0 {
            self.set_flag(FlagBit::Zero, true);
        }
        self.set_flag(FlagBit::AddSub, true);
        self.set_half_carry_sub(old_reg_value, value);
        self.set_flag(FlagBit::Carry, did_overflow);
    }

    fn do_call(&mut self, address: u16) -> Option<u16> {
        let old_pc = self.program_state.program_counter;
        let old_sp = self.program_state.stack_pointer;
        self.memory[(old_sp - 1) as usize] = ((old_pc & 0xff00) >> 8) as u8;
        self.memory[(old_sp - 2) as usize] = (old_pc & 0x00ff) as u8;
        self.program_state.stack_pointer = old_sp - 2;
        Some(address)
    }

    fn register_pair_value(&self, hi_register: Register, lo_register: Register) -> u16 {
        let hi_addr = self.get_register_value(hi_register);
        let lo_addr = self.get_register_value(lo_register);
        ((hi_addr as u16) << 8) + (lo_addr as u16)
    }

    fn save_register_pair(
        &mut self,
        hi_register: Register,
        lo_register: Register,
        value: u16,
    ) -> () {
        if hi_register == Register::SPHi && lo_register == Register::SPLo {
            self.program_state.stack_pointer = value;
            return;
        }

        self.handle_save_register(hi_register, ((value & 0xff00) >> 8) as u8);
        self.handle_save_register(lo_register, (value & 0xff) as u8);
    }

    fn get_register_value(&self, register: Register) -> u8 {
        match register {
            Register::A => self.register_state.a,
            Register::B => self.register_state.b,
            Register::C => self.register_state.c,
            Register::D => self.register_state.d,
            Register::E => self.register_state.e,
            Register::F => self.register_state.f,
            Register::H => self.register_state.h,
            Register::L => self.register_state.l,
            Register::SPHi | Register::SPLo | Register::PCHi | Register::PCLo | Register::Empty => {
                panic!("unhandled get register value");
            }
        }
    }

    fn handle_save_register(&mut self, register: Register, value: u8) -> () {
        let field = match register {
            Register::A => &mut self.register_state.a,
            Register::B => &mut self.register_state.b,
            Register::C => &mut self.register_state.c,
            Register::D => &mut self.register_state.d,
            Register::E => &mut self.register_state.e,
            Register::F => &mut self.register_state.f,
            Register::H => &mut self.register_state.h,
            Register::L => &mut self.register_state.l,
            Register::SPHi | Register::SPLo | Register::PCHi | Register::PCLo | Register::Empty => {
                panic!("unhandled save register")
            }
        };
        *field = value;
    }

    fn set_flag(&mut self, flag: FlagBit, set: bool) -> () {
        let flag_picker = flag_picker(flag);
        let old_value = self.get_register_value(Register::F);
        let new_value = if set {
            old_value | flag_picker
        } else {
            old_value & (!flag_picker)
        };
        self.handle_save_register(Register::F, new_value);
    }

    fn get_flag(&self, flag: FlagBit) -> bool {
        self.get_register_value(Register::F) & flag_picker(flag) != 0
    }

    fn set_half_carry_add(&mut self, a: u8, b: u8) -> () {
        self.set_flag(FlagBit::HalfCarry, (((a & 0xf) + (b & 0xf)) & 0x10) == 0x10)
    }

    fn set_half_carry_sub(&mut self, a: u8, b: u8) -> () {
        self.set_flag(FlagBit::HalfCarry, (a & 0xf) < (b & 0xf))
    }

    fn set_half_carry_add16(&mut self, a: u16, b: u16) -> () {
        self.set_flag(
            FlagBit::HalfCarry,
            (((a & 0xff) + (b & 0xff)) & 0x100) == 0x100,
        )
    }

    fn set_half_carry_sub16(&mut self, a: u16, b: u16) -> () {
        self.set_flag(FlagBit::HalfCarry, (a & 0xff) < (b & 0xff))
    }

    fn load_address(&self, address: u16) -> u8 {
        self.rom.read_rom(address as usize)
    }

    pub fn run_program(&mut self) -> () {
        loop {
            self.run_single_instruction();
        }
    }
}
