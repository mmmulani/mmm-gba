const NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fs;
use std::num::Wrapping;
use std::panic;

mod math;

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
    SpecialLoadHL,
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
        0x6 => Register::SpecialLoadHL,
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
    LoadHLIntoSP,
    SaveRegister(Register, u16),
    LoadHLInc,
    SaveHLInc,
    LoadHLDec,
    SaveHLDec,
    AddHL(Register, Register),
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
    RLC(Register),
    RRC(Register),
    RL(Register),
    RR(Register),
    SLA(Register),
    SRA(Register),
    Swap(Register),
    SRL(Register),
    Bit(Register, u8),
    Reset(Register, u8),
    Set(Register, u8),
    DAA,
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
            0x09 => (Opcode::AddHL(Register::B, Register::C), 1),
            0x19 => (Opcode::AddHL(Register::D, Register::E), 1),
            0x29 => (Opcode::AddHL(Register::H, Register::L), 1),
            0x39 => (Opcode::AddHL(Register::SPHi, Register::SPLo), 1),
            0x10 => (Opcode::Stop, 1),
            0x27 => (Opcode::DAA, 1),
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
                Opcode::SaveRegister(Register::A, 0xff00 + (immediate8 as u16)),
                2,
            ),
            0xF0 => (
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
                    if right_register == Register::SpecialLoadHL {
                        Opcode::LoadAddressFromRegisters(left_register, Register::H, Register::L)
                    } else if left_register == Register::SpecialLoadHL {
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
                (Opcode::Dec(nth_register((opcode_value & 0x38) >> 3)), 1)
            }
            0x22 => (Opcode::SaveHLInc, 1),
            0x2A => (Opcode::LoadHLInc, 1),
            0x32 => (Opcode::SaveHLDec, 1),
            0x3A => (Opcode::LoadHLDec, 1),
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
            0xF9 => (Opcode::LoadHLIntoSP, 1),
            0x07 => (Opcode::RLC(Register::A), 1),
            0x0F => (Opcode::RRC(Register::A), 1),
            0x17 => (Opcode::RL(Register::A), 1),
            0x1F => (Opcode::RR(Register::A), 1),
            0xCB => (self.cb_opcode(immediate8), 2),
            _ => (Opcode::UnimplementedOpcode(self.content[address]), 1),
        }
    }

    pub fn cb_opcode(&self, value: u8) -> Opcode {
        match value {
            0x00..=0x07 => Opcode::RLC(nth_register(value & 0x7)),
            0x08..=0x0F => Opcode::RRC(nth_register(value & 0x7)),
            0x10..=0x17 => Opcode::RL(nth_register(value & 0x7)),
            0x18..=0x1F => Opcode::RR(nth_register(value & 0x7)),
            0x20..=0x27 => Opcode::SLA(nth_register(value & 0x7)),
            0x28..=0x2F => Opcode::SRA(nth_register(value & 0x7)),
            0x30..=0x37 => Opcode::Swap(nth_register(value & 0x7)),
            0x38..=0x3F => Opcode::SRL(nth_register(value & 0x7)),
            0x40..=0x7F => Opcode::Bit(nth_register(value & 0x7), (value & 0x38) >> 3),
            0x80..=0xBF => Opcode::Reset(nth_register(value & 0x7), (value & 0x38) >> 3),
            0xC0..=0xFF => Opcode::Set(nth_register(value & 0x7), (value & 0x38) >> 3),
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

    pub fn read_rom_bank(&self, _bank: u8, _address: usize) -> u8 {
        panic!("unimplemented");
    }
}

#[derive(Copy, Clone, Debug)]
pub struct RegisterState {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: u8,
    pub h: u8,
    pub l: u8,
}

#[derive(Copy, Clone, Debug)]
pub struct ProgramState {
    pub stack_pointer: u16,
    pub program_counter: u16,
    cycle_count: u64,
    rom_bank: u8,
}

struct Memory {
    video_ram: Vec<u8>,
    work_ram_0: Vec<u8>,
    work_ram_1: Vec<u8>,
    other_ram: Vec<u8>,
    external_ram: Vec<u8>,
    external_ram_enabled: bool,
}

pub struct Interpreter {
    rom: ROM,
    pub register_state: RegisterState,
    pub program_state: ProgramState,
    memory: Memory,
}

impl Interpreter {
    pub fn with_rom(rom: ROM) -> Interpreter {
        if !rom.has_valid_header_checksum() || !rom.has_nintendo_logo() {
            panic!("invalid header");
        }
        let ram_size = rom.ram_size();
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
                rom_bank: 0,
            },
            memory: Memory {
                video_ram: vec![0; 0x2000],
                work_ram_0: vec![0; 0x1000],
                work_ram_1: vec![0; 0x1000],
                other_ram: vec![0; 0x10000],
                external_ram: vec![0; ram_size],
                external_ram_enabled: false,
            },
        }
    }

    pub fn get_next_instructions(&self) -> BTreeMap<u16, Opcode> {
        let mut pc = self.program_state.program_counter;
        let mut result = BTreeMap::new();
        for _i in 0..3 {
            let (opcode, opcode_size) = self.rom.opcode(pc as usize);
            result.insert(pc, opcode);
            pc += opcode_size;
        }
        return result;
    }

    pub fn run_single_instruction(&mut self) -> () {
        let current_pc = self.program_state.program_counter;
        let (opcode, opcode_size) = self.rom.opcode(current_pc as usize);

        // PC should be updated before we actually run the instruction.
        // This matters when you store return pointers on the stack.
        self.program_state.program_counter = current_pc + opcode_size;

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
                self.save_memory(address, value);
            }
            Opcode::Call(address) => jump_location = self.do_call(address),
            Opcode::CallCond(flag, set, address) => {
                if self.get_flag(flag) == set {
                    jump_location = self.do_call(address);
                }
            }
            Opcode::Return => {
                let old_sp = self.program_state.stack_pointer;
                let new_pc: u16 = (self.read_memory(old_sp) as u16)
                    + ((self.read_memory(old_sp + 1) as u16) << 8);
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
                self.save_memory(address, self.get_register_value(from_register));
            }
            Opcode::Push(hi_register, lo_register) => {
                let old_sp = self.program_state.stack_pointer;
                self.save_memory(old_sp - 1, self.get_register_value(hi_register));
                self.save_memory(old_sp - 2, self.get_register_value(lo_register));
                self.program_state.stack_pointer -= 2;
            }
            Opcode::Pop(hi_register, lo_register) => {
                let old_sp = self.program_state.stack_pointer;
                self.handle_save_register(lo_register, self.read_memory(old_sp));
                self.handle_save_register(hi_register, self.read_memory(old_sp + 1));
                self.program_state.stack_pointer += 2;
            }
            Opcode::AddHL(hi_register, lo_register) => {
                let a = self.register_pair_value(Register::H, Register::L);
                let b = self.register_pair_value(hi_register, lo_register);
                let result = math::add16(a, b);
                self.save_register_pair(Register::H, Register::L, result.value);
                self.set_flag(FlagBit::AddSub, result.add_sub.unwrap());
                self.set_flag(FlagBit::Carry, result.carry.unwrap());
                self.set_flag(FlagBit::HalfCarry, result.half_carry.unwrap());
            }
            Opcode::IncPair(hi_register, lo_register) => {
                let value = self
                    .register_pair_value(hi_register, lo_register)
                    .wrapping_add(1);
                self.save_register_pair(hi_register, lo_register, value);
            }
            Opcode::Inc(register) => {
                let old_value = self.get_register_value(register);
                let new_value = old_value.wrapping_add(1);
                self.set_flag(FlagBit::Zero, new_value == 0);
                self.set_flag(FlagBit::AddSub, false);
                self.set_half_carry_add(old_value, 1);
                self.handle_save_register(register, new_value);
            }
            Opcode::DecPair(hi_register, lo_register) => {
                let value = self
                    .register_pair_value(hi_register, lo_register)
                    .wrapping_sub(1);
                self.save_register_pair(hi_register, lo_register, value);
            }
            Opcode::Dec(register) => {
                let old_value = self.get_register_value(register);
                let new_value = old_value.wrapping_sub(1);
                self.set_flag(FlagBit::Zero, new_value == 0);
                self.set_flag(FlagBit::AddSub, true);
                self.set_half_carry_sub(old_value, 1);
                self.handle_save_register(register, new_value);
            }
            Opcode::LoadHLInc => {
                let address = self.register_pair_value(Register::H, Register::L);
                self.handle_save_register(Register::A, self.load_address(address));
                self.save_register_pair(Register::H, Register::L, address + 1);
            }
            Opcode::SaveHLInc => {
                let address = self.register_pair_value(Register::H, Register::L);
                self.save_memory(address, self.get_register_value(Register::A));
                self.save_register_pair(Register::H, Register::L, address + 1);
            }
            Opcode::LoadHLDec => {
                let address = self.register_pair_value(Register::H, Register::L);
                self.handle_save_register(Register::A, self.load_address(address));
                self.save_register_pair(Register::H, Register::L, address - 1);
            }
            Opcode::SaveHLDec => {
                let address = self.register_pair_value(Register::H, Register::L);
                self.save_memory(address, self.get_register_value(Register::A));
                self.save_register_pair(Register::H, Register::L, address - 1);
            }
            Opcode::LoadHLIntoSP => {
                let address = self.register_pair_value(Register::H, Register::L);
                self.program_state.stack_pointer = address;
            }
            Opcode::DAA => {
                let result = math::daa(
                    self.get_register_value(Register::A),
                    self.get_flag(FlagBit::Carry),
                    self.get_flag(FlagBit::HalfCarry),
                    self.get_flag(FlagBit::AddSub),
                );
                self.handle_save_register(Register::A, result.value);
                self.apply_math_result_flags(result);
            }
            Opcode::And(register) => self.do_math_reg(register, math::and),
            Opcode::Or(register) => self.do_math_reg(register, math::or),
            Opcode::Xor(register) => self.do_math_reg(register, math::xor),
            Opcode::AndValue(value) => self.do_math(value, math::and),
            Opcode::OrValue(value) => self.do_math(value, math::or),
            Opcode::XorValue(value) => self.do_math(value, math::xor),
            Opcode::AddValue(value) => self.do_math(value, math::add),
            Opcode::AddCarryValue(value) => self.do_math_carry(value, math::adc),
            Opcode::SubValue(value) => self.do_math(value, math::sub),
            Opcode::CpValue(value) => self.do_math(value, math::cp),
            Opcode::Cp(register) => self.do_math_reg(register, math::cp),
            Opcode::Add(register) => self.do_math_reg(register, math::add),
            Opcode::AddCarry(register) => self.do_math_carry_reg(register, math::adc),
            Opcode::Sub(register) => self.do_math_reg(register, math::sub),
            Opcode::RLC(register) => self.do_bit_op(register, math::rlc),
            Opcode::RRC(register) => self.do_bit_op(register, math::rrc),
            Opcode::RR(register) => self.do_bit_op_carry(register, math::rr),
            Opcode::RL(register) => self.do_bit_op_carry(register, math::rl),
            Opcode::SLA(register) => self.do_bit_op(register, math::sla),
            Opcode::SRA(register) => self.do_bit_op(register, math::sra),
            Opcode::SRL(register) => self.do_bit_op(register, math::srl),
            Opcode::Swap(register) => self.do_bit_op(register, math::swap),
            Opcode::Bit(register, pos) => {
                let value = self.get_register_value(register);
                let set = (value & (0x1 << pos)) != 0x0;
                self.set_flag(FlagBit::Zero, !set);
                self.set_flag(FlagBit::AddSub, false);
                self.set_flag(FlagBit::HalfCarry, true);
            }
            Opcode::Set(register, pos) => {
                let value = self.get_register_value(register);
                let new_val = value | (0x1 << pos);
                self.handle_save_register(register, new_val);
            }
            Opcode::Reset(register, pos) => {
                let value = self.get_register_value(register);
                let new_val = value & !(0x1 << pos);
                self.handle_save_register(register, new_val);
            }
            _ => {
                println!("unhandled opcode {:X?}", opcode);
                panic!();
            }
        }
        if jump_location.is_some() {
            self.program_state.program_counter = jump_location.unwrap();
        }

        self.program_state.cycle_count += 1;
    }

    fn do_math_reg(&mut self, register: Register, f: fn(u8, u8) -> math::Result) -> () {
        self.do_math(self.get_register_value(register), f);
    }

    fn do_bit_op(&mut self, register: Register, f: fn(u8) -> math::Result) -> () {
        let a = self.get_register_value(register);
        let result = f(a);
        self.handle_save_register(register, result.value);
        self.apply_math_result_flags(result);
    }

    fn do_bit_op_carry(&mut self, register: Register, f: fn(u8, bool) -> math::Result) -> () {
        let a = self.get_register_value(register);
        let result = f(a, self.get_flag(FlagBit::Carry));
        self.handle_save_register(register, result.value);
        self.apply_math_result_flags(result);
    }

    fn do_math_carry_reg(&mut self, register: Register, f: fn(u8, u8, bool) -> math::Result) -> () {
        self.do_math_carry(self.get_register_value(register), f);
    }

    fn do_math_carry(&mut self, value: u8, f: fn(u8, u8, bool) -> math::Result) -> () {
        let a = self.get_register_value(Register::A);
        let result = f(a, value, self.get_flag(FlagBit::Carry));
        self.handle_save_register(Register::A, result.value);
        self.apply_math_result_flags(result);
    }

    fn do_math(&mut self, value: u8, f: fn(u8, u8) -> math::Result) -> () {
        let a = self.get_register_value(Register::A);
        let result = f(a, value);
        self.handle_save_register(Register::A, result.value);
        self.apply_math_result_flags(result);
    }

    fn apply_math_result_flags(&mut self, result: math::Result) {
        if result.zero.is_some() {
            self.set_flag(FlagBit::Zero, result.zero.unwrap());
        }
        if result.add_sub.is_some() {
            self.set_flag(FlagBit::AddSub, result.add_sub.unwrap());
        }
        if result.half_carry.is_some() {
            self.set_flag(FlagBit::HalfCarry, result.half_carry.unwrap());
        }
        if result.carry.is_some() {
            self.set_flag(FlagBit::Carry, result.carry.unwrap());
        }
    }

    fn do_call(&mut self, address: u16) -> Option<u16> {
        let old_pc = self.program_state.program_counter;
        let old_sp = self.program_state.stack_pointer;
        self.save_memory(old_sp - 1, ((old_pc & 0xff00) >> 8) as u8);
        self.save_memory(old_sp - 2, (old_pc & 0x00ff) as u8);
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
            Register::SPHi => ((self.program_state.stack_pointer & 0xff00) >> 8) as u8,
            Register::SPLo => (self.program_state.stack_pointer & 0x00ff) as u8,
            Register::PCHi => ((self.program_state.program_counter & 0xff00) >> 8) as u8,
            Register::PCLo => (self.program_state.program_counter & 0x00ff) as u8,
            Register::SpecialLoadHL => {
                self.read_memory(self.register_pair_value(Register::H, Register::L))
            }
            Register::Empty => {
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
            Register::SPHi
            | Register::SPLo
            | Register::PCHi
            | Register::PCLo
            | Register::SpecialLoadHL
            | Register::Empty => panic!("unhandled save register"),
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

    fn read_memory(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x3FFF => self.rom.read_rom(address as usize),
            0x4000..=0x7FFF => self
                .rom
                .read_rom_bank(self.program_state.rom_bank, address as usize),
            0x8000..=0x9FFF => self.memory.video_ram[(address - 0x8000) as usize],
            0xA000..=0xBFFF => self.memory.external_ram[(address - 0xA000) as usize],
            0xC000..=0xCFFF => self.memory.work_ram_0[(address - 0xC000) as usize],
            0xD000..=0xDFFF => self.memory.work_ram_1[(address - 0xD000) as usize],
            0xE000..=0xFDFF => panic!("unimplemented echo memory"),
            0xFE00..=0xFFFF => self.memory.other_ram[address as usize],
        }
    }

    fn save_memory(&mut self, address: u16, value: u8) -> () {
        match address {
            0xFF70 => panic!("writing to ram bank switcher"),
            0x0000..=0x7FFF => panic!("writing to rom"),
            0x8000..=0x9FFF => self.memory.video_ram[(address - 0x8000) as usize] = value,
            0xA000..=0xBFFF => {
                if self.memory.external_ram_enabled
                    && self.rom.cartridge_type() != MemoryBankType::ROM
                {
                    self.memory.external_ram[(address - 0xA000) as usize] = value
                } else {
                    panic!("external ram disabled")
                }
            }
            0xC000..=0xCFFF => self.memory.work_ram_0[(address - 0xC000) as usize] = value,
            0xD000..=0xDFFF => self.memory.work_ram_1[(address - 0xD000) as usize] = value,
            0xE000..=0xFDFF => panic!("unimplemented echo memory"),
            0xFE00..=0xFFFF => self.memory.other_ram[address as usize] = value,
        }
    }

    fn set_half_carry_add(&mut self, a: u8, b: u8) -> () {
        self.set_flag(FlagBit::HalfCarry, (((a & 0xf) + (b & 0xf)) & 0x10) == 0x10)
    }

    fn set_half_carry_sub(&mut self, a: u8, b: u8) -> () {
        self.set_flag(FlagBit::HalfCarry, (a & 0xf) < (b & 0xf))
    }

    fn load_address(&self, address: u16) -> u8 {
        self.rom.read_rom(address as usize)
    }

    pub fn run_program(&mut self) -> () {
        let result = panic::catch_unwind(panic::AssertUnwindSafe(|| loop {
            self.run_single_instruction();
        }));
        if result.is_err() {
            println!("registers: {:X?}", self.register_state);
            println!("program state: {:X?}", self.program_state);
        }
    }
}
