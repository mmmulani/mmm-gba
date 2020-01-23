const NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

use std::cmp::Ordering;
use std::fs;
use std::num::Wrapping;

#[derive(Debug, PartialEq, Eq)]
enum Register {
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
    Inc(Register, Register),
    Dec(Register, Register),
    Jump(u16),
    JumpRelative(i8),
    DisableInterrupts,
    EnableInterrupts,
    Push(Register, Register),
    Pop(Register, Register),
    Call(u16),
    Return,
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
        let immediate16 = self.read_u16(address + 2);
        let opcode_value = self.content[address];
        match opcode_value {
            0x0 => (Opcode::Noop, 1),
            0xC3 => (Opcode::Jump(immediate16), 3),
            0x18 => (Opcode::JumpRelative(immediate8 as i8), 2),
            0x01 => (Opcode::Load16(Register::B, Register::C, immediate16), 3),
            0x11 => (Opcode::Load16(Register::D, Register::E, immediate16), 3),
            0x21 => (Opcode::Load16(Register::H, Register::L, immediate16), 3),
            0x31 => (
                Opcode::Load16(Register::SPHi, Register::SPLo, immediate16),
                3,
            ),
            0x3E => (Opcode::Load8(Register::A, immediate8), 2),
            0xF3 => (Opcode::DisableInterrupts, 1),
            0xFB => (Opcode::EnableInterrupts, 1),
            0xEA => (Opcode::LoadAddress(Register::A, immediate16), 3),
            0xE0 => (
                Opcode::LoadAddress(Register::A, 0xff00 + (immediate8 as u16)),
                2,
            ),
            0xCD => (Opcode::Call(immediate16), 3),
            0x76 => (Opcode::Halt, 1),
            0x40..=0x7F => (
                {
                    let right_register = match opcode_value & 0x7 {
                        0x0 => Register::B,
                        0x1 => Register::C,
                        0x2 => Register::D,
                        0x3 => Register::E,
                        0x4 => Register::H,
                        0x5 => Register::L,
                        0x7 => Register::A,
                        _ => Register::Empty,
                    };
                    let left_register = match (opcode_value & 0x38) >> 3 {
                        0x0 => Register::B,
                        0x1 => Register::C,
                        0x2 => Register::D,
                        0x3 => Register::E,
                        0x4 => Register::H,
                        0x5 => Register::L,
                        0x7 => Register::A,
                        _ => Register::Empty,
                    };
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

        println!("At 0x{:x}, got opcode: {:?}", current_pc, opcode);
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
            Opcode::DisableInterrupts => (),
            Opcode::EnableInterrupts => (),
            Opcode::Load8(register, value) => {
                self.handle_save_register(register, value);
            }
            Opcode::Load16(hi_register, lo_register, value) => {
                self.handle_load16(hi_register, lo_register, value);
            }
            Opcode::LoadAddress(register, address) => {
                let value = self.load_address(address);
                self.handle_save_register(register, value);
            }
            Opcode::Call(address) => {
                jump_location = Some(address);
                let old_pc = self.program_state.program_counter;
                let old_sp = self.program_state.stack_pointer;
                self.memory[(old_sp - 1) as usize] = ((old_pc & 0xff00) >> 8) as u8;
                self.memory[(old_sp - 2) as usize] = (old_pc & 0x00ff) as u8;
                self.program_state.stack_pointer = old_sp - 2;
            }
            Opcode::LoadReg(to_register, from_register) => {
                self.handle_save_register(to_register, self.get_register_value(from_register));
            }
            Opcode::LoadAddressFromRegisters(to_register, hi_addr, lo_addr) => {
                let address = self.register_pair_to_address(hi_addr, lo_addr);
                self.handle_save_register(to_register, self.load_address(address));
            }
            Opcode::LoadRegisterIntoMemory(from_register, hi_addr, lo_addr) => {
                let address = self.register_pair_to_address(hi_addr, lo_addr);
                self.memory[address as usize] = self.get_register_value(from_register);
            }
            _ => {
                println!("unhandled opcode {:?}", opcode);
                panic!();
            }
        }
        if jump_location.is_some() {
            self.program_state.program_counter = jump_location.unwrap()
        }
    }

    fn register_pair_to_address(&self, hi_register: Register, lo_register: Register) -> u16 {
        let hi_addr = self.get_register_value(hi_register);
        let lo_addr = self.get_register_value(lo_register);
        ((hi_addr as u16) << 8) + (lo_addr as u16)
    }

    fn handle_load16(&mut self, hi_register: Register, lo_register: Register, value: u16) -> () {
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

    fn load_address(&self, address: u16) -> u8 {
        self.rom.read_rom(address as usize)
    }

    pub fn run_program(&mut self) -> () {
        loop {
            self.run_single_instruction();
        }
    }
}
