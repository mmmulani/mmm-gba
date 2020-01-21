use std::env;
use std::fs;

mod gba {
    use std::fs;

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
        LoadHL(Register),
        LoadA(Register, Register),
        Inc(Register, Register),
        Dec(Register, Register),
        Jump(u16),
        Error,
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
                }
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

        pub fn opcode(&self, address: usize) -> Opcode {
            match self.content[address] {
                0x0 => Opcode::Noop,
                0xC3 => Opcode::Jump((self.content[address + 1] as u16) << 8 + (self.content[address + 2] as u16)),
                _ => Opcode::Error,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::gba;

    #[test]
    fn title() {
        let rom = gba::ROM::from_path("test-roms/cpu_instrs.gb");
        assert_eq!(rom.title(), "CPU_INSTRS");
    }

    #[test]
    fn opcode_checks() {
        let rom = gba::ROM::from_path("test-roms/cpu_instrs.gb");
        assert_eq!(rom.opcode(0x100), gba::Opcode::Noop);
    }
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    println!("Got filename {}", filename);
    let bytes: Vec<u8> = fs::read(filename)?;

    println!("Hello, world! {}", bytes.len());

    let rom = gba::ROM::from_path(filename);
    println!("rom title {}", rom.title());

    let content = rom.bytes(0x100, 4);
    println!("instructions {:x} {:x} {:x} {:x}", content[0], content[1], content[2], content[3]);

    Ok(())
}
