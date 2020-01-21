use std::cmp::Ordering;
use std::env;
use std::fs;

const NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

mod gba {
    use std::fs;
    use std::cmp::Ordering;

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

        pub fn has_nintendo_logo(&self) -> bool {
            self.content[0x104..0x134].iter().cmp(crate::NINTENDO_LOGO.iter()) == Ordering::Equal
        }
    }
}

#[cfg(test)]
mod tests {
    use super::gba;

    #[test]
    fn test_cpu_instrs_rom() {
        let rom = gba::ROM::from_path("test-roms/cpu_instrs.gb");
        assert_eq!(rom.title(), "CPU_INSTRS");
        assert_eq!(rom.opcode(0x100), gba::Opcode::Noop);
        assert_eq!(rom.has_nintendo_logo(), true);
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

    println!("has nintendo logo {}", rom.has_nintendo_logo());

    Ok(())
}
