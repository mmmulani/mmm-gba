use std::env;
use std::fs;

mod gba;

#[cfg(test)]
mod tests {
    use super::gba;
    use gba::Opcode;
    use gba::Register;

    #[test]
    fn test_cpu_instrs_rom() {
        let rom = gba::ROM::from_path("test-roms/cpu_instrs.gb");
        assert_eq!(rom.title(), "CPU_INSTRS");
        assert_eq!(rom.opcode(0x100), (gba::Opcode::Noop, 1));
        assert_eq!(rom.has_nintendo_logo(), true);
        assert_eq!(rom.has_valid_header_checksum(), true);
        assert_eq!(rom.cartridge_type(), gba::MemoryBankType::MBC1);
        assert_eq!(rom.ram_size(), 0);
    }

    fn opcode(bytes: &[u8]) -> (gba::Opcode, u16) {
        let mut bytes = bytes.to_vec();
        if bytes.len() < 3 {
            bytes.resize(3, 0);
        }
        let rom = gba::ROM::from_bytes(bytes.to_vec());
        rom.opcode(0)
    }

    #[test]
    fn test_opcodes() {
        assert_eq!(opcode(&[0x0]), (Opcode::Noop, 1));
        assert_eq!(opcode(&[0x40]), (Opcode::LoadReg(Register::B, Register::B), 1));
        assert_eq!(opcode(&[0x5A]), (Opcode::LoadReg(Register::E, Register::D), 1));
        assert_eq!(opcode(&[0x18, 0xFF]), (Opcode::JumpRelative(-1), 2));
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
    println!(
        "instructions {:x} {:x} {:x} {:x}",
        content[0], content[1], content[2], content[3]
    );

    println!("has nintendo logo {}", rom.has_nintendo_logo());

    println!("memory type {:?}", rom.cartridge_type());

    println!("ram size {:x}", rom.ram_size());

    rom.has_valid_header_checksum();

    let mut interpreter = gba::Interpreter::with_rom(rom);
    interpreter.run_program();

    Ok(())
}
