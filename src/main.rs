use std::env;
use std::fs;

mod gba;

#[cfg(test)]
mod tests {
    use super::gba;

    #[test]
    fn test_cpu_instrs_rom() {
        let rom = gba::ROM::from_path("test-roms/cpu_instrs.gb");
        assert_eq!(rom.title(), "CPU_INSTRS");
        assert_eq!(rom.opcode(0x100), gba::Opcode::Noop);
        assert_eq!(rom.has_nintendo_logo(), true);
        assert_eq!(rom.has_valid_header_checksum(), true);
        assert_eq!(rom.cartridge_type(), gba::MemoryBankType::MBC1);
        assert_eq!(rom.ram_size(), 0);
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
