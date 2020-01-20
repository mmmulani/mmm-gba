use std::env;
use std::fs;

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    println!("Got filename {}", filename);
    let bytes: Vec<u8> = fs::read(filename)?;

    println!("Hello, world! {}", bytes.len());

    for i in 0x104..0x133 {
        println!("Byte at {:#x}:{:#04x}", i, bytes[i]);
    }

    Ok(())
}
