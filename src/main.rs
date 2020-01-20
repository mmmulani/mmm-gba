use std::env;
use std::fs;

mod gba {
    use std::fs;

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

    Ok(())
}
