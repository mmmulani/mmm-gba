extern crate cursive;
extern crate termion;

use cursive::traits::*;
use cursive::views::{EditView, LinearLayout, NamedView, ScrollView, TextView};
use cursive::Cursive;

use gba::JoypadButton;

use terminal_size::{terminal_size, Height, Width};

use std::cell::RefCell;
use std::env;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;
use std::sync::mpsc;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::TryRecvError;
use std::{thread, time};

use parse_int::parse;

use termion::event::Key;
use termion::input::TermRead;
use termion::raw::IntoRawMode;

mod gba;

#[cfg(test)]
mod tests {
    use super::gba;
    use gba::Opcode;
    use gba::Register;

    #[test]
    fn test_cpu_instrs_rom() {
        let rom = gba::ROM::from_path("test-roms/blargg/cpu_instrs/cpu_instrs.gb");
        assert_eq!(rom.title(), "CPU_INSTRS");
        assert_eq!(rom.has_nintendo_logo(), true);
        assert_eq!(rom.has_valid_header_checksum(), true);
        assert_eq!(rom.cartridge_type(), gba::MemoryBankType::MBC1);
        assert_eq!(rom.ram_size(), 0);
    }

    fn opcode(bytes: &[u8]) -> (gba::Opcode, u16, u16) {
        let mut bytes = bytes.to_vec();
        if bytes.len() < 3 {
            bytes.resize(3, 0);
        }
        let rom = gba::ROM::from_bytes(bytes.to_vec());
        rom.opcode(0, |address| bytes[address as usize])
    }

    #[test]
    fn test_opcodes() {
        assert_eq!(opcode(&[0x0]), (Opcode::Noop, 1, 4));
        assert_eq!(
            opcode(&[0x40]),
            (Opcode::LoadReg(Register::B, Register::B), 1, 4)
        );
        assert_eq!(
            opcode(&[0x5A]),
            (Opcode::LoadReg(Register::E, Register::D), 1, 4)
        );
        assert_eq!(opcode(&[0x18, 0xFF]), (Opcode::JumpRelative(-1), 2, 12));
    }

    #[test]
    fn test_roms() {
        let test_cpu_instr = |name| {
            let rom = gba::ROM::from_path(&format!(
                "test-roms/blargg/cpu_instrs/individual/{}.gb",
                name
            ));
            let mut interpreter = gba::Interpreter::with_rom(rom);
            let mut passing = false;
            for _i in 0..10000 {
                for _j in 0..1000 {
                    interpreter.run_single_instruction();
                }
                if interpreter.output.contains("Passed") || interpreter.output.contains("Fail") {
                    passing = interpreter.output.contains("Passed");
                    break;
                }
            }
            assert!(passing);
        };
        let test_rom = |name| {
            let rom = gba::ROM::from_path(&format!("test-roms/blargg/{}.gb", name));
            let mut interpreter = gba::Interpreter::with_rom(rom);
            let mut passing = false;
            let mut old_output_length = 0;
            loop {
                for _j in 0..10000000 {
                    interpreter.run_single_instruction();
                }
                let new_length = interpreter.output.len();
                if old_output_length == new_length {
                    break;
                }
                old_output_length = new_length;
                if interpreter.output.contains("Passed") || interpreter.output.contains("Fail") {
                    passing = interpreter.output.contains("Passed");
                    break;
                }
            }
            assert!(passing);
        };
        test_cpu_instr("01-special");
        test_cpu_instr("02-interrupts");
        test_cpu_instr("03-op sp,hl");
        test_cpu_instr("04-op r,imm");
        test_cpu_instr("05-op rp");
        test_cpu_instr("05-op rp");
        test_cpu_instr("06-ld r,r");
        test_cpu_instr("07-jr,jp,call,ret,rst");
        test_cpu_instr("08-misc instrs");
        test_cpu_instr("09-op r,r");
        test_cpu_instr("10-bit ops");
        test_cpu_instr("11-op a,(hl)");
        test_rom("cpu_instrs/cpu_instrs");
        test_rom("instr_timing/instr_timing");
        test_rom("mem_timing/mem_timing");
    }
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let rom = gba::ROM::from_path(filename);
    let _title = rom.title();
    let interpreter = gba::Interpreter::with_rom(rom);
    let ref_inter = Rc::new(RefCell::new(interpreter));

    let mut should_run = false;
    let mut should_show = false;

    for arg in args {
        match arg.as_ref() {
            "--run" => should_run = true,
            "--show" => should_show = true,
            _ => (),
        }
    }

    if should_run {
        ref_inter.borrow_mut().run_program();
        return Ok(());
    }

    if should_show {
        let size = terminal_size();
        if let Some((Width(w), Height(h))) = size {
            if w < 160 || h < 74 {
                println!("Your terminal must be at least 160 cols wide and 74 lines tall, it is currently {}x{}", w, h);
                std::process::exit(0);
            }
        }

        let (tx, rx) = mpsc::channel::<termion::event::Key>();
        thread::spawn(move || loop {
            for c in stdin().keys() {
                tx.send(c.unwrap()).unwrap();
            }
        });

        let mut inter = ref_inter.borrow_mut();
        let mut stdout = stdout().into_raw_mode().unwrap();
        let mut buttons_to_release: Vec<JoypadButton> = vec![];
        loop {
            for _i in 0..70000 {
                inter.run_single_instruction();
            }
            print!("\x1b[H");
            for y in (0..144).step_by(2) {
                for x in 0..160 {
                    let top = inter.pixel_at(x, y);
                    let bottom = inter.pixel_at(x, y + 1);
                    let top_str = match top {
                        0 => "30",
                        1 => "90",
                        2 => "37",
                        _ => "97",
                    };
                    let bottom_str = match bottom {
                        0 => "40",
                        1 => "100",
                        2 => "47",
                        _ => "107",
                    };
                    print!("\x1b[0;{};{}m▀", top_str, bottom_str);
                }
                println!("\x1b[m\r");
            }
            println!("\x1b[0;37m█████████████████████████████████████████████████\x1b[m\r");

            for p in buttons_to_release {
                inter.release_button(p);
            }
            buttons_to_release = vec![];

            let mut press = |k| {
                inter.push_button(k);
                buttons_to_release.push(k);
            };
            for c in rx.try_iter() {
                match c {
                    Key::Ctrl('c') | Key::Char('q') => return Ok(()),
                    Key::Char('\n') => press(JoypadButton::Start),
                    Key::Char('a') => press(JoypadButton::A),
                    Key::Char('z') | Key::Char(';') | Key::Char('s') | Key::Char('o') => {
                        press(JoypadButton::B)
                    }
                    Key::Char(' ') => press(JoypadButton::Select),
                    Key::Up => press(JoypadButton::Up),
                    Key::Down => press(JoypadButton::Down),
                    Key::Left => press(JoypadButton::Left),
                    Key::Right => press(JoypadButton::Right),
                    _ => (),
                }
            }

            stdout.flush().unwrap();

            std::thread::sleep(std::time::Duration::from_millis(100));
        }
    }

    let mut app = Cursive::default();

    fn update_screen(c: &mut Cursive, interpreter: &gba::Interpreter) {
        let instructions = interpreter.get_next_instructions();
        let mut first = true;
        c.call_on_name("code_scroll", |v: &mut ScrollView<NamedView<TextView>>| {
            v.set_scroll_strategy(cursive::view::ScrollStrategy::StickToBottom);
        });
        for (address, opcode) in &instructions {
            c.call_on_name("code", |v: &mut TextView| {
                v.append(format!(
                    "{} 0x{:X} {:X?}\n",
                    if first { "\n-->" } else { "   " },
                    address,
                    opcode
                ));
            });
            first = false;
        }

        let registers = interpreter.register_state;
        let program_state = interpreter.program_state;
        let interrupts = interpreter.interrupts;
        c.call_on_name("registers", |v: &mut TextView| {
            v.set_content(format!(
                "A   F   \n\
                 {:02X}  {:02X}\n\
                 B   C   \n\
                 {:02X}  {:02X}\n\
                 D   E   \n\
                 {:02X}  {:02X}\n\
                 H   L   \n\
                 {:02X}  {:02X}\n\
                 SP  {:04X}\n\
                 PC  {:04X}\n\
                 ZNHC\n\
                 {}{}{}{}\n\
                 --------\n\
                 IME {}\n\
                 ...JSTLV\n\
                 IE {:05b}\n\
                 IF {:05b}\n\
                 \n\
                 {}\n\
                 {:?}\n\
                 RAM: {}\n\
                 Cycles\n\
                 {}",
                registers.a,
                registers.f,
                registers.b,
                registers.c,
                registers.d,
                registers.e,
                registers.h,
                registers.l,
                program_state.stack_pointer,
                program_state.program_counter,
                ((registers.f & (1 << 7)) != 0) as u8,
                ((registers.f & (1 << 6)) != 0) as u8,
                ((registers.f & (1 << 5)) != 0) as u8,
                ((registers.f & (1 << 4)) != 0) as u8,
                if interrupts.master_enabled { 1 } else { 0 },
                interrupts.enable_flag,
                interrupts.request_flag & 0x1F,
                interpreter.rom.title(),
                interpreter.rom.cartridge_type(),
                interpreter.rom.ram_size(),
                program_state.cycle_count,
            ));
        });
    };

    let code_output = TextView::new(vec![" "; 200].join("") + &vec![""; 100].join("\n"))
        .with_name("code")
        .scrollable()
        .scroll_strategy(cursive::view::ScrollStrategy::StickToBottom)
        .with_name("code_scroll");
    let mut input = EditView::new().filler(" ").with_name("input");
    {
        let ref_inter = ref_inter.clone();
        input.get_mut().set_on_submit(move |mut c, str| {
            let interpreter = &mut ref_inter.borrow_mut();
            if str == "c" {
                loop {
                    let result = interpreter.safely_run_instruction();
                    if result.is_err() {
                        break;
                    }
                }
            } else if str.starts_with("x") {
                let args: Vec<&str> = str.split(" ").collect();
                let amount = parse::<u64>(&args[1]).unwrap();
                let target = interpreter.program_state.cycle_count + amount;
                while interpreter.program_state.cycle_count < target {
                    interpreter.run_single_instruction();
                }
            } else if str.starts_with("o") {
                let args: Vec<&str> = str.split(" ").collect();
                let amount = parse::<u64>(&args[1]).unwrap();
                for _ in 0..amount {
                    interpreter.run_single_instruction();
                }
            } else if str.starts_with("b") {
                let args: Vec<&str> = str.split(" ").collect();
                let pc = parse::<u16>(&args[1]).unwrap();
                loop {
                    if interpreter.program_state.program_counter == pc {
                        break;
                    }
                    interpreter.run_single_instruction();
                }
            } else {
                interpreter.run_single_instruction();
            }
            update_screen(&mut c, interpreter);
        });
    }

    let repl = LinearLayout::vertical()
        .child(code_output)
        .weight(1)
        .child(input);

    let mut sidebar = LinearLayout::vertical();
    let registers = TextView::new("register output").with_name("registers");

    sidebar.add_child(registers);

    let panes = LinearLayout::horizontal()
        .child(repl)
        .weight(1)
        .child(sidebar);

    app.add_fullscreen_layer(panes);
    app.focus_name("input").ok();

    {
        let mut interpreter = ref_inter.borrow_mut();
        while interpreter.program_state.program_counter != 0x100 {
            interpreter.run_single_instruction();
        }
    }

    update_screen(&mut app, &ref_inter.borrow_mut());

    app.run();

    Ok(())
}
