extern crate cursive;

use cursive::traits::*;
use cursive::views::{EditView, LinearLayout, TextView};
use cursive::Cursive;

use std::cell::RefCell;
use std::env;
use std::rc::Rc;

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
        assert_eq!(
            opcode(&[0x40]),
            (Opcode::LoadReg(Register::B, Register::B), 1)
        );
        assert_eq!(
            opcode(&[0x5A]),
            (Opcode::LoadReg(Register::E, Register::D), 1)
        );
        assert_eq!(opcode(&[0x18, 0xFF]), (Opcode::JumpRelative(-1), 2));
    }
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let rom = gba::ROM::from_path(filename);
    let title = rom.title();
    let interpreter = gba::Interpreter::with_rom(rom);
    let ref_inter = Rc::new(RefCell::new(interpreter));

    ref_inter.borrow_mut().run_program();

    return Ok(());

    let mut app = Cursive::default();

    fn update_screen(c: &mut Cursive, interpreter: &gba::Interpreter) {
        let instructions = interpreter.get_next_instructions();
        let mut first = true;
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
        c.call_on_name("registers", |v: &mut TextView| {
            v.set_content(format!(
                "A   F   \n\
                 {:04X}{:04X}\n\
                 B   C   \n\
                 {:04X}{:04X}\n\
                 D   E   \n\
                 {:04X}{:04X}\n\
                 H   L   \n\
                 {:04X}{:04X}\n\
                 SP0x{:04X}\n\
                 PC0x{:04X}",
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
            ));
        });
    };

    let code_output = TextView::new(vec![" "; 200].join("") + &vec![""; 100].join("\n"))
        .with_name("code")
        .scrollable()
        .scroll_strategy(cursive::view::ScrollStrategy::StickToBottom);
    let mut input = EditView::new().filler(" ");
    {
        let ref_inter = ref_inter.clone();
        input.set_on_submit(move |_c, _str| {
            let interpreter = &mut ref_inter.borrow_mut();
            loop {
                interpreter.run_single_instruction();
            }
        });
    }

    let repl = LinearLayout::vertical()
        .child(code_output)
        .weight(1)
        .child(input);

    let mut sidebar = LinearLayout::vertical();
    let mut registers = TextView::new("register output").with_name("registers");

    sidebar.add_child(registers);

    let panes = LinearLayout::horizontal()
        .child(repl)
        .weight(1)
        .child(sidebar);

    app.add_fullscreen_layer(panes);

    update_screen(&mut app, &ref_inter.borrow_mut());

    app.run();

    Ok(())
}
