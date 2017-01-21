extern crate nes;

use nes::{Nes, State};

#[test]
fn abs_x_wrap() {
    run_test_rom("tests/cpu/abs_x_wrap.nes");
}

#[test]
fn branch_wrap() {
    run_test_rom("tests/cpu/branch_wrap.nes");
}

#[test]
fn basics() {
    run_test_rom("tests/cpu/basics.nes");
}

#[test]
fn implied() {
    run_test_rom("tests/cpu/implied.nes");
}

#[test]
fn immediate() {
    run_test_rom("tests/cpu/immediate.nes");
}

#[test]
fn zero_page() {
    run_test_rom("tests/cpu/zero_page.nes");
}

#[test]
fn zp_xy() {
    run_test_rom("tests/cpu/zp_xy.nes");
}

#[test]
fn absolute() {
    run_test_rom("tests/cpu/absolute.nes");
}

#[test]
fn abs_xy() {
    run_test_rom("tests/cpu/abs_xy.nes");
}

#[test]
fn ind_x() {
    run_test_rom("tests/cpu/ind_x.nes");
}

#[test]
fn ind_y() {
    run_test_rom("tests/cpu/ind_y.nes");
}

#[test]
fn branches() {
    run_test_rom("tests/cpu/branches.nes");
}

#[test]
fn stack() {
    run_test_rom("tests/cpu/stack.nes");
}

#[test]
fn jmp_jsr() {
    run_test_rom("tests/cpu/jmp_jsr.nes");
}

#[test]
fn rts() {
    run_test_rom("tests/cpu/rts.nes");
}

#[test]
fn rti() {
    run_test_rom("tests/cpu/rti.nes");
}

#[test]
fn brk() {
    run_test_rom("tests/cpu/brk.nes");
}

#[test]
fn special() {
    run_test_rom("tests/cpu/special.nes");
}

fn run_test_rom(path: &str) {
    let mut nes = Nes::from_rom(path);

    loop {
        nes.step();

        let state = nes.state();
        let test_activity = state.memory.fetch_multi(0x6001, 3);

        if &[0xDEu8, 0xB0, 0x61] == test_activity.as_slice() {
            match state.memory.fetch(0x6000u16) {
                0x00 => break,
                0x80 => {}
                _ => panic!("{}", read_message(&state)),
            }
        }
    }
}

fn read_message(state: &State) -> String {
    let mut size = 0;
    for i in 0x6004u16.. {
        if state.memory.fetch(i) == 0 {
            size = i - 0x6004;
            break;
        }
    }

    let bytes = state.memory.fetch_multi(0x6004, size as usize);
    String::from_utf8_lossy(&bytes).into()
}
