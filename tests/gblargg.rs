extern crate nes;

use nes::{MutMemoryAccess, Nes};

#[test]
fn abs_x_wrap() {
    run_test_rom("tests/roms/abs_x_wrap.nes");
}

#[test]
fn branch_wrap() {
    run_test_rom("tests/roms/branch_wrap.nes");
}

#[test]
fn basics() {
    run_test_rom("tests/roms/basics.nes");
}

#[test]
fn implied() {
    run_test_rom("tests/roms/implied.nes");
}

#[test]
fn immediate() {
    run_test_rom("tests/roms/immediate.nes");
}

#[test]
fn zero_page() {
    run_test_rom("tests/roms/zero_page.nes");
}

#[test]
fn zp_xy() {
    run_test_rom("tests/roms/zp_xy.nes");
}

#[test]
fn absolute() {
    run_test_rom("tests/roms/absolute.nes");
}

#[test]
fn abs_xy() {
    run_test_rom("tests/roms/abs_xy.nes");
}

#[test]
fn ind_x() {
    run_test_rom("tests/roms/ind_x.nes");
}

#[test]
fn ind_y() {
    run_test_rom("tests/roms/ind_y.nes");
}

#[test]
fn branches() {
    run_test_rom("tests/roms/branches.nes");
}

#[test]
fn stack() {
    run_test_rom("tests/roms/stack.nes");
}

#[test]
fn jmp_jsr() {
    run_test_rom("tests/roms/jmp_jsr.nes");
}

#[test]
fn rts() {
    run_test_rom("tests/roms/rts.nes");
}

#[test]
fn rti() {
    run_test_rom("tests/roms/rti.nes");
}

#[test]
fn brk() {
    run_test_rom("tests/roms/brk.nes");
}

#[test]
fn special() {
    run_test_rom("tests/roms/special.nes");
}

fn run_test_rom(path: &str) {
    let mut nes = Nes::from_rom(path);

    loop {
        nes.step();

        let test_activity = nes.read_multi(0x6001, 3);

        if &[0xDEu8, 0xB0, 0x61] == test_activity.as_slice() {
            match nes.read(0x6000u16) {
                0x00 => break,
                0x80 => {}
                _ => panic!("{}", read_message(&mut nes)),
            }
        }
    }
}

fn read_message(nes: &mut Nes) -> String {
    let mut size = 0;
    for i in 0x6004u16.. {
        if nes.read(i) == 0 {
            size = i - 0x6004;
            break;
        }
    }

    let bytes = nes.read_multi(0x6004, size as usize);
    String::from_utf8_lossy(&bytes).into()
}
