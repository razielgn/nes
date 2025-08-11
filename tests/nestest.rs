extern crate nes;

use nes::Nes;
use std::fmt::Write;

#[test]
fn nestest() {
    let mut nes = Nes::from_buf(include_bytes!("roms/nestest.nes"));
    nes.set_pc(0xC000);

    let mut log = String::with_capacity(1_000_000);

    for _ in 0..8991 {
        let state = nes.debug_step();
        writeln!(log, "{state}").unwrap();
    }

    insta::assert_snapshot!(log);
}
