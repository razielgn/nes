extern crate nes;

use nes::Nes;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[test]
fn nestest() {
    let f = File::open("tests/roms/nestest.log").unwrap();
    let reader = BufReader::new(f);

    let mut nes = Nes::from_rom("tests/roms/nestest.nes");
    nes.set_pc(0xC000);

    for expected_state in reader.lines() {
        let cpu_state = nes.step();
        assert_eq!(expected_state.unwrap(), format!("{}", cpu_state));
    }
}
