extern crate env_logger;
extern crate nes;

use nes::Nes;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[test]
fn nestest() {
    env_logger::init();

    let f = File::open("tests/roms/nestest.log").unwrap();
    let reader = BufReader::new(f);

    let mut nes = Nes::from_rom("tests/roms/nestest.nes");
    nes.set_pc(0xC000);

    for (i, expected_state) in reader.lines().enumerate() {
        nes.step();
        assert_eq!(
            expected_state.unwrap(),
            format!("{}", nes.debug_state()),
            "line {}",
            i + 1
        );
    }
}
