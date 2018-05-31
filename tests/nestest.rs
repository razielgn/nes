extern crate nes;

use nes::Nes;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[test]
fn nestest() {
    let expected_states = {
        let f = File::open("tests/roms/nestest.log").unwrap();
        let reader = BufReader::new(f);
        reader.lines().collect::<Vec<_>>()
    };

    let mut nes = Nes::from_rom("tests/roms/nestest.nes");
    nes.set_pc(0xC000);

    println!("");
    for expected_state in expected_states {
        {
            let cpu_state = nes.cpu_state();
            assert_eq!(
                expected_state.unwrap(),
                format!("{}", cpu_state)
            );
        }

        nes.step();
    }
}
