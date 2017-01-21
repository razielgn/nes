extern crate nes;

use nes::{Cpu, Rom};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

#[test]
fn nestest() {
    let expected_states = {
        let f = File::open(Path::new("tests/cpu/nestest.log")).unwrap();
        let reader = BufReader::new(f);
        reader.lines().collect::<Vec<_>>()
    };

    let mut cpu = Cpu::new(Rom::from_file("tests/cpu/nestest.nes"));
    cpu.jump(0xC000);

    println!("");
    for expected_state in expected_states {
        {
            let state = cpu.state();
            let as_str = format!("{}", state);
            println!("{}", as_str);
            assert_eq!(expected_state.unwrap(), as_str);
        }

        cpu.step();
    }
}
