extern crate nes;

use nes::Nes;

#[test]
fn nestest() {
    let mut nes = Nes::from_buf(include_bytes!("roms/nestest.nes"));
    nes.set_pc(0xC000);

    let testlog = include_str!("roms/nestest.log");

    for (i, expected_state) in testlog.lines().enumerate() {
        let state = nes.debug_step();
        assert_eq!(expected_state, format!("{}", state), "line {}", i + 1);
    }
}
