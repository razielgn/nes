extern crate nes;

use nes::Nes;
use std::env;

fn main() {
    let path = env::args().nth(1).unwrap();
    let mut nes = Nes::from_rom(path);

    loop {
        let cpu_state = nes.step();
        println!("{}", cpu_state);
    }
}
