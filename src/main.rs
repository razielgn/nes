extern crate env_logger;
extern crate nes;

use nes::Nes;
use std::env;

fn main() {
    env_logger::init();

    let path = env::args().nth(1).unwrap();
    let mut nes = Nes::from_rom(path);

    loop {
        nes.step();
        println!("{}", nes.cpu_state_str());
    }
}
