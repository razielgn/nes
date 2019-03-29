extern crate nes;

use nes::Nes;
use std::env;

fn main() {
    env_logger::init();

    let path = env::args().nth(1).unwrap();
    let mut nes = Nes::from_path(path);

    loop {
        let debug_state = nes.debug_step();
        println!("{}", debug_state);
    }
}
