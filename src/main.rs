extern crate nes;

use nes::{Cpu, Rom};
use std::env;

fn main() {
    let path = env::args().nth(1).unwrap();
    let rom = Rom::from_file(&path);
    let mut cpu = Cpu::new(rom);

    loop {
        println!("{}", cpu.state());
        cpu.step();
    }
}
