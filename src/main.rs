#[macro_use]
extern crate nom;

mod cpu;
mod instruction;
mod memory;
mod ppu;
mod rom;

use cpu::Cpu;
use rom::Rom;
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
