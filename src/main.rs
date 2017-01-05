#[macro_use]
extern crate nom;

mod cpu;
mod rom;

use cpu::Cpu;
use rom::Rom;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn main() {
    let path = env::args().nth(1).unwrap();
    let mut f = File::open(Path::new(&path)).unwrap();
    let mut buf = Vec::new();
    f.read_to_end(&mut buf).unwrap();

    let (_, rom) = Rom::parse(buf.as_slice()).unwrap();

    let mut cpu = Cpu::new(rom);

    loop {
        println!("{}", cpu.state());
        cpu.step();
    }
}
