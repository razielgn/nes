#![cfg_attr(feature = "cargo-clippy", allow(new_without_default))]

#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;

mod bits;
mod cpu;
mod instruction;
mod mapper;
mod memory;
mod ppu;
mod rom;

pub use cpu::{Cpu, Cycles};
use mapper::Mapper;
pub use memory::{Access, Memory, MutMemory, Ram};
use ppu::Ppu;
pub use rom::Rom;
use std::path::Path;

pub struct Nes {
    prev_cpu: Cpu,
    cpu: Cpu,
    mapper: Mapper,
    ppu: Ppu,
    ram: Ram,
}

impl Nes {
    pub fn from_rom<P: AsRef<Path>>(path: P) -> Self {
        let rom = Rom::from_path(path);
        let mapper = Mapper::new(rom);
        let pc = mapper.read_word(0xFFFC);

        Nes {
            cpu: Cpu::new(pc),
            prev_cpu: Cpu::new(pc),
            mapper: mapper,
            ppu: Ppu::new(),
            ram: Ram::new(),
        }
    }

    pub fn step(&mut self) -> Cycles {
        debug!("step");
        self.prev_cpu = self.cpu;

        let cycles = {
            let mut mmap = MutMemory {
                ram: &mut self.ram,
                mapper: &mut self.mapper,
                ppu: &mut self.ppu,
            };

            self.cpu.step(&mut mmap)
        };

        for _ in 0..cycles * 3 {
            let nmi_triggered = self.ppu.step();

            if nmi_triggered {
                self.cpu.trigger_nmi();
            }
        }

        cycles
    }

    pub fn reset(&mut self) {
        let mut mmap = MutMemory {
            ram: &mut self.ram,
            mapper: &mut self.mapper,
            ppu: &mut self.ppu,
        };

        self.cpu.reset(&mut mmap)
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.jump(pc);
    }

    pub fn cpu_state_str(&self) -> String {
        use instruction::AddressingMode::*;

        let p: u8 = self.prev_cpu.p.into();

        let args;
        let mode;
        match self.cpu.addr_mode {
            Absolute => {
                let addr = self.cpu.op_arg;
                args = format!("{:02X} {:02X}", addr as u8, addr >> 8);
                mode = format!("${:04X}", addr);
            }
            AbsoluteX(..) => {
                let pc = self.prev_cpu.pc;
                let addr = self.read_word(pc + 1);
                args = format!("{:02X} {:02X}", addr as u8, addr >> 8);
                mode = format!("${:04X},X", addr);
            }
            AbsoluteY(..) => {
                let pc = self.prev_cpu.pc;
                let addr = self.read_word(pc + 1);
                args = format!("{:02X} {:02X}", addr as u8, addr >> 8);
                mode = format!("${:04X},Y", addr);
            }
            Accumulator => {
                args = "".into();
                mode = "A".into();
            }
            Indirect => {
                let pc = self.prev_cpu.pc;
                let addr = self.read_word(pc + 1);
                args = format!("{:02X} {:02X}", addr as u8, addr >> 8);
                mode = format!("(${:04X})", addr);
            }
            Immediate => {
                let addr = self.cpu.op_arg;
                let val = self.read(addr);
                args = format!("{:02X}", val);
                mode = format!("#${:02X}", val);
            }
            ZeroPage => {
                let addr = self.cpu.op_arg;
                args = format!("{:02X}", addr);
                mode = format!("${:02X}", addr as u8);
            }
            ZeroPageX => {
                let pc = self.prev_cpu.pc;
                let addr = self.read(pc + 1);
                args = format!("{:02X}", addr);
                mode = format!("${:02X},X", addr as u8);
            }
            ZeroPageY => {
                let pc = self.prev_cpu.pc;
                let addr = self.read(pc + 1);
                args = format!("{:02X}", addr);
                mode = format!("${:02X},Y", addr as u8);
            }
            IndexedIndirect => {
                let pc = self.prev_cpu.pc;
                let arg = self.read(pc + 1);
                args = format!("{:02X}", arg);
                mode = format!("(${:02X},X)", arg);
            }
            IndirectIndexed(..) => {
                let pc = self.prev_cpu.pc;
                let arg = self.read(pc + 1);
                args = format!("{:02X}", arg);
                mode = format!("(${:02X}),Y", arg);
            }
            Relative => {
                let offset_addr = self.cpu.op_arg;
                let offset = u16::from(self.read(offset_addr));
                args = format!("{:02X}", offset);

                let mut addr =
                    self.prev_cpu.pc.wrapping_add(2).wrapping_add(offset);
                if offset >= 0x80 {
                    addr = addr.wrapping_sub(0x100);
                }

                mode = format!("${:04X}", addr);
            }
            None | Implied => {
                args = "".into();
                mode = "".into();
            }
        }

        format!(
            "{pc:04X}  {op:02X} {args:6} {label:?} {mode:27} A:{a:02X} X:{x:02X} \
             Y:{y:02X} P:{p:02X} SP:{sp:2X} CYC:{cyc:3?}",
            pc = self.prev_cpu.pc,
            op = self.cpu.op,
            label = self.cpu.label,
            args = args,
            mode = mode,
            a = self.prev_cpu.a,
            x = self.prev_cpu.x,
            y = self.prev_cpu.y,
            p = p,
            sp = self.prev_cpu.sp,
            cyc = (self.prev_cpu.cycles * 3) % 341,
        )
    }

    fn memory<'a>(&'a self) -> Memory<'a> {
        Memory {
            ram: &self.ram,
            mapper: &self.mapper,
            ppu: &self.ppu,
        }
    }
}

impl Access for Nes {
    fn read(&self, addr: u16) -> u8 {
        self.memory().read(addr)
    }
}
