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

use bits::HighLowBits;
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

    pub fn debug_state<'a>(&'a self) -> DebugState<'a> {
        DebugState {
            prev_cpu: self.prev_cpu,
            cpu: self.cpu,
            mem: self.memory(),
        }
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

pub struct DebugState<'a> {
    prev_cpu: Cpu,
    cpu: Cpu,
    mem: Memory<'a>,
}

impl<'a> ::std::fmt::Display for DebugState<'a> {
    fn fmt(
        &self,
        f: &mut ::std::fmt::Formatter,
    ) -> Result<(), ::std::fmt::Error> {
        use instruction::AddressingMode::*;

        let p: u8 = self.prev_cpu.p.into();
        let pc = self.prev_cpu.pc;

        let bytecode = match self.cpu.addr_mode {
            Absolute | AbsoluteX(..) | AbsoluteY(..) | Indirect => {
                let (hi, lo) = self.mem.read_word(pc + 1).split();
                format!("{:02X} {:02X}", lo, hi)
            }
            ZeroPage | Relative | Immediate | ZeroPageX | ZeroPageY
            | IndexedIndirect | IndirectIndexed(..) => {
                let val = self.mem.read(pc + 1);
                format!("{:02X}", val)
            }
            None | Implied | Accumulator => "".into(),
        };

        let mode = match self.cpu.addr_mode {
            Absolute => {
                let addr = self.cpu.op_arg;
                format!("${:04X}", addr)
            }
            AbsoluteX(..) => {
                let addr = self.mem.read_word(pc + 1);
                format!("${:04X},X", addr)
            }
            AbsoluteY(..) => {
                let addr = self.mem.read_word(pc + 1);
                format!("${:04X},Y", addr)
            }
            Accumulator => "A".into(),
            Indirect => {
                let addr = self.mem.read_word(pc + 1);
                format!("(${:04X})", addr)
            }
            Immediate => {
                let addr = self.cpu.op_arg;
                let val = self.mem.read(addr);
                format!("#${:02X}", val)
            }
            ZeroPage => {
                let addr = self.cpu.op_arg;
                format!("${:02X}", addr.low())
            }
            ZeroPageX => {
                let addr = self.mem.read(pc + 1);
                format!("${:02X},X", addr)
            }
            ZeroPageY => {
                let addr = self.mem.read(pc + 1);
                format!("${:02X},Y", addr)
            }
            IndexedIndirect => {
                let arg = self.mem.read(pc + 1);
                format!("(${:02X},X)", arg)
            }
            IndirectIndexed(..) => {
                let arg = self.mem.read(pc + 1);
                format!("(${:02X}),Y", arg)
            }
            Relative => {
                let offset_addr = self.cpu.op_arg;
                let offset = u16::from(self.mem.read(offset_addr));

                let mut addr = pc.wrapping_add(2).wrapping_add(offset);
                if offset >= 0x80 {
                    addr = addr.wrapping_sub(0x100);
                }

                format!("${:04X}", addr)
            }
            None | Implied => "".into(),
        };

        write!(
            f,
            "{pc:04X}  {op:02X} {bytecode:6} {label:?} {mode:27} A:{a:02X} X:{x:02X} \
             Y:{y:02X} P:{p:02X} SP:{sp:2X} CYC:{cyc:3?}",
            pc = self.prev_cpu.pc,
            op = self.cpu.op,
            label = self.cpu.label,
            bytecode = bytecode,
            mode = mode,
            a = self.prev_cpu.a,
            x = self.prev_cpu.x,
            y = self.prev_cpu.y,
            p = p,
            sp = self.prev_cpu.sp,
            cyc = (self.prev_cpu.cycles * 3) % 341,
        )
    }
}
