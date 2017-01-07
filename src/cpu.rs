use rom::Rom;
use std::fmt;

pub struct Cpu<'rom> {
    cycles: usize,
    pc: u16,
    sp: u8,
    p: P,
    a: u8,
    x: u8,
    y: u8,

    /// 0x100   => Zero Page
    /// 0x200   => Stack
    /// 0x800   => RAM
    /// 0x2000  => Mirrors (0-0x7FF)
    /// 0x2008  => I/O Registers
    /// 0x4000  => Mirrors (0x2000-0x2007)
    /// 0x4020  => I/O Registers
    /// 0x6000  => Expansion ROM
    /// 0x8000  => SRAM
    /// 0xC000  => PRG-ROM (Lower Bank)
    /// 0x10000 => PRG-ROM (Upper Bank)
    memory: Memory<'rom>,
}

impl<'rom> Cpu<'rom> {
    pub fn new(rom: Rom<'rom>) -> Self {
        Cpu {
            cycles: 0,
            pc: 0xc000,
            sp: 0xfd,
            p: P::new(),
            a: 0,
            x: 0,
            y: 0,
            memory: Memory::new(rom),
        }
    }

    pub fn state(&self) -> CpuState {
        CpuState {
            pc: self.pc,
            a: self.a,
            x: self.x,
            y: self.y,
            p: self.p,
            sp: self.sp,
            cycles: self.cycles,
            instr: self.fetch(),
            mem: &self.memory,
        }
    }

    pub fn step(&mut self) {
        use self::Label::*;
        use self::Status::*;

        let instr = self.fetch();
        let addr = instr.addr;

        self.pc += instr.size as u16;
        self.cycles += instr.cycles as usize;

        match instr.label {
            JMP => {
                self.pc = addr;
            }
            JSR => {
                let pc = self.pc;
                self.push_double(pc - 1);
                self.pc = addr;
            }
            RTS => {
                self.pc = self.pop_double() + 1;
            }
            RTI => {
                self.pop_p();
                self.pc = self.pop_double();
            }
            PHP => {
                let mut p = self.p;
                p.set(BreakCommand);
                self.push(p.into());
            }
            PHA => {
                let a = self.a;
                self.push(a);
            }
            PLA => {
                self.a = self.pop();
                self.p.set_if_zn(self.a);
            }
            PLP => {
                self.pop_p();
            }
            LDA => {
                self.a = self.memory.fetch(addr);
                self.p.set_if_zn(self.a);
            }
            LDX => {
                self.x = self.memory.fetch(addr);
                self.p.set_if_zn(self.x);
            }
            LDY => {
                self.y = self.memory.fetch(addr);
                self.p.set_if_zn(self.y);
            }
            STX => {
                self.memory.store(addr, self.x);
            }
            STA => {
                self.memory.store(addr, self.a);
            }
            NOP => {}
            SEC => {
                self.p.set(CarryFlag);
            }
            SEI => {
                self.p.set(InterruptDisable);
            }
            SED => {
                self.p.set(DecimalMode);
            }
            CLC => {
                self.p.unset(CarryFlag);
            }
            CLD => {
                self.p.unset(DecimalMode);
            }
            CLV => {
                self.p.unset(OverflowFlag);
            }
            BCS => {
                if self.p.is_set(CarryFlag) {
                    self.cycles += 1;
                    self.pc = addr;
                }
            }
            BCC => {
                if !self.p.is_set(CarryFlag) {
                    self.cycles += 1;
                    self.pc = addr;
                }
            }
            BEQ => {
                if self.p.is_set(ZeroFlag) {
                    self.cycles += 1;
                    self.pc = addr;
                }
            }
            BNE => {
                if !self.p.is_set(ZeroFlag) {
                    self.cycles += 1;
                    self.pc = addr;
                }
            }
            BVS => {
                if self.p.is_set(OverflowFlag) {
                    self.cycles += 1;
                    self.pc = addr;
                }
            }
            BVC => {
                if !self.p.is_set(OverflowFlag) {
                    self.cycles += 1;
                    self.pc = addr;
                }
            }
            BMI => {
                if self.p.is_set(NegativeFlag) {
                    self.cycles += 1;
                    self.pc = addr;
                }
            }
            BPL => {
                if !self.p.is_set(NegativeFlag) {
                    self.cycles += 1;
                    self.pc = addr;
                }
            }
            BIT => {
                let m = self.memory.fetch(addr);
                let res = self.a & m;

                self.p.set_if_zero(res);
                self.p.set_if_negative(m);
                self.p.set_if(OverflowFlag, (m >> 6) & 1 == 1);
            }
            AND => {
                self.a &= self.memory.fetch(addr);
                self.p.set_if_zn(self.a);
            }
            ORA => {
                self.a |= self.memory.fetch(addr);
                self.p.set_if_zn(self.a);
            }
            EOR => {
                self.a ^= self.memory.fetch(addr);
                self.p.set_if_zn(self.a);
            }
            CMP => {
                let m = self.memory.fetch(addr);
                let n = self.a.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.a >= m);
                self.p.set_if(NegativeFlag, (n as i8) < 0);
                self.p.set_if(ZeroFlag, n == 0);
            }
            CPY => {
                let m = self.memory.fetch(addr);
                let n = self.y.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.y >= m);
                self.p.set_if(NegativeFlag, (n as i8) < 0);
                self.p.set_if(ZeroFlag, n == 0);
            }
            CPX => {
                let m = self.memory.fetch(addr);
                let n = self.x.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.x >= m);
                self.p.set_if(NegativeFlag, (n as i8) < 0);
                self.p.set_if(ZeroFlag, n == 0);
            }
            ADC => {
                let m = self.memory.fetch(addr);
                let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };
                let (sum, overflow1) = self.a.overflowing_add(m);
                let (sum, overflow2) = sum.overflowing_add(c);
                let overflow = overflow1 || overflow2;

                self.p.set_if(CarryFlag, overflow);
                self.p.set_if(OverflowFlag,
                              (self.a ^ sum) & 0x80 != 0 &&
                              (self.a ^ m) & 0x80 == 0);
                self.p.set_if_zn(sum);

                self.a = sum;
            }
            SBC => {
                let m = self.memory.fetch(addr);
                let c = if self.p.is_set(CarryFlag) { 0 } else { 1 };
                let (sub, overflow1) = self.a.overflowing_sub(m);
                let (sub, overflow2) = sub.overflowing_sub(c);
                let overflow = overflow1 || overflow2;

                self.p.unset_if(CarryFlag, overflow);
                self.p.set_if(OverflowFlag,
                              (self.a ^ sub) & 0x80 != 0 &&
                              (self.a ^ m) & 0x80 != 0);
                self.p.set_if_zn(sub);

                self.a = sub;
            }
            INY => {
                self.y = self.y.wrapping_add(1);
                self.p.set_if_zn(self.y);
            }
            DEY => {
                self.y = self.y.wrapping_sub(1);
                self.p.set_if_zn(self.y);
            }
            INX => {
                self.x = self.x.wrapping_add(1);
                self.p.set_if_zn(self.x);
            }
            DEX => {
                self.x = self.x.wrapping_sub(1);
                self.p.set_if_zn(self.x);
            }
            TAY => {
                self.y = self.a;
                self.p.set_if_zn(self.y);
            }
            TAX => {
                self.x = self.a;
                self.p.set_if_zn(self.x);
            }
            TYA => {
                self.a = self.y;
                self.p.set_if_zn(self.a);
            }
            TXA => {
                self.a = self.x;
                self.p.set_if_zn(self.a);
            }
            TXS => {
                self.sp = self.x;
            }
            TSX => {
                self.x = self.sp;
                self.p.set_if_zn(self.x);
            }
            _ => panic!("can't execute {:?}", instr),
        }
    }

    fn fetch(&self) -> Instruction {
        use self::AddressingMode::*;
        use self::Label::*;

        let op = self.memory.fetch(self.pc);
        let (label, mode, size, cycles, page_cycles) = match op {
            0x00 => (BRK, Implied, 1, 7, 0),
            0x01 => (ORA, IndexedIndirect, 2, 6, 0),
            0x05 => (ORA, ZeroPage, 2, 3, 0),
            0x06 => (ASL, ZeroPage, 2, 5, 0),
            0x08 => (PHP, Implied, 1, 3, 0),
            0x09 => (ORA, Immediate, 2, 2, 0),
            0x0A => (ASL, Accumulator, 1, 2, 0),
            0x0D => (ORA, Absolute, 3, 4, 0),
            0x0E => (ASL, Absolute, 3, 6, 0),
            0x10 => (BPL, Relative, 2, 2, 2),
            0x11 => (ORA, IndirectIndexed, 2, 5, 1),
            0x15 => (ORA, ZeroPageX, 2, 4, 0),
            0x16 => (ASL, ZeroPageX, 2, 6, 0),
            0x18 => (CLC, Implied, 1, 2, 0),
            0x19 => (ORA, AbsoluteY, 3, 4, 1),
            0x1D => (ORA, AbsoluteX, 3, 4, 1),
            0x1E => (ASL, AbsoluteX, 3, 7, 0),
            0x20 => (JSR, Absolute, 3, 6, 0),
            0x21 => (AND, IndexedIndirect, 2, 6, 0),
            0x24 => (BIT, ZeroPage, 2, 3, 0),
            0x25 => (AND, ZeroPage, 2, 3, 0),
            0x26 => (ROL, ZeroPage, 2, 5, 0),
            0x28 => (PLP, Implied, 1, 4, 0),
            0x29 => (AND, Immediate, 2, 2, 0),
            0x2A => (ROL, Accumulator, 1, 2, 0),
            0x2C => (BIT, Absolute, 3, 4, 0),
            0x2D => (AND, Absolute, 3, 4, 0),
            0x2E => (ROL, Absolute, 3, 6, 0),
            0x30 => (BMI, Relative, 2, 2, 2),
            0x31 => (AND, IndirectIndexed, 2, 5, 1),
            0x35 => (AND, ZeroPageX, 2, 4, 0),
            0x36 => (ROL, ZeroPageX, 2, 6, 0),
            0x38 => (SEC, Implied, 1, 2, 0),
            0x39 => (AND, AbsoluteY, 3, 4, 1),
            0x3D => (AND, AbsoluteX, 3, 4, 1),
            0x3E => (ROL, AbsoluteX, 3, 7, 0),
            0x40 => (RTI, Implied, 1, 6, 0),
            0x41 => (EOR, IndexedIndirect, 2, 6, 0),
            0x45 => (EOR, ZeroPage, 2, 3, 0),
            0x46 => (LSR, ZeroPage, 2, 5, 0),
            0x48 => (PHA, Implied, 1, 3, 0),
            0x49 => (EOR, Immediate, 2, 2, 0),
            0x4A => (LSR, Accumulator, 1, 2, 0),
            0x4C => (JMP, Absolute, 3, 3, 0),
            0x4D => (EOR, Absolute, 3, 4, 0),
            0x4E => (LSR, Absolute, 3, 6, 0),
            0x50 => (BVC, Relative, 2, 2, 2),
            0x51 => (EOR, IndirectIndexed, 2, 5, 1),
            0x55 => (EOR, ZeroPageX, 2, 4, 0),
            0x56 => (LSR, ZeroPageX, 2, 6, 0),
            0x58 => (CLI, Implied, 1, 2, 0),
            0x59 => (EOR, AbsoluteY, 3, 4, 1),
            0x5D => (EOR, AbsoluteX, 3, 4, 1),
            0x5E => (LSR, AbsoluteX, 3, 7, 0),
            0x60 => (RTS, Implied, 1, 6, 0),
            0x61 => (ADC, IndexedIndirect, 2, 6, 0),
            0x65 => (ADC, ZeroPage, 2, 3, 0),
            0x66 => (ROR, ZeroPage, 2, 5, 0),
            0x68 => (PLA, Implied, 1, 4, 0),
            0x69 => (ADC, Immediate, 2, 2, 0),
            0x6A => (ROR, Accumulator, 1, 2, 0),
            0x6C => (JMP, Indirect, 3, 5, 0),
            0x6D => (ADC, Absolute, 3, 4, 0),
            0x6E => (ROR, Absolute, 3, 6, 0),
            0x70 => (BVS, Relative, 2, 2, 2),
            0x71 => (ADC, IndirectIndexed, 2, 5, 1),
            0x75 => (ADC, ZeroPageX, 2, 4, 0),
            0x76 => (ROR, ZeroPageX, 2, 6, 0),
            0x78 => (SEI, Implied, 1, 2, 0),
            0x79 => (ADC, AbsoluteY, 3, 4, 1),
            0x7D => (ADC, AbsoluteX, 3, 4, 1),
            0x7E => (ROR, AbsoluteX, 3, 7, 0),
            0x81 => (STA, IndexedIndirect, 2, 6, 0),
            0x84 => (STY, ZeroPage, 2, 3, 0),
            0x85 => (STA, ZeroPage, 2, 3, 0),
            0x86 => (STX, ZeroPage, 2, 3, 0),
            0x88 => (DEY, Implied, 1, 2, 0),
            0x8A => (TXA, Implied, 1, 2, 0),
            0x8C => (STY, Absolute, 3, 4, 0),
            0x8D => (STA, Absolute, 3, 4, 0),
            0x8E => (STX, Absolute, 3, 4, 0),
            0x90 => (BCC, Relative, 2, 2, 2),
            0x91 => (STA, IndirectIndexed, 2, 6, 0),
            0x94 => (STY, ZeroPageY, 2, 4, 0),
            0x95 => (STA, ZeroPageX, 2, 4, 0),
            0x96 => (STX, ZeroPageY, 2, 4, 0),
            0x98 => (TYA, Implied, 1, 2, 0),
            0x99 => (STA, AbsoluteY, 3, 5, 0),
            0x9A => (TXS, Implied, 1, 2, 0),
            0x9D => (STA, AbsoluteX, 3, 5, 0),
            0xA0 => (LDY, Immediate, 2, 2, 0),
            0xA1 => (LDA, IndexedIndirect, 2, 6, 0),
            0xA2 => (LDX, Immediate, 2, 2, 0),
            0xA4 => (LDY, ZeroPage, 2, 3, 0),
            0xA5 => (LDA, ZeroPage, 2, 3, 0),
            0xA6 => (LDX, ZeroPage, 2, 3, 0),
            0xA8 => (TAY, Implied, 1, 2, 0),
            0xA9 => (LDA, Immediate, 2, 2, 0),
            0xAA => (TAX, Implied, 1, 2, 0),
            0xAC => (LDY, Absolute, 3, 4, 0),
            0xAD => (LDA, Absolute, 3, 4, 0),
            0xAE => (LDX, Absolute, 3, 4, 0),
            0xB0 => (BCS, Relative, 2, 2, 2),
            0xB1 => (LDA, IndirectIndexed, 2, 5, 1),
            0xB4 => (LDY, ZeroPageX, 2, 4, 0),
            0xB5 => (LDA, ZeroPageX, 2, 4, 0),
            0xB6 => (LDX, ZeroPageY, 2, 4, 0),
            0xB8 => (CLV, Implied, 1, 2, 0),
            0xB9 => (LDA, AbsoluteY, 3, 4, 1),
            0xBA => (TSX, Implied, 1, 2, 0),
            0xBC => (LDY, AbsoluteX, 3, 4, 1),
            0xBD => (LDA, AbsoluteX, 3, 4, 1),
            0xBE => (LDX, AbsoluteY, 3, 4, 1),
            0xC0 => (CPY, Immediate, 2, 2, 0),
            0xC1 => (CMP, IndexedIndirect, 2, 6, 0),
            0xC4 => (CPY, ZeroPage, 2, 3, 0),
            0xC5 => (CMP, ZeroPage, 2, 3, 0),
            0xC6 => (DEC, ZeroPage, 2, 5, 0),
            0xC8 => (INY, Implied, 1, 2, 0),
            0xC9 => (CMP, Immediate, 2, 2, 0),
            0xCA => (DEX, Implied, 1, 2, 0),
            0xCC => (CPY, Absolute, 3, 4, 0),
            0xCD => (CMP, Absolute, 3, 4, 0),
            0xCE => (DEC, Absolute, 3, 6, 0),
            0xD0 => (BNE, Relative, 2, 2, 2),
            0xD1 => (CMP, IndirectIndexed, 2, 5, 1),
            0xD5 => (CMP, ZeroPageX, 2, 4, 0),
            0xD6 => (DEC, ZeroPageX, 2, 6, 0),
            0xD8 => (CLD, Implied, 1, 2, 0),
            0xD9 => (CMP, AbsoluteY, 3, 4, 1),
            0xDD => (CMP, AbsoluteX, 3, 4, 1),
            0xDE => (DEC, AbsoluteX, 3, 7, 0),
            0xE0 => (CPX, Immediate, 2, 2, 0),
            0xE1 => (SBC, IndexedIndirect, 2, 6, 0),
            0xE4 => (CPX, ZeroPage, 2, 3, 0),
            0xE5 => (SBC, ZeroPage, 2, 3, 0),
            0xE6 => (INC, ZeroPage, 2, 5, 0),
            0xE8 => (INX, Implied, 1, 2, 0),
            0xE9 => (SBC, Immediate, 2, 2, 0),
            0xEA => (NOP, Implied, 1, 2, 0),
            0xEC => (CPX, Absolute, 3, 4, 0),
            0xED => (SBC, Absolute, 3, 4, 0),
            0xEE => (INC, Absolute, 3, 6, 0),
            0xF0 => (BEQ, Relative, 2, 2, 2),
            0xF1 => (SBC, IndirectIndexed, 2, 5, 1),
            0xF5 => (SBC, ZeroPageX, 2, 4, 0),
            0xF6 => (INC, ZeroPageX, 2, 6, 0),
            0xF8 => (SED, Implied, 1, 2, 0),
            0xF9 => (SBC, AbsoluteY, 3, 4, 1),
            0xFD => (SBC, AbsoluteX, 3, 4, 1),
            0xFE => (INC, AbsoluteX, 3, 7, 0),
            op => panic!("unknown opcode 0x{:X} at addr {:04X}", op, self.pc),
        };

        Instruction {
            mode: mode,
            label: label,
            op: op,
            addr: self.addr_from_mode(mode),
            size: size,
            cycles: cycles,
            page_cycles: page_cycles,
        }
    }

    fn addr_from_mode(&self, mode: AddressingMode) -> u16 {
        use self::AddressingMode::*;

        match mode {
            Implied | Accumulator => 0,
            Immediate => self.pc + 1,
            ZeroPage => self.memory.fetch(self.pc + 1) as u16,
            ZeroPageX => (self.memory.fetch(self.pc + 1) + self.x) as u16,
            ZeroPageY => (self.memory.fetch(self.pc + 1) + self.y) as u16,
            Relative => {
                let offset = self.memory.fetch(self.pc + 1) as i8;

                if offset < 0 {
                    self.pc + 2 - offset as u16
                } else {
                    self.pc + 2 + offset as u16
                }
            }
            Absolute => self.memory.fetch_double(self.pc + 1),
            AbsoluteX => self.memory.fetch_double(self.pc + 1) + self.x as u16,
            AbsoluteY => self.memory.fetch_double(self.pc + 1) + self.y as u16,
            Indirect => unimplemented!(),
            IndexedIndirect => unimplemented!(),
            IndirectIndexed => unimplemented!(),
        }
    }

    fn push(&mut self, val: u8) {
        self.memory.store(0x100 + self.sp as u16, val);
        self.sp -= 1;
    }

    fn push_double(&mut self, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = val as u8;

        self.push(hi);
        self.push(lo);
    }

    fn pop(&mut self) -> u8 {
        self.sp += 1;
        self.memory.fetch(0x100 + self.sp as u16)
    }

    fn pop_double(&mut self) -> u16 {
        let lo = self.pop() as u16;
        let hi = self.pop() as u16;

        hi << 8 | lo
    }

    fn pop_p(&mut self) {
        self.p = self.pop().into();
        self.p.unset(Status::BreakCommand);
        self.p.set(Status::UnusedFlag);
    }
}

#[allow(dead_code)]
enum Status {
    CarryFlag = 0,
    ZeroFlag = 1,
    InterruptDisable = 2,
    DecimalMode = 3,
    BreakCommand = 4,
    UnusedFlag = 5,
    OverflowFlag = 6,
    NegativeFlag = 7,
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct P(u8);

impl P {
    fn new() -> Self {
        P(0x24)
    }

    fn set_if_zn(&mut self, val: u8) {
        self.set_if_zero(val);
        self.set_if_negative(val);
    }

    fn set_if_zero(&mut self, val: u8) {
        self.set_if(Status::ZeroFlag, val == 0)
    }

    fn set_if_negative(&mut self, val: u8) {
        self.set_if(Status::NegativeFlag, (val as i8) < 0)
    }

    fn set_if(&mut self, s: Status, v: bool) {
        if v {
            self.set(s);
        } else {
            self.unset(s);
        }
    }

    fn unset_if(&mut self, s: Status, v: bool) {
        self.set_if(s, !v)
    }

    fn set(&mut self, s: Status) {
        self.0 |= 1 << (s as u8);
    }

    fn unset(&mut self, s: Status) {
        self.0 &= !(1 << (s as u8));
    }

    fn is_set(&self, s: Status) -> bool {
        (self.0 >> (s as u8)) & 1 == 1
    }
}

impl From<u8> for P {
    fn from(p: u8) -> Self {
        P(p)
    }
}

impl Into<u8> for P {
    fn into(self) -> u8 {
        self.0
    }
}

impl fmt::UpperHex for P {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:02X}", self.0)
    }
}

//#[derive(Debug, PartialEq)]
pub struct CpuState<'mem, 'rom: 'mem> {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    p: P,
    sp: u8,
    cycles: usize,
    instr: Instruction,
    mem: &'mem Memory<'rom>,
}

impl<'a, 'b> fmt::Display for CpuState<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{pc:04X}  {bytecode:9} {instr:31} A:{a:02X} X:{x:02X} \
                Y:{y:02X} P:{p:02X} SP:{sp:2X} CYC:{cyc:3?}",
               pc = self.pc,
               bytecode = self.instr.bytecode(&self),
               instr = self.instr.to_string(&self),
               a = self.a,
               x = self.x,
               y = self.y,
               p = self.p,
               sp = self.sp,
               cyc = (self.cycles * 3) % 341)
    }
}

struct Memory<'rom> {
    ram: [u8; 0x2000],
    rom: Rom<'rom>,
}

impl<'rom> Memory<'rom> {
    pub fn new(rom: Rom<'rom>) -> Self {
        Memory {
            ram: [0; 0x2000],
            rom: rom,
        }
    }

    pub fn fetch<A>(&self, addr: A) -> u8
        where A: Into<u16>
    {
        let addr = addr.into() as usize;

        if addr < 0x2000 {
            self.ram[addr]
        } else {
            self.rom.prg[addr - 0xc000]
        }
    }

    pub fn fetch_double(&self, addr: u16) -> u16 {
        ((self.fetch(addr + 1) as u16) << 8) + self.fetch(addr) as u16
    }

    pub fn store<A, V>(&mut self, addr: A, val: V)
        where A: Into<u16>,
              V: Into<u8>
    {
        let addr = addr.into() as usize;
        let val = val.into();

        if addr < 0x2000 {
            self.ram[addr] = val;
        } else {
            panic!("cannot write outside handled memory");
        }
    }
}

//#[derive(Debug, PartialEq)]
//#pub enum Interrupt {
//#IrqBrk,
//#Nmi,
//#Reset,
//

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(dead_code)]
pub enum AddressingMode {
    Implied,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Instruction {
    mode: AddressingMode,
    label: Label,
    op: u8,
    addr: u16,
    size: u8,
    cycles: u8,
    page_cycles: u8,
}

impl Instruction {
    pub fn bytecode(&self, s: &CpuState) -> String {
        use self::AddressingMode::*;

        let args = match self.mode {
            Absolute => format!("{:02X} {:02X}", self.addr as u8, self.addr >> 8 as u8),
            Immediate => format!("{:02X}", s.mem.fetch(self.addr)),
            Implied | Accumulator => "".into(),
            Relative => format!("{:02X}", self.addr - s.pc - 2),
            _ => format!("{:02X}", self.addr as u8),
        };

        format!("{:02X} {:6}", self.op, args)
    }

    pub fn to_string(&self, s: &CpuState) -> String {
        use self::AddressingMode::*;
        use self::Label::*;

        let args = match (self.label, self.mode) {
            (STX, Absolute) |
            (LDX, Absolute) |
            (LDA, Absolute) => {
                format!("${:04X} = {:02X}", self.addr, s.mem.fetch(self.addr))
            }
            (_, Absolute) => format!("${:04X}", self.addr),
            (_, Immediate) => format!("#${:02X}", s.mem.fetch(self.addr)),
            (_, ZeroPage) => {
                format!("${:02X} = {:02X}", self.addr, s.mem.fetch(self.addr))
            }
            (_, Relative) => {
                format!("${:04X}", self.addr)
            }
            (_, Accumulator) => "A".into(),
            _ => "".into(),
        };

        format!("{:?} {}", self.label, args)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(dead_code, non_snake_case)]
pub enum Label {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

#[cfg(test)]
mod test {
    use rom::Rom;
    use std::fs::File;
    use std::io::{BufRead, BufReader, Read};
    use std::path::Path;
    use super::{Cpu, Status};

    #[test]
    fn nestest() {
        let buf = {
            let mut f = File::open(Path::new("test/nestest.nes")).unwrap();
            let mut buf = Vec::new();
            f.read_to_end(&mut buf).unwrap();
            buf
        };

        let (_, rom) = Rom::parse(buf.as_slice()).unwrap();

        let expected_states = {
            let f = File::open(Path::new("test/nestest.log")).unwrap();
            let reader = BufReader::new(f);
            reader.lines().collect::<Vec<_>>()
        };

        let mut cpu = Cpu::new(rom);

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

    #[test]
    fn bit_ops_on_p() {
        let mut p = super::P::new();

        p.set(Status::CarryFlag);
        assert!(p.is_set(Status::CarryFlag));

        p.unset(Status::CarryFlag);
        assert!(!p.is_set(Status::CarryFlag));
    }
}
