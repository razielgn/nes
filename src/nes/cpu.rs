use self::Status::*;
use instruction::AddressingMode;
use instruction::AddressingMode::*;
use instruction::Instruction;
use instruction::Label::*;
use memory::Memory;
use rom::Rom;
use std::fmt;

const BRK_VECTOR: u16 = 0xFFFE;

pub struct Cpu {
    cycles: usize,
    pc: u16,
    sp: u8,
    p: P,
    a: u8,
    x: u8,
    y: u8,
    memory: Memory,
}

impl Cpu {
    pub fn new(rom: Rom) -> Self {
        let memory = Memory::new(rom);

        Cpu {
            cycles: 0,
            pc: memory.fetch_double(0xFFFC),
            sp: 0xFD,
            p: P::new(),
            a: 0,
            x: 0,
            y: 0,
            memory: memory,
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

    pub fn jump(&mut self, addr: u16) {
        self.pc = addr;
    }

    pub fn step(&mut self) {
        let instr = self.fetch();
        let addr = instr.addr;

        self.pc += instr.size as u16;
        self.cycles += instr.cycles as usize;

        if instr.page_crossed {
            self.cycles += instr.page_cycles as usize;
        }

        match instr.label {
            JMP => {
                self.jump(addr);
            }
            JSR => {
                let pc = self.pc;
                self.push_double(pc - 1);
                self.jump(addr);
            }
            RTS => {
                let addr = self.pop_double() + 1;
                self.jump(addr);
            }
            RTI => {
                self.pop_p();

                let addr = self.pop_double();
                self.jump(addr);
            }
            PHP => self.php(),
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
            STY => {
                self.memory.store(addr, self.y);
            }
            STA => {
                self.memory.store(addr, self.a);
            }
            LAX => {
                self.a = self.memory.fetch(addr);
                self.x = self.a;
                self.p.set_if_zn(self.a);
            }
            SAX => {
                self.memory.store(addr, self.a & self.x);
            }
            NOP => {}
            SEC => {
                self.p.set(CarryFlag);
            }
            SEI => self.sei(),
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
                    self.add_branching_cycles(addr);
                    self.jump(addr);
                }
            }
            BCC => {
                if !self.p.is_set(CarryFlag) {
                    self.add_branching_cycles(addr);
                    self.jump(addr);
                }
            }
            BEQ => {
                if self.p.is_set(ZeroFlag) {
                    self.add_branching_cycles(addr);
                    self.jump(addr);
                }
            }
            BNE => {
                if !self.p.is_set(ZeroFlag) {
                    self.add_branching_cycles(addr);
                    self.jump(addr);
                }
            }
            BVS => {
                if self.p.is_set(OverflowFlag) {
                    self.add_branching_cycles(addr);
                    self.jump(addr);
                }
            }
            BVC => {
                if !self.p.is_set(OverflowFlag) {
                    self.add_branching_cycles(addr);
                    self.jump(addr);
                }
            }
            BMI => {
                if self.p.is_set(NegativeFlag) {
                    self.add_branching_cycles(addr);
                    self.jump(addr);
                }
            }
            BPL => {
                if !self.p.is_set(NegativeFlag) {
                    self.add_branching_cycles(addr);
                    self.jump(addr);
                }
            }
            BIT => {
                let m = self.memory.fetch(addr);
                let res = self.a & m;

                self.p.set_if_zero(res);
                self.p.set_if_negative(m);
                self.p.set_if(OverflowFlag, (m >> 6) & 1 == 1);
            }
            AND => self.and(instr),
            ORA => self.ora(instr),
            EOR => self.eor(instr),
            CMP => self.cmp(instr),
            CPY => {
                let m = self.memory.fetch(addr);
                let n = self.y.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.y >= m);
                self.p.set_if_zn(n);
            }
            CPX => {
                let m = self.memory.fetch(addr);
                let n = self.x.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.x >= m);
                self.p.set_if_zn(n);
            }
            ADC => self.adc(instr),
            SBC => self.sbc(instr),
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
            DEX => self.dex(),
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
            LSR => self.lsr(instr),
            ASL => self.asl(instr),
            ROR => self.ror(instr),
            ROL => self.rol(instr),
            INC => self.inc(instr),
            DEC => self.dec(instr),
            DCP => {
                self.dec(instr);
                self.cmp(instr);
            }
            ISB => {
                self.inc(instr);
                self.sbc(instr);
            }
            SLO => {
                self.asl(instr);
                self.ora(instr);
            }
            RLA => {
                self.rol(instr);
                self.and(instr);
            }
            RRA => {
                self.ror(instr);
                self.adc(instr);
            }
            SRE => {
                self.lsr(instr);
                self.eor(instr);
            }
            CLI => {
                self.p.unset(InterruptDisable);
            }
            ANC => {
                self.and(instr);
                self.p.copy(NegativeFlag, CarryFlag);
            }
            ALR => {
                self.a &= self.memory.fetch(instr.addr);
                self.p.set_if(CarryFlag, self.a & 0x01 == 1);

                self.a >>= 1;
                self.p.set_if_zero(self.a);
                self.p.unset(NegativeFlag);
            }
            ARR => {
                self.and(instr);
                self.ror_acc();

                let c = (self.a >> 6) & 1;
                self.p.set_if(CarryFlag, c == 1);
                self.p.set_if(OverflowFlag, (c ^ (self.a >> 5) & 1) == 1);
            }
            AXS => {
                let m = self.memory.fetch(instr.addr);
                let n = (self.a & self.x).wrapping_sub(m);

                self.p.set_if(CarryFlag, (self.a & self.x) >= m);
                self.p.set_if(NegativeFlag, (n as i8) < 0);
                self.p.set_if_zero(n);
                self.x = n;
            }
            SHY => {
                let (x, y) = (self.x, self.y);
                self.strange_address_write(instr, y, x);
            }
            SHX => {
                let (x, y) = (self.x, self.y);
                self.strange_address_write(instr, x, y);
            }
            BRK => {
                let pc = self.pc + 1;
                self.push_double(pc);
                self.php();
                self.sei();

                let addr = self.memory.fetch_double(BRK_VECTOR);
                self.jump(addr);
            }
            _ => panic!("can't execute {:?}", instr),
        }
    }

    fn php(&mut self) {
        let mut p = self.p;
        p.set(BreakCommand);

        self.push(p.into());
    }

    fn sei(&mut self) {
        self.p.set(InterruptDisable);
    }

    fn strange_address_write(&mut self, instr: Instruction, val: u8, idx: u8) {
        let addr = if instr.page_crossed {
            instr.addr & ((val as u16) << 8) | (instr.addr & 0x00FF)
        } else {
            instr.addr
        };

        let orig_addr = instr.addr.wrapping_sub(idx as u16);
        self.memory.store(addr, val & ((orig_addr >> 8) as u8 + 1));
    }

    fn inc(&mut self, i: Instruction) {
        let m = self.memory.fetch(i.addr).wrapping_add(1);
        self.p.set_if_zn(m);
        self.memory.store(i.addr, m);
    }

    fn dec(&mut self, i: Instruction) {
        let m = self.memory.fetch(i.addr).wrapping_sub(1);
        self.p.set_if_zn(m);
        self.memory.store(i.addr, m);
    }

    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.p.set_if_zn(self.x);
    }

    fn cmp(&mut self, i: Instruction) {
        let m = self.memory.fetch(i.addr);
        let n = self.a.wrapping_sub(m);

        self.p.set_if(CarryFlag, self.a >= m);
        self.p.set_if(NegativeFlag, (n as i8) < 0);
        self.p.set_if_zero(n);
    }

    fn sbc(&mut self, i: Instruction) {
        let m = self.memory.fetch(i.addr);
        let c = if self.p.is_set(CarryFlag) { 0 } else { 1 };
        let (sub, overflow1) = self.a.overflowing_sub(m);
        let (sub, overflow2) = sub.overflowing_sub(c);
        let overflow = overflow1 || overflow2;

        self.p.unset_if(CarryFlag, overflow);
        self.p.set_if(OverflowFlag,
                      (self.a ^ sub) & 0x80 != 0 && (self.a ^ m) & 0x80 != 0);
        self.p.set_if_zn(sub);

        self.a = sub;
    }

    fn asl(&mut self, i: Instruction) {
        if i.mode == Accumulator {
            self.p.set_if(CarryFlag, self.a >> 7 & 1 == 1);
            self.a <<= 1;
            self.p.set_if_zn(self.a);
        } else {
            let mut m = self.memory.fetch(i.addr);
            self.p.set_if(CarryFlag, m >> 7 & 1 == 1);
            m <<= 1;
            self.p.set_if_zn(m);
            self.memory.store(i.addr, m);
        }
    }

    fn ror(&mut self, i: Instruction) {
        if i.mode == Accumulator {
            self.ror_acc();
        } else {
            let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };
            let mut m = self.memory.fetch(i.addr);
            self.p.set_if(CarryFlag, m & 1 == 1);
            m = (m >> 1) | (c << 7);
            self.p.set_if_zn(m);
            self.memory.store(i.addr, m);
        }
    }

    fn ror_acc(&mut self) {
        let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };
        self.p.set_if(CarryFlag, self.a & 1 == 1);
        self.a = (self.a >> 1) | (c << 7);
        self.p.set_if_zn(self.a);
    }

    fn rol(&mut self, i: Instruction) {
        let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };

        if i.mode == Accumulator {
            self.p.set_if(CarryFlag, (self.a >> 7) & 1 == 1);
            self.a = (self.a << 1) | c;
            self.p.set_if_zn(self.a);
        } else {
            let mut m = self.memory.fetch(i.addr);
            self.p.set_if(CarryFlag, (m >> 7) & 1 == 1);
            m = (m << 1) | c;
            self.p.set_if_zn(m);
            self.memory.store(i.addr, m);
        }
    }

    fn ora(&mut self, i: Instruction) {
        self.a |= self.memory.fetch(i.addr);
        self.p.set_if_zn(self.a);
    }

    fn eor(&mut self, i: Instruction) {
        self.a ^= self.memory.fetch(i.addr);
        self.p.set_if_zn(self.a);
    }

    fn and(&mut self, i: Instruction) {
        self.a &= self.memory.fetch(i.addr);
        self.p.set_if_zn(self.a);
    }

    fn adc(&mut self, i: Instruction) {
        let m = self.memory.fetch(i.addr);
        let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };
        let (sum, overflow1) = self.a.overflowing_add(m);
        let (sum, overflow2) = sum.overflowing_add(c);
        let overflow = overflow1 || overflow2;

        self.p.set_if(CarryFlag, overflow);
        self.p.set_if(OverflowFlag,
                      (self.a ^ sum) & 0x80 != 0 && (self.a ^ m) & 0x80 == 0);
        self.p.set_if_zn(sum);

        self.a = sum;
    }

    fn lsr(&mut self, i: Instruction) {
        if i.mode == Accumulator {
            self.p.set_if(CarryFlag, self.a & 1 == 1);
            self.a >>= 1;
            self.p.set_if_zn(self.a);
        } else {
            let mut m = self.memory.fetch(i.addr);
            self.p.set_if(CarryFlag, m & 1 == 1);
            m >>= 1;
            self.p.set_if_zn(m);
            self.memory.store(i.addr, m);
        }
    }

    fn add_branching_cycles(&mut self, addr: u16) {
        self.cycles += 1;

        if pages_differ(self.pc, addr) {
            self.cycles += 1;
        }
    }

    fn fetch(&self) -> Instruction {
        let op = self.memory.fetch(self.pc);
        let (label, mode, size, cycles, page_cycles) = match op {
            0x00 => (BRK, Implied, 1, 7, 0),
            0x01 => (ORA, IndexedIndirect, 2, 6, 0),
            0x03 => (SLO, IndexedIndirect, 2, 8, 0),
            0x04 | 0x44 | 0x64 => (NOP, ZeroPage, 2, 3, 0),
            0x05 => (ORA, ZeroPage, 2, 3, 0),
            0x06 => (ASL, ZeroPage, 2, 5, 0),
            0x07 => (SLO, ZeroPage, 2, 5, 0),
            0x08 => (PHP, Implied, 1, 3, 0),
            0x09 => (ORA, Immediate, 2, 2, 0),
            0x0A => (ASL, Accumulator, 1, 2, 0),
            0x0B | 0x2B => (ANC, Immediate, 2, 2, 0),
            0x0C => (NOP, Absolute, 3, 4, 0),
            0x0D => (ORA, Absolute, 3, 4, 0),
            0x0E => (ASL, Absolute, 3, 6, 0),
            0x0F => (SLO, Absolute, 3, 6, 0),
            0x10 => (BPL, Relative, 2, 2, 2),
            0x11 => (ORA, IndirectIndexed, 2, 5, 1),
            0x13 => (SLO, IndirectIndexed, 2, 8, 0),
            0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => (NOP, ZeroPageX, 2, 4, 0),
            0x15 => (ORA, ZeroPageX, 2, 4, 0),
            0x16 => (ASL, ZeroPageX, 2, 6, 0),
            0x17 => (SLO, ZeroPageX, 2, 6, 0),
            0x18 => (CLC, Implied, 1, 2, 0),
            0x19 => (ORA, AbsoluteY, 3, 4, 1),
            0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA | 0xEA => {
                (NOP, Implied, 1, 2, 0)
            }
            0x1B => (SLO, AbsoluteY, 3, 6, 1),
            0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => (NOP, AbsoluteX, 3, 4, 1),
            0x1D => (ORA, AbsoluteX, 3, 4, 1),
            0x1E => (ASL, AbsoluteX, 3, 7, 0),
            0x1F => (SLO, AbsoluteX, 3, 6, 1),
            0x20 => (JSR, Absolute, 3, 6, 0),
            0x21 => (AND, IndexedIndirect, 2, 6, 0),
            0x23 => (RLA, IndexedIndirect, 2, 8, 0),
            0x24 => (BIT, ZeroPage, 2, 3, 0),
            0x25 => (AND, ZeroPage, 2, 3, 0),
            0x26 => (ROL, ZeroPage, 2, 5, 0),
            0x27 => (RLA, ZeroPage, 2, 5, 0),
            0x28 => (PLP, Implied, 1, 4, 0),
            0x29 => (AND, Immediate, 2, 2, 0),
            0x2A => (ROL, Accumulator, 1, 2, 0),
            0x2C => (BIT, Absolute, 3, 4, 0),
            0x2D => (AND, Absolute, 3, 4, 0),
            0x2E => (ROL, Absolute, 3, 6, 0),
            0x2F => (RLA, Absolute, 3, 6, 0),
            0x30 => (BMI, Relative, 2, 2, 2),
            0x31 => (AND, IndirectIndexed, 2, 5, 1),
            0x33 => (RLA, IndirectIndexed, 2, 8, 0),
            0x35 => (AND, ZeroPageX, 2, 4, 0),
            0x36 => (ROL, ZeroPageX, 2, 6, 0),
            0x37 => (RLA, ZeroPageX, 2, 6, 0),
            0x38 => (SEC, Implied, 1, 2, 0),
            0x39 => (AND, AbsoluteY, 3, 4, 1),
            0x3B => (RLA, AbsoluteY, 3, 6, 1),
            0x3D => (AND, AbsoluteX, 3, 4, 1),
            0x3E => (ROL, AbsoluteX, 3, 7, 0),
            0x3F => (RLA, AbsoluteX, 3, 6, 1),
            0x40 => (RTI, Implied, 1, 6, 0),
            0x41 => (EOR, IndexedIndirect, 2, 6, 0),
            0x43 => (SRE, IndexedIndirect, 2, 8, 0),
            0x45 => (EOR, ZeroPage, 2, 3, 0),
            0x46 => (LSR, ZeroPage, 2, 5, 0),
            0x47 => (SRE, ZeroPage, 2, 5, 0),
            0x48 => (PHA, Implied, 1, 3, 0),
            0x49 => (EOR, Immediate, 2, 2, 0),
            0x4A => (LSR, Accumulator, 1, 2, 0),
            0x4B => (ALR, Immediate, 2, 2, 0),
            0x4C => (JMP, Absolute, 3, 3, 0),
            0x4D => (EOR, Absolute, 3, 4, 0),
            0x4E => (LSR, Absolute, 3, 6, 0),
            0x4F => (SRE, Absolute, 3, 6, 0),
            0x50 => (BVC, Relative, 2, 2, 2),
            0x51 => (EOR, IndirectIndexed, 2, 5, 1),
            0x53 => (SRE, IndirectIndexed, 2, 8, 0),
            0x55 => (EOR, ZeroPageX, 2, 4, 0),
            0x56 => (LSR, ZeroPageX, 2, 6, 0),
            0x57 => (SRE, ZeroPageX, 2, 6, 0),
            0x58 => (CLI, Implied, 1, 2, 0),
            0x59 => (EOR, AbsoluteY, 3, 4, 1),
            0x5B => (SRE, AbsoluteY, 3, 6, 1),
            0x5D => (EOR, AbsoluteX, 3, 4, 1),
            0x5E => (LSR, AbsoluteX, 3, 7, 0),
            0x5F => (SRE, AbsoluteX, 3, 6, 1),
            0x60 => (RTS, Implied, 1, 6, 0),
            0x61 => (ADC, IndexedIndirect, 2, 6, 0),
            0x63 => (RRA, IndexedIndirect, 2, 8, 0),
            0x65 => (ADC, ZeroPage, 2, 3, 0),
            0x66 => (ROR, ZeroPage, 2, 5, 0),
            0x67 => (RRA, ZeroPage, 2, 5, 0),
            0x68 => (PLA, Implied, 1, 4, 0),
            0x69 => (ADC, Immediate, 2, 2, 0),
            0x6A => (ROR, Accumulator, 1, 2, 0),
            0x6B => (ARR, Immediate, 2, 2, 0),
            0x6C => (JMP, Indirect, 3, 5, 0),
            0x6D => (ADC, Absolute, 3, 4, 0),
            0x6E => (ROR, Absolute, 3, 6, 0),
            0x6F => (RRA, Absolute, 3, 6, 0),
            0x70 => (BVS, Relative, 2, 2, 2),
            0x71 => (ADC, IndirectIndexed, 2, 5, 1),
            0x73 => (RRA, IndirectIndexed, 2, 8, 0),
            0x75 => (ADC, ZeroPageX, 2, 4, 0),
            0x76 => (ROR, ZeroPageX, 2, 6, 0),
            0x77 => (RRA, ZeroPageX, 2, 6, 0),
            0x78 => (SEI, Implied, 1, 2, 0),
            0x79 => (ADC, AbsoluteY, 3, 4, 1),
            0x7B => (RRA, AbsoluteY, 3, 6, 1),
            0x7D => (ADC, AbsoluteX, 3, 4, 1),
            0x7E => (ROR, AbsoluteX, 3, 7, 0),
            0x7F => (RRA, AbsoluteX, 3, 6, 1),
            0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => (NOP, Immediate, 2, 2, 0),
            0x81 => (STA, IndexedIndirect, 2, 6, 0),
            0x83 => (SAX, IndexedIndirect, 2, 6, 0),
            0x84 => (STY, ZeroPage, 2, 3, 0),
            0x85 => (STA, ZeroPage, 2, 3, 0),
            0x86 => (STX, ZeroPage, 2, 3, 0),
            0x87 => (SAX, ZeroPage, 2, 3, 0),
            0x88 => (DEY, Implied, 1, 2, 0),
            0x8A => (TXA, Implied, 1, 2, 0),
            0x8C => (STY, Absolute, 3, 4, 0),
            0x8D => (STA, Absolute, 3, 4, 0),
            0x8E => (STX, Absolute, 3, 4, 0),
            0x8F => (SAX, Absolute, 3, 4, 0),
            0x90 => (BCC, Relative, 2, 2, 2),
            0x91 => (STA, IndirectIndexed, 2, 6, 0),
            0x94 => (STY, ZeroPageX, 2, 4, 0),
            0x95 => (STA, ZeroPageX, 2, 4, 0),
            0x96 => (STX, ZeroPageY, 2, 4, 0),
            0x97 => (SAX, ZeroPageY, 2, 4, 0),
            0x98 => (TYA, Implied, 1, 2, 0),
            0x99 => (STA, AbsoluteY, 3, 5, 0),
            0x9A => (TXS, Implied, 1, 2, 0),
            0x9C => (SHY, AbsoluteX, 3, 5, 1),
            0x9D => (STA, AbsoluteX, 3, 5, 0),
            0x9E => (SHX, AbsoluteY, 3, 5, 1),
            0xA0 => (LDY, Immediate, 2, 2, 0),
            0xA1 => (LDA, IndexedIndirect, 2, 6, 0),
            0xA2 => (LDX, Immediate, 2, 2, 0),
            0xA3 => (LAX, IndexedIndirect, 2, 6, 0),
            0xA4 => (LDY, ZeroPage, 2, 3, 0),
            0xA5 => (LDA, ZeroPage, 2, 3, 0),
            0xA6 => (LDX, ZeroPage, 2, 3, 0),
            0xA7 => (LAX, ZeroPage, 2, 3, 0),
            0xA8 => (TAY, Implied, 1, 2, 0),
            0xA9 => (LDA, Immediate, 2, 2, 0),
            0xAA => (TAX, Implied, 1, 2, 0),
            0xAC => (LDY, Absolute, 3, 4, 0),
            0xAB => (LAX, Immediate, 2, 2, 0),
            0xAD => (LDA, Absolute, 3, 4, 0),
            0xAE => (LDX, Absolute, 3, 4, 0),
            0xAF => (LAX, Absolute, 3, 4, 0),
            0xB0 => (BCS, Relative, 2, 2, 2),
            0xB1 => (LDA, IndirectIndexed, 2, 5, 1),
            0xB3 => (LAX, IndirectIndexed, 2, 5, 1),
            0xB4 => (LDY, ZeroPageX, 2, 4, 0),
            0xB5 => (LDA, ZeroPageX, 2, 4, 0),
            0xB6 => (LDX, ZeroPageY, 2, 4, 0),
            0xB7 => (LAX, ZeroPageY, 2, 4, 0),
            0xB8 => (CLV, Implied, 1, 2, 0),
            0xB9 => (LDA, AbsoluteY, 3, 4, 1),
            0xBA => (TSX, Implied, 1, 2, 0),
            0xBC => (LDY, AbsoluteX, 3, 4, 1),
            0xBD => (LDA, AbsoluteX, 3, 4, 1),
            0xBE => (LDX, AbsoluteY, 3, 4, 1),
            0xBF => (LAX, AbsoluteY, 3, 4, 0),
            0xC0 => (CPY, Immediate, 2, 2, 0),
            0xC1 => (CMP, IndexedIndirect, 2, 6, 0),
            0xC3 => (DCP, IndexedIndirect, 2, 8, 0),
            0xC4 => (CPY, ZeroPage, 2, 3, 0),
            0xC5 => (CMP, ZeroPage, 2, 3, 0),
            0xC6 => (DEC, ZeroPage, 2, 5, 0),
            0xC7 => (DCP, ZeroPage, 2, 5, 0),
            0xC8 => (INY, Implied, 1, 2, 0),
            0xC9 => (CMP, Immediate, 2, 2, 0),
            0xCA => (DEX, Implied, 1, 2, 0),
            0xCB => (AXS, Immediate, 2, 2, 0),
            0xCC => (CPY, Absolute, 3, 4, 0),
            0xCD => (CMP, Absolute, 3, 4, 0),
            0xCE => (DEC, Absolute, 3, 6, 0),
            0xCF => (DCP, Absolute, 3, 6, 0),
            0xD0 => (BNE, Relative, 2, 2, 2),
            0xD1 => (CMP, IndirectIndexed, 2, 5, 1),
            0xD3 => (DCP, IndirectIndexed, 2, 8, 0),
            0xD5 => (CMP, ZeroPageX, 2, 4, 0),
            0xD6 => (DEC, ZeroPageX, 2, 6, 0),
            0xD7 => (DCP, ZeroPageX, 2, 6, 0),
            0xD8 => (CLD, Implied, 1, 2, 0),
            0xD9 => (CMP, AbsoluteY, 3, 4, 1),
            0xDB => (DCP, AbsoluteY, 3, 6, 1),
            0xDD => (CMP, AbsoluteX, 3, 4, 1),
            0xDE => (DEC, AbsoluteX, 3, 7, 0),
            0xDF => (DCP, AbsoluteX, 3, 6, 1),
            0xE0 => (CPX, Immediate, 2, 2, 0),
            0xE1 => (SBC, IndexedIndirect, 2, 6, 0),
            0xE3 => (ISB, IndexedIndirect, 2, 8, 0),
            0xE4 => (CPX, ZeroPage, 2, 3, 0),
            0xE5 => (SBC, ZeroPage, 2, 3, 0),
            0xE6 => (INC, ZeroPage, 2, 5, 0),
            0xE7 => (ISB, ZeroPage, 2, 5, 0),
            0xE8 => (INX, Implied, 1, 2, 0),
            0xE9 | 0xEB => (SBC, Immediate, 2, 2, 0),
            0xEC => (CPX, Absolute, 3, 4, 0),
            0xED => (SBC, Absolute, 3, 4, 0),
            0xEE => (INC, Absolute, 3, 6, 0),
            0xEF => (ISB, Absolute, 3, 6, 0),
            0xF0 => (BEQ, Relative, 2, 2, 2),
            0xF1 => (SBC, IndirectIndexed, 2, 5, 1),
            0xF3 => (ISB, IndirectIndexed, 2, 8, 0),
            0xF5 => (SBC, ZeroPageX, 2, 4, 0),
            0xF6 => (INC, ZeroPageX, 2, 6, 0),
            0xF7 => (ISB, ZeroPageX, 2, 6, 0),
            0xF8 => (SED, Implied, 1, 2, 0),
            0xF9 => (SBC, AbsoluteY, 3, 4, 1),
            0xFB => (ISB, AbsoluteY, 3, 6, 1),
            0xFD => (SBC, AbsoluteX, 3, 4, 1),
            0xFE => (INC, AbsoluteX, 3, 7, 0),
            0xFF => (ISB, AbsoluteX, 3, 6, 1),
            op => panic!("unknown opcode 0x{:X} at addr {:04X}", op, self.pc),
        };

        let (addr, page_crossed) = self.addr_from_mode(mode);

        Instruction {
            mode: mode,
            label: label,
            op: op,
            addr: addr,
            size: size,
            cycles: cycles,
            page_crossed: page_crossed,
            page_cycles: page_cycles,
        }
    }

    fn addr_from_mode(&self, mode: AddressingMode) -> (u16, bool) {
        let addr = match mode {
            Implied | Accumulator => 0,
            Immediate => self.pc + 1,
            ZeroPage => self.memory.fetch(self.pc + 1) as u16,
            ZeroPageX => {
                self.memory.fetch(self.pc + 1).wrapping_add(self.x) as u16
            }
            ZeroPageY => {
                self.memory.fetch(self.pc + 1).wrapping_add(self.y) as u16
            }
            Relative => {
                let offset = self.memory.fetch(self.pc + 1) as u16;

                if offset < 0x80 {
                    self.pc.wrapping_add(2).wrapping_add(offset)
                } else {
                    self.pc
                        .wrapping_add(2)
                        .wrapping_add(offset)
                        .wrapping_sub(0x100)
                }
            }
            Absolute => self.memory.fetch_double(self.pc + 1),
            AbsoluteX => {
                self.memory.fetch_double(self.pc + 1).wrapping_add(self.x as u16)
            }
            AbsoluteY => {
                self.memory.fetch_double(self.pc + 1).wrapping_add(self.y as u16)
            }
            Indirect => {
                let addr = self.memory.fetch_double(self.pc + 1);
                self.memory.fetch_double_bug(addr)
            }
            IndexedIndirect => {
                let addr = self.memory.fetch(self.pc + 1).wrapping_add(self.x);
                self.memory.fetch_double_bug(addr as u16)
            }
            IndirectIndexed => {
                let addr = self.memory.fetch(self.pc + 1);
                self.memory
                    .fetch_double_bug(addr as u16)
                    .wrapping_add(self.y as u16)
            }
        };

        let page_crossed = match mode {
            AbsoluteX => pages_differ(addr.wrapping_sub(self.x as u16), addr),
            AbsoluteY | IndirectIndexed => {
                pages_differ(addr.wrapping_sub(self.y as u16), addr)
            }
            _ => false,
        };

        (addr, page_crossed)
    }

    fn push(&mut self, val: u8) {
        self.memory.store(0x100 + self.sp as u16, val);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn push_double(&mut self, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = val as u8;

        self.push(hi);
        self.push(lo);
    }

    fn pop(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
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

fn pages_differ(x: u16, y: u16) -> bool {
    x & 0xFF00 != y & 0xFF00
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
pub struct P(u8);

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

    fn copy(&mut self, from: Status, to: Status) {
        let set = self.is_set(from);
        self.set_if(to, set);
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

pub struct CpuState<'a> {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: P,
    pub sp: u8,
    pub cycles: usize,
    pub instr: Instruction,
    pub mem: &'a Memory,
}

impl<'a> fmt::Display for CpuState<'a> {
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

#[cfg(test)]
mod test {
    use super::{P, Status};

    #[test]
    fn bit_ops_on_p() {
        let mut p = P::new();

        p.set(Status::CarryFlag);
        assert!(p.is_set(Status::CarryFlag));

        p.unset(Status::CarryFlag);
        assert!(!p.is_set(Status::CarryFlag));
    }
}
