use self::Status::*;
use crate::{
    bits::{BitOps, HighLowBits},
    instruction::{AddressingMode, Label, Label::*},
    mapper::Mapper,
    memory::MutAccess,
    pin::Pin,
    ppu::Ppu,
};
use log::*;
use std::{cell::RefCell, rc::Rc};

const NMI_VECTOR: u16 = 0xFFFA;
const RESET_VECTOR: u16 = 0xFFFC;
const IRQ_VECTOR: u16 = 0xFFFE;

pub struct Cpu {
    pub cycles: usize,
    pub pc: u16,
    pub sp: u8,
    pub p: P,
    pub a: u8,
    pub x: u8,
    pub y: u8,

    pub addr_mode: AddressingMode,
    pub label: Label,
    pub op: u8,
    pub op_arg: u16,

    nmi_pin: Pin,
    ppu: Rc<RefCell<Ppu>>,
    mapper: Rc<RefCell<Mapper>>,
    memory: Box<dyn MutAccess>,
}

impl Cpu {
    pub fn new(
        pc: u16,
        nmi_pin: Pin,
        memory: Box<dyn MutAccess>,
        ppu: Rc<RefCell<Ppu>>,
        mapper: Rc<RefCell<Mapper>>,
    ) -> Self {
        Self {
            cycles: 0,
            pc,
            sp: 0xFD,
            p: P::new(),
            a: 0,
            x: 0,
            y: 0,
            addr_mode: AddressingMode::None,
            op: 0,
            label: KIL,
            op_arg: 0,
            nmi_pin,
            memory,
            ppu,
            mapper,
        }
    }

    pub fn jump(&mut self, addr: u16) {
        self.pc = addr;
    }

    fn perform_interrupt(&mut self, addr: u16) {
        self.dummy_read();
        self.dummy_read();

        let pc = self.pc;
        self.push_double(pc);
        self.php();
        self.pc = self.read_word(addr);
        self.p.set(InterruptDisable);
    }

    pub fn reset(&mut self) {
        self.nmi_pin.clear();

        self.pc = self.read_word(RESET_VECTOR);
        self.p.set(InterruptDisable);
        self.sp = self.sp.wrapping_sub(3);

        self.cycles = 0;
    }

    pub fn step(&mut self) -> usize {
        trace!("step");

        let old_cycles = self.cycles;
        self.fetch_op();

        match self.label {
            JMP => {
                let addr = self.op_arg;
                self.jump(addr);
            }
            JSR => {
                let addr = self.op_arg;
                self.dummy_read();
                let pc = self.pc;
                self.push_double(pc - 1);
                self.jump(addr);
            }
            RTS => {
                let addr = self.pop_double() + 1;
                self.dummy_read();
                self.dummy_read();
                self.jump(addr);
            }
            RTI => {
                self.dummy_read();
                self.pop_p();

                let addr = self.pop_double();
                self.jump(addr);
            }
            PHP => self.php(),
            PHA => {
                self.push(self.a);
            }
            PLA => {
                self.dummy_read();
                self.a = self.pop();
                self.p.set_if_zn(self.a);
            }
            PLP => {
                self.dummy_read();
                self.pop_p();
            }
            LDA => {
                let addr = self.op_arg;
                self.a = self.read(addr);
                self.p.set_if_zn(self.a);
            }
            LDX => {
                let addr = self.op_arg;
                self.x = self.read(addr);
                self.p.set_if_zn(self.x);
            }
            LDY => {
                let addr = self.op_arg;
                self.y = self.read(addr);
                self.p.set_if_zn(self.y);
            }
            STX => {
                let addr = self.op_arg;
                self.write(addr, self.x);
            }
            STY => {
                let addr = self.op_arg;
                self.write(addr, self.y);
            }
            STA => {
                let addr = self.op_arg;
                self.write(addr, self.a);
            }
            LAX => {
                let addr = self.op_arg;
                self.a = self.read(addr);
                self.x = self.a;
                self.p.set_if_zn(self.x);
            }
            SAX => {
                let addr = self.op_arg;
                let m = self.a & self.x;
                self.write(addr, m);
            }
            NOP => self.nop(),
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
                let branch = self.p.is_set(CarryFlag);
                self.branch_relative(branch);
            }
            BCC => {
                let branch = !self.p.is_set(CarryFlag);
                self.branch_relative(branch);
            }
            BEQ => {
                let branch = self.p.is_set(ZeroFlag);
                self.branch_relative(branch);
            }
            BNE => {
                let branch = !self.p.is_set(ZeroFlag);
                self.branch_relative(branch);
            }
            BVS => {
                let branch = self.p.is_set(OverflowFlag);
                self.branch_relative(branch);
            }
            BVC => {
                let branch = !self.p.is_set(OverflowFlag);
                self.branch_relative(branch);
            }
            BMI => {
                let branch = self.p.is_set(NegativeFlag);
                self.branch_relative(branch);
            }
            BPL => {
                let branch = !self.p.is_set(NegativeFlag);
                self.branch_relative(branch);
            }
            BIT => {
                let addr = self.op_arg;
                let m = self.read(addr);
                let res = self.a & m;

                self.p.set_if_zero(res);
                self.p.set_if_negative(m);
                self.p.set_if(OverflowFlag, (m >> 6) & 1 == 1);
            }
            AND => {
                let addr = self.op_arg;
                let m = self.read(addr);
                self.and(m);
            }
            ORA => {
                let addr = self.op_arg;
                let m = self.read(addr);
                self.ora(m);
            }
            EOR => {
                let addr = self.op_arg;
                let m = self.read(addr);
                self.eor(m);
            }
            CMP => {
                let addr = self.op_arg;
                let m = self.read(addr);
                self.cmp(m)
            }
            CPY => {
                let addr = self.op_arg;
                let m = self.read(addr);
                let n = self.y.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.y >= m);
                self.p.set_if_zn(n);
            }
            CPX => {
                let addr = self.op_arg;
                let m = self.read(addr);
                let n = self.x.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.x >= m);
                self.p.set_if_zn(n);
            }
            ADC => {
                let addr = self.op_arg;
                let m = self.read(addr);
                self.adc(m);
            }
            SBC => {
                let addr = self.op_arg;
                let m = self.read(addr);
                self.sbc(m);
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
            LSR => {
                self.lsr();
            }
            ASL => {
                self.asl();
            }
            ROR => {
                self.ror();
            }
            ROL => {
                self.rol();
            }
            INC => {
                self.inc();
            }
            DEC => {
                self.dec();
            }
            DCP => {
                let m = self.dec();
                self.cmp(m);
            }
            ISB => {
                let m = self.inc();
                self.sbc(m);
            }
            SLO => {
                let m = self.asl();
                self.ora(m);
            }
            RLA => {
                let m = self.rol();
                self.and(m);
            }
            RRA => {
                let m = self.ror();
                self.adc(m);
            }
            SRE => {
                let m = self.lsr();
                self.eor(m);
            }
            CLI => {
                self.p.unset(InterruptDisable);
            }
            ANC => {
                let addr = self.op_arg;
                let m = self.read(addr);
                self.and(m);
                self.p.copy(NegativeFlag, CarryFlag);
            }
            ALR => {
                let addr = self.op_arg;
                self.a &= self.read(addr);
                self.p.set_if(CarryFlag, self.a.is_bit_set(0));

                self.a >>= 1;
                self.p.set_if_zero(self.a);
                self.p.unset(NegativeFlag);
            }
            ARR => {
                let addr = self.op_arg;
                let m = self.read(addr);
                self.and(m);
                self.ror_acc();

                let c = self.a.get_bit(6);
                self.p.set_if(CarryFlag, c == 1);
                self.p.set_if(OverflowFlag, (c ^ self.a.get_bit(5)) == 1);
            }
            AXS => {
                let addr = self.op_arg;
                let m = self.read(addr);
                let n = (self.a & self.x).wrapping_sub(m);

                self.p.set_if(CarryFlag, (self.a & self.x) >= m);
                self.p.set_if(NegativeFlag, (n as i8) < 0);
                self.p.set_if_zero(n);
                self.x = n;
            }
            SHY => {
                let (hi, lo) = self.op_arg.split();
                let val = self.y & (hi + 1);

                let addr = u16::from_hilo(val, lo);
                self.write(addr, val);
            }
            SHX => {
                let (hi, lo) = self.op_arg.split();
                let val = self.x & (hi + 1);

                let addr = u16::from_hilo(val, lo);
                self.write(addr, val);
            }
            BRK => {
                self.push_double(self.pc + 1);

                self.php();
                self.sei();

                let vector = if self.nmi_pin.is_pulled() {
                    self.nmi_pin.clear();
                    NMI_VECTOR
                } else {
                    IRQ_VECTOR
                };

                self.pc = self.read_word(vector);
            }
            label => panic!("can't execute op 0x{:02X} {:?}", self.op, label),
        }

        if self.nmi_pin.is_pulled() {
            debug!("handling NMI");
            self.perform_interrupt(NMI_VECTOR);
            self.nmi_pin.clear();
        }
        self.nmi_pin.decr_delay();

        self.cycles - old_cycles
    }

    fn nop(&mut self) {
        use crate::instruction::AddressingMode::*;

        match self.addr_mode {
            None | Accumulator | Implied | Relative => {}
            _ => {
                let addr = self.op_arg;
                let _ = self.read(addr);
            }
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

    fn inc(&mut self) -> u8 {
        let addr = self.op_arg;
        let mut m = self.read(addr);
        self.write(addr, m); // Dummy write
        m = m.wrapping_add(1);
        self.p.set_if_zn(m);
        self.write(addr, m);
        m
    }

    fn dec(&mut self) -> u8 {
        let addr = self.op_arg;
        let mut m = self.read(addr);
        self.write(addr, m); // Dummy write
        m = m.wrapping_sub(1);
        self.p.set_if_zn(m);
        self.write(addr, m);
        m
    }

    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.p.set_if_zn(self.x);
    }

    fn cmp(&mut self, m: u8) {
        let n = self.a.wrapping_sub(m);

        self.p.set_if(CarryFlag, self.a >= m);
        self.p.set_if(NegativeFlag, (n as i8) < 0);
        self.p.set_if_zero(n);
    }

    fn sbc(&mut self, m: u8) {
        let c = if self.p.is_set(CarryFlag) { 0 } else { 1 };
        let (sub, overflow1) = self.a.overflowing_sub(m);
        let (sub, overflow2) = sub.overflowing_sub(c);
        let overflow = overflow1 || overflow2;

        self.p.unset_if(CarryFlag, overflow);
        self.p.set_if(
            OverflowFlag,
            (self.a ^ sub) & 0x80 != 0 && (self.a ^ m) & 0x80 != 0,
        );
        self.p.set_if_zn(sub);

        self.a = sub;
    }

    fn asl(&mut self) -> u8 {
        if self.addr_mode == AddressingMode::Accumulator {
            self.p.set_if(CarryFlag, self.a.is_bit_set(7));
            self.a <<= 1;
            self.p.set_if_zn(self.a);
            self.a
        } else {
            let addr = self.op_arg;
            let mut m = self.read(addr);
            self.write(addr, m); // Dummy write
            self.p.set_if(CarryFlag, m.is_bit_set(7));
            m <<= 1;
            self.p.set_if_zn(m);
            self.write(addr, m);
            m
        }
    }

    fn ror(&mut self) -> u8 {
        if self.addr_mode == AddressingMode::Accumulator {
            self.ror_acc();
            0 // TODO: extract ror_addr
        } else {
            let addr = self.op_arg;
            let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };

            let mut m = self.read(addr);
            self.write(addr, m); // Dummy write

            self.p.set_if(CarryFlag, m.is_bit_set(0));
            m = (m >> 1) | (c << 7);
            self.p.set_if_zn(m);
            self.write(addr, m);
            m
        }
    }

    fn ror_acc(&mut self) {
        let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };
        self.p.set_if(CarryFlag, self.a.is_bit_set(0));
        self.a = (self.a >> 1) | (c << 7);
        self.p.set_if_zn(self.a);
    }

    fn rol(&mut self) -> u8 {
        let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };

        if self.addr_mode == AddressingMode::Accumulator {
            self.p.set_if(CarryFlag, self.a.is_bit_set(7));
            self.a = (self.a << 1) | c;
            self.p.set_if_zn(self.a);
            self.a
        } else {
            let addr = self.op_arg;
            let mut m = self.read(addr);
            self.write(addr, m); // Dummy write

            self.p.set_if(CarryFlag, m.is_bit_set(7));
            m = (m << 1) | c;
            self.p.set_if_zn(m);
            self.write(addr, m);
            m
        }
    }

    fn ora(&mut self, m: u8) {
        self.a |= m;
        self.p.set_if_zn(self.a);
    }

    fn eor(&mut self, m: u8) {
        self.a ^= m;
        self.p.set_if_zn(self.a);
    }

    fn and(&mut self, m: u8) {
        self.a &= m;
        self.p.set_if_zn(self.a);
    }

    fn adc(&mut self, m: u8) {
        let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };
        let (sum, overflow1) = self.a.overflowing_add(m);
        let (sum, overflow2) = sum.overflowing_add(c);
        let overflow = overflow1 || overflow2;

        self.p.set_if(CarryFlag, overflow);
        self.p.set_if(
            OverflowFlag,
            (self.a ^ sum) & 0x80 != 0 && (self.a ^ m) & 0x80 == 0,
        );
        self.p.set_if_zn(sum);

        self.a = sum;
    }

    fn lsr(&mut self) -> u8 {
        let addr = self.op_arg;

        if self.addr_mode == AddressingMode::Accumulator {
            self.p.set_if(CarryFlag, self.a.is_bit_set(0));
            self.a >>= 1;
            self.p.set_if_zn(self.a);
            self.a
        } else {
            let mut m = self.read(addr);
            self.write(addr, m); // Dummy write
            self.p.set_if(CarryFlag, m.is_bit_set(0));
            m >>= 1;
            self.p.set_if_zn(m);
            self.write(addr, m);
            m
        }
    }

    fn branch_relative(&mut self, branch: bool) {
        let offset = u16::from(self.read(self.op_arg));

        if branch {
            self.dummy_read();

            let prev_pc = self.pc;

            self.pc = self.pc.wrapping_add(offset);
            if offset > 0x80 {
                self.pc = self.pc.wrapping_sub(0x100);
            }

            if self.pc & 0xFF00 != prev_pc & 0xFF00 {
                trace!("page crossed during branch");
                self.dummy_read();
            }
        }
    }

    fn dummy_read(&mut self) {
        let _ = self.read(self.pc);
    }

    fn read(&mut self, addr: u16) -> u8 {
        self.inc_cycles();
        self.memory.mut_read(addr)
    }

    fn read_word(&mut self, addr: u16) -> u16 {
        self.inc_cycles();
        self.inc_cycles();
        self.memory.mut_read_word(addr)
    }

    fn read_word_indirect_hw_bug(&mut self, addr: u16) -> u16 {
        self.inc_cycles();
        self.inc_cycles();
        self.memory.mut_read_word_bug(addr)
    }

    fn read_word_page_wraparound(&mut self) -> u16 {
        let lo = self.read(0xff);
        let hi = self.read(0x00);
        u16::from_hilo(hi, lo)
    }

    fn write(&mut self, addr: u16, val: u8) {
        self.inc_cycles();

        if addr == 0x4014 {
            self.dma_transfer(val);
        } else {
            self.memory.write(addr, val);
        }
    }

    fn dma_transfer(&mut self, offset: u8) {
        if self.cycles % 2 != 0 {
            self.dummy_read();
        }

        self.dummy_read();

        for i in 0..256 {
            let val = self.read(u16::from(offset) * 0x100 + i);
            self.write(0x2004, val);
        }
    }

    fn inc_cycles(&mut self) {
        self.cycles += 1;

        self.mapper.borrow_mut().cpu_cycle();
        self.ppu.borrow_mut().cpu_cycle();
    }

    fn fetch_op(&mut self) {
        self.op = self.read(self.pc);
        self.label = self.op.into();
        self.pc += 1;

        self.addr_mode = self.op.into();
        self.op_arg_from_mode();
    }

    fn op_arg_from_mode(&mut self) {
        use crate::instruction::AddressingMode::*;

        let pc = self.pc;
        self.op_arg = match self.addr_mode {
            None => 0,
            Accumulator | Implied => {
                self.dummy_read();
                0
            }
            Immediate | Relative => {
                self.pc += 1;
                pc
            }
            ZeroPage => {
                let val = self.read(pc);
                self.pc += 1;
                u16::from(val)
            }
            ZeroPageX => {
                let val = self.read(pc);
                self.pc += 1;
                let _ = self.read(u16::from(val));
                u16::from(val.wrapping_add(self.x))
            }
            ZeroPageY => {
                let val = self.read(pc);
                self.pc += 1;
                let _ = self.read(u16::from(val));
                u16::from(val.wrapping_add(self.y))
            }
            Indirect => {
                let addr = self.read_word(pc);
                self.pc += 2;
                self.read_word_indirect_hw_bug(addr)
            }
            Absolute => {
                let addr = self.read_word(pc);
                self.pc += 2;
                addr
            }
            IndexedIndirect => {
                let mut zero = self.read(pc);
                self.pc += 1;
                let _ = self.read(u16::from(zero)); // Dummy read

                zero = zero.wrapping_add(self.x);
                if zero == 0xff {
                    self.read_word_page_wraparound()
                } else {
                    self.read_word(u16::from(zero))
                }
            }
            IndirectIndexed(dummy_read) => {
                let zero = self.read(pc);
                self.pc += 1;

                let addr = if zero == 0xff {
                    trace!("zero page wraparound");
                    self.read_word_page_wraparound()
                } else {
                    self.read_word(u16::from(zero))
                };

                let page_crossed = is_page_crossed(addr, u16::from(self.y));
                if page_crossed || dummy_read {
                    let dummy_read_addr = if page_crossed {
                        addr.wrapping_add(u16::from(self.y)).wrapping_sub(0x100)
                    } else {
                        addr.wrapping_add(u16::from(self.y))
                    };

                    let _ = self.read(dummy_read_addr);
                }

                addr.wrapping_add(u16::from(self.y))
            }
            AbsoluteX(dummy_read) => {
                let addr = self.read_word(pc);
                self.pc += 2;

                let page_crossed = is_page_crossed(addr, u16::from(self.x));
                if page_crossed || dummy_read {
                    let dummy_read_addr = if page_crossed {
                        addr.wrapping_add(u16::from(self.x)).wrapping_sub(0x100)
                    } else {
                        addr.wrapping_add(u16::from(self.x))
                    };

                    let _ = self.read(dummy_read_addr);
                }

                addr.wrapping_add(u16::from(self.x))
            }
            AbsoluteY(dummy_read) => {
                let addr = self.read_word(pc);
                self.pc += 2;

                let page_crossed = is_page_crossed(addr, u16::from(self.y));
                if page_crossed || dummy_read {
                    let dummy_read_addr = if page_crossed {
                        addr.wrapping_add(u16::from(self.y)).wrapping_sub(0x100)
                    } else {
                        addr.wrapping_add(u16::from(self.y))
                    };

                    let _ = self.read(dummy_read_addr);
                }

                addr.wrapping_add(u16::from(self.y))
            }
        };
    }

    fn push(&mut self, val: u8) {
        let addr = 0x100_u16.wrapping_add(u16::from(self.sp));
        self.write(addr, val);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn push_double(&mut self, val: u16) {
        self.push(val.high());
        self.push(val.low());
    }

    fn pop(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let addr = 0x100_u16.wrapping_add(u16::from(self.sp));
        self.read(addr)
    }

    fn pop_double(&mut self) -> u16 {
        let lo = self.pop();
        let hi = self.pop();
        u16::from_hilo(hi, lo)
    }

    fn pop_p(&mut self) {
        self.p = self.pop().into();
        self.p.unset(Status::BreakCommand);
        self.p.set(Status::UnusedFlag);
    }
}

fn is_page_crossed(a: u16, b: u16) -> bool {
    a.wrapping_add(b) & 0xFF00 != a & 0xFF00
}

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
        self.0.set_bit(s as u8);
    }

    fn unset(&mut self, s: Status) {
        self.0.clear_bit(s as u8);
    }

    fn is_set(self, s: Status) -> bool {
        self.0.is_bit_set(s as u8)
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::instruction::AddressingMode::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn bit_ops_on_p() {
        let mut p = P::new();

        p.set(CarryFlag);
        assert!(p.is_set(CarryFlag));

        p.unset(CarryFlag);
        assert!(!p.is_set(CarryFlag));
    }

    #[test]
    fn nop_implied() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xEA, 0x00];
        cpu.step(&mut m);

        assert_eq!(0xEA, cpu.op);
        assert_eq!(Implied, cpu.addr_mode);
        assert_eq!(0, cpu.op_arg);
        assert_eq!(1, cpu.pc);
        assert_eq!(2, cpu.cycles);
    }

    #[test]
    fn asl_accumulator() {
        let mut cpu = Cpu::with_pc(0);
        cpu.a = 0x02;
        let mut m = vec![0x0A, 0x00];
        cpu.step(&mut m);

        assert_eq!(0x0A, cpu.op);
        assert_eq!(Accumulator, cpu.addr_mode);
        assert_eq!(0, cpu.op_arg);
        assert_eq!(1, cpu.pc);
        assert_eq!(2, cpu.cycles);
        assert_eq!(0x04, cpu.a);
    }

    #[test]
    fn lda_immediate() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xA9, 0xEF];
        cpu.step(&mut m);

        assert_eq!(0xA9, cpu.op);
        assert_eq!(Immediate, cpu.addr_mode);
        assert_eq!(1, cpu.op_arg);
        assert_eq!(2, cpu.pc);
        assert_eq!(2, cpu.cycles);
        assert_eq!(0xEF, cpu.a);
        assert!(cpu.p.is_set(NegativeFlag));
    }

    #[test]
    fn lda_zero_page() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xA5, 0x02, 0xFF];
        cpu.step(&mut m);

        assert_eq!(0xA5, cpu.op);
        assert_eq!(ZeroPage, cpu.addr_mode);
        assert_eq!(2, cpu.op_arg);
        assert_eq!(2, cpu.pc);
        assert_eq!(0xFF, cpu.a);
        assert_eq!(3, cpu.cycles);
    }

    #[test]
    fn lda_zero_page_x() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xB5, 0x02, 0x00, 0xFF];
        cpu.x = 1;
        cpu.step(&mut m);

        assert_eq!(0xB5, cpu.op);
        assert_eq!(ZeroPageX, cpu.addr_mode);
        assert_eq!(3, cpu.op_arg);
        assert_eq!(2, cpu.pc);
        assert_eq!(0xFF, cpu.a);
        assert_eq!(4, cpu.cycles);
    }

    #[test]
    fn ldx_zero_page_y() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xB6, 0x02, 0x00, 0xFF];
        cpu.y = 1;
        cpu.step(&mut m);

        assert_eq!(0xB6, cpu.op);
        assert_eq!(ZeroPageY, cpu.addr_mode);
        assert_eq!(3, cpu.op_arg);
        assert_eq!(2, cpu.pc);
        assert_eq!(0xFF, cpu.x);
        assert_eq!(4, cpu.cycles);
    }

    #[test]
    fn jmp_indirect() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0x6C, 0x04, 0x00, 0x00, 0xFF, 0x00];
        cpu.step(&mut m);

        assert_eq!(0x6C, cpu.op);
        assert_eq!(Indirect, cpu.addr_mode);
        assert_eq!(0xFF, cpu.op_arg);
        assert_eq!(0xFF, cpu.pc);
        assert_eq!(5, cpu.cycles);
    }

    #[test]
    fn lda_absolute() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xAD, 0x04, 0x00, 0x00, 0xFF];
        cpu.step(&mut m);

        assert_eq!(0xAD, cpu.op);
        assert_eq!(Absolute, cpu.addr_mode);
        assert_eq!(4, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(0xFF, cpu.a);
        assert_eq!(4, cpu.cycles);
    }

    #[test]
    fn lda_absolute_x() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xBD, 0x04, 0x00, 0x00, 0x00, 0xFF];
        cpu.x = 1;
        cpu.step(&mut m);

        assert_eq!(0xBD, cpu.op);
        assert_eq!(AbsoluteX(false), cpu.addr_mode);
        assert_eq!(5, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(0xFF, cpu.a);
        assert_eq!(4, cpu.cycles);
    }

    #[test]
    fn lda_absolute_x_page_cross() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xBD, 0x02, 0x01];
        m.resize(0x202, 0);
        m[0x201] = 0xFF;
        cpu.x = 0xFF;
        cpu.step(&mut m);

        assert_eq!(0xBD, cpu.op);
        assert_eq!(AbsoluteX(false), cpu.addr_mode);
        assert_eq!(0x201, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(0xFF, cpu.a);
        assert_eq!(5, cpu.cycles);
    }

    #[test]
    fn rol_absolute_x_dummy_read() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0x3E, 0x04, 0x00, 0x00, 0x00, 0b01010101];
        cpu.x = 1;
        cpu.step(&mut m);

        assert_eq!(0x3E, cpu.op);
        assert_eq!(AbsoluteX(true), cpu.addr_mode);
        assert_eq!(5, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(7, cpu.cycles);
        assert_eq!(0b10101010, m[5]);
    }

    #[test]
    fn lda_absolute_y() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xB9, 0x04, 0x00, 0x00, 0x00, 0xFF];
        cpu.y = 1;
        cpu.step(&mut m);

        assert_eq!(0xB9, cpu.op);
        assert_eq!(AbsoluteY(false), cpu.addr_mode);
        assert_eq!(5, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(0xFF, cpu.a);
        assert_eq!(4, cpu.cycles);
    }

    #[test]
    fn lda_absolute_y_page_cross() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xB9, 0x02, 0x01];
        m.resize(0x202, 0);
        m[0x201] = 0xFF;
        cpu.y = 0xFF;
        cpu.step(&mut m);

        assert_eq!(0xB9, cpu.op);
        assert_eq!(AbsoluteY(false), cpu.addr_mode);
        assert_eq!(0x201, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(0xFF, cpu.a);
        assert_eq!(5, cpu.cycles);
    }

    #[test]
    fn sta_absolute_y_dummy_read() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0x99, 0x04, 0x00, 0x00, 0x00, 0x00];
        cpu.y = 1;
        cpu.a = 0xFF;
        cpu.step(&mut m);

        assert_eq!(0x99, cpu.op);
        assert_eq!(AbsoluteY(true), cpu.addr_mode);
        assert_eq!(5, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(5, cpu.cycles);
        assert_eq!(0xFF, m[5]);
    }

    #[test]
    fn bpl_relative_branch() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0x10, 0x02, 0x00, 0x00];
        cpu.step(&mut m);

        assert_eq!(0x10, cpu.op);
        assert_eq!(Relative, cpu.addr_mode);
        assert_eq!(0x01, cpu.op_arg);
        assert_eq!(0x04, cpu.pc);
        assert_eq!(3, cpu.cycles);
    }

    #[test]
    fn bpl_relative_not_branch() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0x10, 0x02, 0x00, 0x00];
        cpu.p.set(NegativeFlag);
        cpu.step(&mut m);

        assert_eq!(0x10, cpu.op);
        assert_eq!(Relative, cpu.addr_mode);
        assert_eq!(0x01, cpu.op_arg);
        assert_eq!(0x02, cpu.pc);
        assert_eq!(2, cpu.cycles);
    }

    #[test]
    fn lda_indexed_indirect() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xA1, 0x02, 0x00, 0x05, 0x00, 0xFF];
        cpu.x = 1;
        cpu.step(&mut m);

        assert_eq!(0xA1, cpu.op);
        assert_eq!(IndexedIndirect, cpu.addr_mode);
        assert_eq!(0x05, cpu.op_arg);
        assert_eq!(2, cpu.pc);
        assert_eq!(6, cpu.cycles);
        assert_eq!(0xFF, cpu.a);
    }

    #[test]
    fn lda_indexed_indirect_page_wraparound() {
        let mut cpu = Cpu::with_pc(1);
        let mut m = vec![0xDE, 0xA1, 0x01];
        m.resize(0xFFFF, 0);
        m[0xFF] = 0xAD;
        m[0xDEAD] = 0xFF;
        cpu.x = 0xFE;
        cpu.step(&mut m);

        assert_eq!(0xA1, cpu.op);
        assert_eq!(IndexedIndirect, cpu.addr_mode);
        assert_eq!(0xDEAD, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(6, cpu.cycles);
        assert_eq!(0xFF, cpu.a);
    }

    #[test]
    fn lda_indirect_indexed() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xB1, 0x02, 0x03, 0x00, 0xFF];
        cpu.y = 1;
        cpu.step(&mut m);

        assert_eq!(0xB1, cpu.op);
        assert_eq!(IndirectIndexed(false), cpu.addr_mode);
        assert_eq!(0x04, cpu.op_arg);
        assert_eq!(2, cpu.pc);
        assert_eq!(5, cpu.cycles);
        assert_eq!(0xFF, cpu.a);
    }

    #[test]
    fn lda_indirect_indexed_page_wraparound() {
        let mut cpu = Cpu::with_pc(1);
        let mut m = vec![0x00, 0xB1, 0xFF];
        m.resize(0x100, 0);
        m[0xFF] = 0x04;
        m[0x05] = 0xFF;
        cpu.y = 1;
        cpu.step(&mut m);

        assert_eq!(0xB1, cpu.op);
        assert_eq!(IndirectIndexed(false), cpu.addr_mode);
        assert_eq!(0x05, cpu.op_arg);
        assert_eq!(3, cpu.pc);
        assert_eq!(5, cpu.cycles);
        assert_eq!(0xFF, cpu.a);
    }

    #[test]
    fn lda_indirect_indexed_page_crossing() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0xB1, 0x02, 0x02, 0x01];
        m.resize(0x202, 0);
        m[0x201] = 0xFF;
        cpu.y = 0xFF;
        cpu.step(&mut m);

        assert_eq!(0xB1, cpu.op);
        assert_eq!(IndirectIndexed(false), cpu.addr_mode);
        assert_eq!(0x201, cpu.op_arg);
        assert_eq!(2, cpu.pc);
        assert_eq!(6, cpu.cycles);
        assert_eq!(0xFF, cpu.a);
    }

    #[test]
    fn sta_indirect_indexed_dummy_read() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0x91, 0x02, 0x00, 0x01];
        m.resize(0x102, 0);
        cpu.y = 1;
        cpu.a = 0xFF;
        cpu.step(&mut m);

        assert_eq!(0x91, cpu.op);
        assert_eq!(IndirectIndexed(true), cpu.addr_mode);
        assert_eq!(0x101, cpu.op_arg);
        assert_eq!(2, cpu.pc);
        assert_eq!(6, cpu.cycles);
        assert_eq!(cpu.a, m[0x101]);
    }

    #[test]
    fn reset() {
        let mut cpu = Cpu::with_pc(0);
        let mut m = vec![0u8; 0xFFFF];
        m[0xFFFC] = 0xAD;
        m[0xFFFD] = 0xDE;
        cpu.reset(&mut m);

        assert_eq!(0xDEAD, cpu.pc);
        assert_eq!(0xFA, cpu.sp);
        assert!(cpu.p.is_set(InterruptDisable));
        assert_eq!(0, cpu.cycles);
    }
}
