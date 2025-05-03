use crate::{
    Nes, bits::HighLowBits, instruction::AddressingMode::*,
    instruction::Label::*, memory::Access,
};
use std::fmt::{self, Display, Formatter};

pub struct DebugState {
    pub prev: Nes,
    pub curr: Nes,
}

impl Display for DebugState {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        let prev_cpu = &self.prev.cpu;
        let curr_cpu = &self.curr.cpu;
        let mem = self.prev.memory();

        let pc = prev_cpu.pc;

        let mode = curr_cpu.addr_mode;
        let label = curr_cpu.label;

        let args = mem.read_word(pc + 1);
        let read_addr = curr_cpu.op_arg;
        let read_val = mem.read(read_addr);

        let bytecode = match mode {
            Absolute | AbsoluteX(..) | AbsoluteY(..) | Indirect => {
                let (hi, lo) = args.split();
                format!("{lo:02X} {hi:02X}")
            }
            ZeroPage | Relative | Immediate | ZeroPageX | ZeroPageY
            | IndexedIndirect | IndirectIndexed(..) => {
                format!("{:02X}", args.low())
            }
            None | Implied | Accumulator => String::new(),
        };

        let mode = match mode {
            Absolute => match label {
                JMP | JSR => format!("${read_addr:04X}"),
                _ => format!("${read_addr:04X} = {read_val:02X}"),
            },
            AbsoluteX(..) | AbsoluteY(..) => {
                let reg = if let AbsoluteX(..) = mode { "X" } else { "Y" };
                format!("${args:04X},{reg} @ {read_addr:04X} = {read_val:02X}")
            }
            Accumulator => "A".into(),
            Indirect => format!("(${args:04X}) = {read_addr:04X}"),
            Immediate => format!("#${read_val:02X}"),
            ZeroPage => format!("${read_addr:02X} = {read_val:02X}"),
            ZeroPageX => format!(
                "${:02X},X @ {:02X} = {read_val:02X}",
                args.low(),
                args.low().wrapping_add(prev_cpu.x),
            ),
            ZeroPageY => format!(
                "${:02X},Y @ {:02X} = {read_val:02X}",
                args.low(),
                args.low().wrapping_add(prev_cpu.y),
            ),
            IndexedIndirect => format!(
                "(${:02X},X) @ {:02X} = {read_addr:04X} = {read_val:02X}",
                args.low(),
                args.low().wrapping_add(prev_cpu.x),
            ),
            IndirectIndexed(..) => format!(
                "(${:02X}),Y = {:04X} @ {read_addr:04X} = {read_val:02X}",
                args.low(),
                read_addr.wrapping_sub(u16::from(prev_cpu.y)),
            ),
            Relative => {
                let mut addr =
                    pc.wrapping_add(2).wrapping_add(u16::from(read_val));

                if read_val >= 0x80 {
                    addr = addr.wrapping_sub(0x100);
                }

                format!("${addr:04X}")
            }
            None | Implied => String::new(),
        };

        let p: u8 = prev_cpu.p.into();

        write!(
            f,
            "{pc:04X}  {op:02X} {bytecode:6} {label:?} {mode:27} A:{a:02X} X:{x:02X} \
             Y:{y:02X} P:{p:02X} SP:{sp:2X} CYC:{cyc:3?}",
            pc = prev_cpu.pc,
            op = curr_cpu.op,
            a = prev_cpu.a,
            x = prev_cpu.x,
            y = prev_cpu.y,
            sp = prev_cpu.sp,
            cyc = (prev_cpu.cycles * 3) % 341,
        )
    }
}
