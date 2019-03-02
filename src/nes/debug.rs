use bits::HighLowBits;
use instruction::AddressingMode::*;
use instruction::Label::*;
use memory::Access;
use std::fmt::{self, Display, Formatter};
use Nes;

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
                format!("{:02X} {:02X}", lo, hi)
            }
            ZeroPage | Relative | Immediate | ZeroPageX | ZeroPageY
            | IndexedIndirect | IndirectIndexed(..) => {
                format!("{:02X}", args.low())
            }
            None | Implied | Accumulator => "".into(),
        };

        let mode = match mode {
            Absolute => match label {
                JMP | JSR => format!("${:04X}", read_addr),
                _ => format!("${:04X} = {:02X}", read_addr, read_val),
            },
            AbsoluteX(..) | AbsoluteY(..) => {
                let reg = if let AbsoluteX(..) = mode { "X" } else { "Y" };
                format!(
                    "${:04X},{} @ {:04X} = {:02X}",
                    args, reg, read_addr, read_val
                )
            }
            Accumulator => "A".into(),
            Indirect => format!("(${:04X}) = {:04X}", args, read_addr),
            Immediate => format!("#${:02X}", read_val),
            ZeroPage => format!("${:02X} = {:02X}", read_addr, read_val),
            ZeroPageX => format!(
                "${:02X},X @ {:02X} = {:02X}",
                args.low(),
                args.low().wrapping_add(prev_cpu.x),
                read_val
            ),
            ZeroPageY => format!(
                "${:02X},Y @ {:02X} = {:02X}",
                args.low(),
                args.low().wrapping_add(prev_cpu.y),
                read_val
            ),
            IndexedIndirect => format!(
                "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                args.low(),
                args.low().wrapping_add(prev_cpu.x),
                read_addr,
                read_val
            ),
            IndirectIndexed(..) => format!(
                "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                args.low(),
                read_addr.wrapping_sub(u16::from(prev_cpu.y)),
                read_addr,
                read_val
            ),
            Relative => {
                let mut addr =
                    pc.wrapping_add(2).wrapping_add(u16::from(read_val));
                if read_val >= 0x80 {
                    addr = addr.wrapping_sub(0x100);
                }

                format!("${:04X}", addr)
            }
            None | Implied => "".into(),
        };

        let p: u8 = prev_cpu.p.into();

        write!(
            f,
            "{pc:04X}  {op:02X} {bytecode:6} {label:?} {mode:27} A:{a:02X} X:{x:02X} \
             Y:{y:02X} P:{p:02X} SP:{sp:2X} CYC:{cyc:3?}",
            pc = prev_cpu.pc,
            op = curr_cpu.op,
            label = label,
            bytecode = bytecode,
            mode = mode,
            a = prev_cpu.a,
            x = prev_cpu.x,
            y = prev_cpu.y,
            p = p,
            sp = prev_cpu.sp,
            cyc = (prev_cpu.cycles * 3) % 341,
        )
    }
}
