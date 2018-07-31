extern crate nes;

use nes::{Access, Cycles, Nes};

/// Tests generally print information on screen, but also output information
/// in other ways, in case the PPU doesn't work or there isn't one, as in an
/// NSF or a NES emulator early in development.
///
/// When building as an NSF, the final result is reported as a series of
/// beeps (see below). Any important diagnostic bytes are also reported as
/// beeps, before the final result.
///
/// All text output is written starting at $6004, with a zero-byte
/// terminator at the end. As more text is written, the terminator is moved
/// forward, so an emulator can print the current text at any time.
///
/// The test status is written to $6000. $80 means the test is running, $81
/// means the test needs the reset button pressed, but delayed by at least
/// 100 msec from now. $00-$7F means the test has completed and given that
/// result code.
///
/// To allow an emulator to know when one of these tests is running and the
/// data at $6000+ is valid, as opposed to some other NES program, $DE $B0
/// $G1 is written to $6001-$6003.

mod instr_test_v5 {
    use super::*;

    #[test]
    fn basics() {
        run_test_rom("instr_test_v5/01-basics");
    }

    #[test]
    fn implied() {
        run_test_rom("instr_test_v5/02-implied");
    }

    #[test]
    fn immediate() {
        run_test_rom("instr_test_v5/03-immediate");
    }

    #[test]
    fn zero_page() {
        run_test_rom("instr_test_v5/04-zero_page");
    }

    #[test]
    fn zp_xy() {
        run_test_rom("instr_test_v5/05-zp_xy");
    }

    #[test]
    fn absolute() {
        run_test_rom("instr_test_v5/06-absolute");
    }

    #[test]
    fn abs_xy() {
        run_test_rom("instr_test_v5/07-abs_xy");
    }

    #[test]
    fn ind_x() {
        run_test_rom("instr_test_v5/08-ind_x");
    }

    #[test]
    fn ind_y() {
        run_test_rom("instr_test_v5/09-ind_y");
    }

    #[test]
    fn branches() {
        run_test_rom("instr_test_v5/10-branches");
    }

    #[test]
    #[ignore]
    fn stack() {
        run_test_rom("instr_test_v5/11-stack");
    }

    #[test]
    fn jmp_jsr() {
        run_test_rom("instr_test_v5/12-jmp_jsr");
    }

    #[test]
    fn rts() {
        run_test_rom("instr_test_v5/13-rts");
    }

    #[test]
    fn rti() {
        run_test_rom("instr_test_v5/14-rti");
    }

    #[test]
    fn brk() {
        run_test_rom("instr_test_v5/15-brk");
    }

    #[test]
    fn special() {
        run_test_rom("instr_test_v5/16-special");
    }

    #[test]
    fn all_instrs() {
        run_test_rom("instr_test_v5/all_instrs");
    }

    #[test]
    fn official_only() {
        run_test_rom("instr_test_v5/official_only");
    }
}

mod cpu_interrupts_v2 {
    use super::*;

    /// CLI Latency Summary
    /// -------------------
    /// The RTI instruction affects IRQ inhibition immediately. If an IRQ is
    /// pending and an RTI is executed that clears the I flag, the CPU will
    /// invoke the IRQ handler immediately after RTI finishes executing.
    ///
    /// The CLI, SEI, and PLP instructions effectively delay changes to the I
    /// flag until after the next instruction. For example, if an interrupt is
    /// pending and the I flag is currently set, executing CLI will execute the
    /// next instruction before the CPU invokes the IRQ handler. This delay only
    /// affects inhibition, not the value of the I flag itself; CLI followed by
    /// PHP will leave the I flag cleared in the saved status byte on the stack
    /// (bit 2), as expected.
    /// -------------------

    /// Tests the delay in CLI taking effect, and some basic aspects of IRQ
    /// handling and the APU frame IRQ (needed by the tests). It uses the APU's
    /// frame IRQ and first verifies that it works well enough for the tests.
    ///
    /// The later tests execute CLI followed by SEI and equivalent pairs of
    /// instructions (CLI, PLP, where the PLP sets the I flag). These should
    /// only allow at most one invocation of the IRQ handler, even if it doesn't
    /// acknowledge the source of the IRQ. RTI is also tested, which behaves
    /// differently. These tests also *don't* disable interrupts after the first
    /// IRQ, in order to test whether a pair of instructions allows only one
    /// interrupt or causes continuous interrupts that block the main code from
    /// continuing.
    ///
    /// 2) RTI should not adjust return address (as RTS does)
    /// 3) APU should generate IRQ when $4017 = $00
    /// 4) Exactly one instruction after CLI should execute before IRQ is taken
    /// 5) CLI SEI should allow only one IRQ just after SEI
    /// 6) In IRQ allowed by CLI SEI, I flag should be set in saved status flags
    /// 7) CLI PLP should allow only one IRQ just after PLP
    /// 8) PLP SEI should allow only one IRQ just after SEI
    /// 9) PLP PLP should allow only one IRQ just after PLP
    /// 10) CLI RTI should not allow any IRQs
    /// 11) Unacknowledged IRQ shouldn't let any mainline code run
    /// 12) RTI RTI shouldn't let any mainline code run

    #[test]
    #[ignore]
    fn cli_latency() {
        run_test_rom("cli_latency");
    }

    /// NMI behavior when it interrupts BRK. Occasionally fails on
    /// NES due to PPU-CPU synchronization.
    ///
    /// Result when run:
    /// NMI BRK --
    /// 27  36  00 NMI before CLC
    /// 26  36  00 NMI after CLC
    /// 26  36  00
    /// 36  00  00 NMI interrupting BRK, with B bit set on stack
    /// 36  00  00
    /// 36  00  00
    /// 36  00  00
    /// 36  00  00
    /// 27  36  00 NMI after SEC at beginning of IRQ handler
    /// 27  36  00

    #[test]
    #[ignore]
    fn nmi_and_brk() {
        run_test_rom("nmi_and_brk");
    }

    /// NMI behavior when it interrupts IRQ vectoring.
    ///
    /// Result when run:
    /// NMI IRQ
    /// 23  00 NMI occurs before LDA #1
    /// 21  00 NMI occurs after LDA #1 (Z flag clear)
    /// 21  00
    /// 20  00 NMI occurs after CLC, interrupting IRQ
    /// 20  00
    /// 20  00
    /// 20  00
    /// 20  00
    /// 20  00
    /// 20  00 Same result for 7 clocks before IRQ is vectored
    /// 25  20 IRQ occurs, then NMI occurs after SEC in IRQ handler
    /// 25  20

    #[test]
    #[ignore]
    fn nmi_and_irq() {
        run_test_rom("nmi_and_irq");
    }

    /// Has IRQ occur at various times around sprite DMA.
    /// First column refers to what instruction IRQ occurred
    /// after. Second column is time of IRQ, in CPU clocks relative
    /// to some arbitrary starting point.
    ///
    /// 0 +0
    /// 1 +1
    /// 1 +2
    /// 2 +3
    /// 2 +4
    /// 4 +5
    /// 4 +6
    /// 7 +7
    /// 7 +8
    /// 7 +9
    /// 7 +10
    /// 8 +11
    /// 8 +12
    /// 8 +13
    /// ...
    /// 8 +524
    /// 8 +525
    /// 8 +526
    /// 9 +527

    #[test]
    #[ignore]
    fn irq_and_dma() {
        run_test_rom("irq_and_dma");
    }

    /// A taken non-page-crossing branch ignores IRQ during
    /// its last clock, so that next instruction executes
    /// before the IRQ. Other instructions would execute the
    /// NMI before the next instruction.
    ///
    /// The same occurs for NMI, though that's not tested here.
    ///
    /// test_jmp
    /// T+ CK PC
    /// 00 02 04 NOP
    /// 01 01 04
    /// 02 03 07 JMP
    /// 03 02 07
    /// 04 01 07
    /// 05 02 08 NOP
    /// 06 01 08
    /// 07 03 08 JMP
    /// 08 02 08
    /// 09 01 08
    ///
    /// test_branch_not_taken
    /// T+ CK PC
    /// 00 02 04 CLC
    /// 01 01 04
    /// 02 02 06 BCS
    /// 03 01 06
    /// 04 02 07 NOP
    /// 05 01 07
    /// 06 04 0A JMP
    /// 07 03 0A
    /// 08 02 0A
    /// 09 01 0A JMP
    ///
    /// test_branch_taken_pagecross
    /// T+ CK PC
    /// 00 02 0D CLC
    /// 01 01 0D
    /// 02 04 00 BCC
    /// 03 03 00
    /// 04 02 00
    /// 05 01 00
    /// 06 04 03 LDA $100
    /// 07 03 03
    /// 08 02 03
    /// 09 01 03
    ///
    /// test_branch_taken
    /// T+ CK PC
    /// 00 02 04 CLC
    /// 01 01 04
    /// 02 03 07 BCC
    /// 03 02 07
    /// 04 05 0A LDA $100 *** This is the special case
    /// 05 04 0A
    /// 06 03 0A
    /// 07 02 0A
    /// 08 01 0A
    /// 09 03 0A JMP

    #[test]
    #[ignore]
    fn branch_delays_irq() {
        run_test_rom("branch_delays_irq");
    }
}

mod instr_misc {
    use super::*;

    /// NES CPU Instruction Behavior Misc Tests
    /// ----------------------------------------
    /// These tests verify miscellaneous instruction behavior.

    /// Verifies that $FFFF wraps around to 0 for STA abs,X and LDA abs,X.
    #[test]
    fn abs_x_wrap() {
        run_test_rom("abs_x_wrap");
    }

    /// Verifies that branching past end or before beginning of RAM wraps
    /// around.
    #[test]
    fn branch_wrap() {
        run_test_rom("branch_wrap");
    }

    /// Tests some instructions that do dummy reads before the real read/write.
    /// Doesn't test all instructions.
    ///
    /// Tests LDA and STA with modes (ZP,X), (ZP),Y and ABS,X
    /// Dummy reads for the following cases are tested:
    ///
    /// LDA ABS,X or (ZP),Y when carry is generated from low byte
    /// STA ABS,X or (ZP),Y
    /// ROL ABS,X always
    #[test]
    fn dummy_reads() {
        run_test_rom("dummy_reads");
    }

    /// Tests dummy reads for (hopefully) ALL instructions which do them,
    /// including unofficial ones. Prints opcode(s) of failed instructions.
    /// Requires that APU implement $4015 IRQ flag reading.
    #[test]
    #[ignore]
    fn dummy_reads_apu() {
        run_test_rom("dummy_reads_apu");
    }
}

mod instr_timing {
    use super::*;

    /// NES CPU Instruction Timing Test
    /// -------------------------------
    /// These tests verify timing of all NES CPU instructions, except the 12
    /// that freeze the CPU.
    ///
    /// The individual tests report the opcode of any failed instructions.
    /// instr_timing prints the measured and correct times. branch_timing runs
    /// the branch instruction in 8 different situations: four not taken, and
    /// four taken. For each of these four, the first two are for a
    /// non-page-cross both negative and positive, and the second two cross a
    /// page. The correct times are 2 2 2 2 3 3 4 4.

    #[test]
    #[ignore]
    fn instr_timing() {
        run_test_rom("instr_timing");
    }

    #[test]
    #[ignore]
    fn branch_timing() {
        run_test_rom("branch_timing");
    }
}

#[test]
#[ignore]
fn cpu_dummy_reads() {
    run_test_rom("cpu_dummy_reads");
}

#[test]
#[ignore]
fn cpu_timing_test6() {
    run_test_rom("cpu_timing");
}

mod cpu_reset {
    use super::*;

    /// At power, A,X,Y=0 P=$34 S=$FD
    /// At reset, I flag set, S decreased by 3, no other change
    #[test]
    #[ignore]
    fn registers() {
        run_test_rom("registers");
    }

    /// Verifies that reset doesn't alter any RAM.
    #[test]
    fn ram_after_reset() {
        run_test_rom("ram_after_reset");
    }
}

mod vbl_nmi_timing {
    use super::*;

    #[test]
    #[ignore]
    fn frame_basics() {
        run_test_rom("frame_basics");
    }
}

const RESET_DELAY: Cycles = 310_000;

fn run_test_rom(name: &str) {
    let mut nes = Nes::from_rom(format!("tests/roms/{}.nes", name));
    let mut reset_delay: Option<Cycles> = None;

    loop {
        let elapsed_cycles = nes.step();
        reset_delay = match reset_delay {
            Some(0) => {
                nes.reset();
                None
            }
            Some(n) => Some(n.saturating_sub(elapsed_cycles)),
            None => None,
        };

        let test_activity = nes.read_multi(0x6001, 3);

        if [0xDE, 0xB0, 0x61] == test_activity.as_slice() {
            match nes.read(0x6000u16) {
                0x00 => {
                    // passed
                    println!("{}", read_message(&nes));
                    break;
                }
                0x80 => {} // still running
                0x81 => {
                    if reset_delay.is_none() {
                        reset_delay = Some(RESET_DELAY);
                    }
                }
                byte => panic!("exit code: {:02X}\n{}", byte, read_message(&nes)),
            }
        }
    }
}

fn read_message(nes: &Nes) -> String {
    let mut size = 0;
    for i in 0x6004u16.. {
        if nes.read(i) == 0 {
            size = i - 0x6004;
            break;
        }
    }

    let bytes = nes.read_multi(0x6004, size);
    String::from_utf8(bytes).unwrap()
}
