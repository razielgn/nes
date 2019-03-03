extern crate nes;

use nes::{Access, Cycles, Nes};

macro_rules! run {
    ($path:expr) => {
        ::run_test_rom(include_bytes!($path));
    };
}

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
    #[test]
    fn basics() {
        run!("roms/instr_test_v5/01-basics.nes");
    }

    #[test]
    fn implied() {
        run!("roms/instr_test_v5/02-implied.nes");
    }

    #[test]
    fn immediate() {
        run!("roms/instr_test_v5/03-immediate.nes");
    }

    #[test]
    fn zero_page() {
        run!("roms/instr_test_v5/04-zero_page.nes");
    }

    #[test]
    fn zp_xy() {
        run!("roms/instr_test_v5/05-zp_xy.nes");
    }

    #[test]
    fn absolute() {
        run!("roms/instr_test_v5/06-absolute.nes");
    }

    #[test]
    fn abs_xy() {
        run!("roms/instr_test_v5/07-abs_xy.nes");
    }

    #[test]
    fn ind_x() {
        run!("roms/instr_test_v5/08-ind_x.nes");
    }

    #[test]
    fn ind_y() {
        run!("roms/instr_test_v5/09-ind_y.nes");
    }

    #[test]
    fn branches() {
        run!("roms/instr_test_v5/10-branches.nes");
    }

    #[test]
    #[ignore]
    fn stack() {
        run!("roms/instr_test_v5/11-stack.nes");
    }

    #[test]
    fn jmp_jsr() {
        run!("roms/instr_test_v5/12-jmp_jsr.nes");
    }

    #[test]
    fn rts() {
        run!("roms/instr_test_v5/13-rts.nes");
    }

    #[test]
    fn rti() {
        run!("roms/instr_test_v5/14-rti.nes");
    }

    #[test]
    fn brk() {
        run!("roms/instr_test_v5/15-brk.nes");
    }

    #[test]
    fn special() {
        run!("roms/instr_test_v5/16-special.nes");
    }

    #[test]
    fn all_instrs() {
        run!("roms/instr_test_v5/all_instrs.nes");
    }

    #[test]
    fn official_only() {
        run!("roms/instr_test_v5/official_only.nes");
    }
}

mod cpu_interrupts_v2 {
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
        run!("roms/cpu_interrupts_v2/cli_latency.nes");
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
        run!("roms/cpu_interrupts_v2/nmi_and_brk.nes");
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
        run!("roms/cpu_interrupts_v2/nmi_and_irq.nes");
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
        run!("roms/cpu_interrupts_v2/irq_and_dma.nes");
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
        run!("roms/cpu_interrupts_v2/branch_delays_irq.nes");
    }
}

mod instr_misc {
    /// NES CPU Instruction Behavior Misc Tests
    /// ----------------------------------------
    /// These tests verify miscellaneous instruction behavior.

    /// Verifies that $FFFF wraps around to 0 for STA abs,X and LDA abs,X.
    #[test]
    fn abs_x_wrap() {
        run!("roms/instr_misc/abs_x_wrap.nes");
    }

    /// Verifies that branching past end or before beginning of RAM wraps
    /// around.
    #[test]
    fn branch_wrap() {
        run!("roms/instr_misc/branch_wrap.nes");
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
        run!("roms/instr_misc/dummy_reads.nes");
    }

    /// Tests dummy reads for (hopefully) ALL instructions which do them,
    /// including unofficial ones. Prints opcode(s) of failed instructions.
    /// Requires that APU implement $4015 IRQ flag reading.
    #[test]
    #[ignore]
    fn dummy_reads_apu() {
        run!("roms/instr_misc/dummy_reads_apu.nes");
    }
}

mod instr_timing {
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
        run!("roms/instr_timing/instr_timing.nes");
    }

    #[test]
    #[ignore]
    fn branch_timing() {
        run!("roms/instr_timing/branch_timing.nes");
    }
}

mod cpu_reset {
    /// At power, A,X,Y=0 P=$34 S=$FD
    /// At reset, I flag set, S decreased by 3, no other change
    #[test]
    #[ignore]
    fn registers() {
        run!("roms/cpu_reset/registers.nes");
    }

    /// Verifies that reset doesn't alter any RAM.
    #[test]
    fn ram_after_reset() {
        run!("roms/cpu_reset/ram_after_reset.nes");
    }
}

/// Tests OAM reading ($2004), being sure it reads the byte from OAM at the
/// current address in $2003. It scans OAM from 0 to $FF, testing each byte
/// in sequence. It prints a '-' where it reads back from the current
/// address, and '*' where it doesn't. Each row represents 16 bytes of OAM,
/// 16 rows total.
#[test]
fn oam_read() {
    run!("roms/oam_read.nes");
}

/// Thoroughly tests OAM address ($2003) and read/write ($2004). On an NTSC
/// NES, this passes only for one of the four random PPU-CPU
/// synchronizations at power/reset. Test takes about 30 seconds, unless it
/// fails.
///
/// This test randomly sets the address, then randomly either writes a
/// random number of random bytes, or reads from the current address a
/// random number of times and verifies that it matches what's expected. It
/// does this for tens of seconds (refreshing OAM periodically so it doesn't
/// fade). Once done, it verifies that all bytes in OAM match what's
/// expected.
///
/// Expected behavior:
///
/// $2003 write sets OAM address.
///
/// $2004 write sets byte at current OAM address to byte written, then
/// increments OAM address.
///
/// $2004 read gives byte at current OAM address, without modifying OAM
/// address.
#[test]
fn oam_stress() {
    run!("roms/oam_stress.nes");
}

/// Tests behavior when reading from open-bus PPU bits/registers, those bits
/// that aren't otherwise defined. Unlike other open-bus addresses, the PPU
/// ones are separate. Takes about 5 seconds to run.
///
/// The PPU effectively has a "decay register", an 8-bit register. Each bit
/// can be refreshed with a 0 or 1. If a bit isn't refreshed with a 1 for
/// about 600 milliseconds, it will decay to 0 (some decay sooner, depending
/// on the NES and temperature).
///
/// Writing to any PPU register sets the decay register to the value
/// written. Reading from a PPU register is more complex. The following
/// shows the effect of a read from each register:
///
/// 	Addr    Open-bus bits
/// 			7654 3210
/// 	- - - - - - - - - - - - - - - -
/// 	$2000   DDDD DDDD
/// 	$2001   DDDD DDDD
/// 	$2002   ---D DDDD
/// 	$2003   DDDD DDDD
/// 	$2004   ---- ----
/// 	$2005   DDDD DDDD
/// 	$2006   DDDD DDDD
/// 	$2007   ---- ----   non-palette
/// 			DD-- ----   palette
///
/// A D means that this bit reads back as whatever is in the decay register
/// at that bit, and doesn't refresh the decay register at that bit. A -
/// means that this bit reads back as defined by the PPU, and refreshes the
/// decay register at the corresponding bit.
#[test]
fn ppu_open_bus() {
    run!("roms/ppu_open_bus.nes");
}

mod ppu_vbl_nmi {
    /// Tests basic VBL operation and VBL period.
    ///
    /// 2) VBL period is way off
    /// 3) Reading VBL flag should clear it
    /// 4) Writing $2002 shouldn't affect VBL flag
    /// 5) $2002 should be mirrored at $200A
    /// 6) $2002 should be mirrored every 8 bytes up to $2FFA
    /// 7) VBL period is too short with BG off
    /// 8) VBL period is too long with BG off
    #[test]
    fn vbl_basics() {
        run!("roms/ppu_vbl_nmi/vbl_basics.nes");
    }

    /// Verifies time VBL flag is set.
    ///
    /// Reads $2002 twice and prints VBL flags from
    /// them. Test is run one PPU clock later each time,
    /// around the time the flag is set.
    ///
    /// 00 - V
    /// 01 - V
    /// 02 - V
    /// 03 - V   ; after some resets this is - -
    /// 04 - -   ; flag setting is suppressed
    /// 05 V -
    /// 06 V -
    /// 07 V -
    /// 08 V -
    #[test]
    #[ignore]
    fn vbl_set_time() {
        run!("roms/ppu_vbl_nmi/vbl_set_time.nes");
    }

    /// Tests time VBL flag is cleared.
    ///
    /// Reads $2002 and prints VBL flag.
    /// Test is run one PPU clock later each line,
    /// around the time the flag is cleared.
    ///
    /// 00 V
    /// 01 V
    /// 02 V
    /// 03 V
    /// 04 V
    /// 05 V
    /// 06 -
    /// 07 -
    /// 08 -
    #[test]
    fn vbl_clear_time() {
        run!("roms/ppu_vbl_nmi/vbl_clear_time.nes");
    }

    /// Tests immediate NMI behavior when enabling while VBL flag is already set
    ///
    /// 2) Shouldn't occur when disabled
    /// 3) Should occur when enabled and VBL begins
    /// 4) $2000 should be mirrored every 8 bytes
    /// 5) Should occur immediately if enabled while VBL flag is set
    /// 6) Shouldn't occur if enabled while VBL flag is clear
    /// 7) Shouldn't occur again if writing $80 when already enabled
    /// 8) Shouldn't occur again if writing $80 when already enabled 2
    /// 9) Should occur again if enabling after disabled
    /// 10) Should occur again if enabling after disabled 2
    /// 11) Immediate occurence should be after NEXT instruction
    #[test]
    fn nmi_control() {
        run!("roms/ppu_vbl_nmi/nmi_control.nes");
    }

    /// Tests NMI timing.
    ///
    /// Prints which instruction NMI occurred
    /// after. Test is run one PPU clock later
    /// each line.
    ///
    /// 00 4
    /// 01 4
    /// 02 4
    /// 03 3
    /// 04 3
    /// 05 3
    /// 06 3
    /// 07 3
    /// 08 3
    /// 09 2
    #[test]
    #[ignore]
    fn nmi_timing() {
        run!("roms/ppu_vbl_nmi/nmi_timing.nes");
    }

    /// Tests behavior when $2002 is read near time
    /// VBL flag is set.
    ///
    /// Reads $2002 one PPU clock later each time.
    /// Prints whether VBL flag read back as set, and
    /// whether NMI occurred.
    ///
    /// 00 - N
    /// 01 - N
    /// 02 - N
    /// 03 - N  ; normal behavior
    /// 04 - -  ; flag never set, no NMI
    /// 05 V -  ; flag read back as set, but no NMI
    /// 06 V -
    /// 07 V N  ; normal behavior
    /// 08 V N
    /// 09 V N
    #[test]
    #[ignore]
    fn suppression() {
        run!("roms/ppu_vbl_nmi/suppression.nes");
    }

    /// Tests NMI occurrence when enabled near time
    /// VBL flag is cleared.
    ///
    /// Enables NMI one PPU clock later on each line.
    /// Prints whether NMI occurred.
    ///
    /// 00 N
    /// 01 N
    /// 02 N
    /// 03 N
    /// 04 N
    /// 05 -
    /// 06 -
    /// 07 -
    /// 08 -
    #[test]
    #[ignore]
    fn nmi_on_timing() {
        run!("roms/ppu_vbl_nmi/nmi_on_timing.nes");
    }

    /// Tests NMI occurrence when disabled near time
    /// VBL flag is set.
    ///
    /// Disables NMI one PPU clock later on each line.
    /// Prints whether NMI occurred.
    ///
    /// 03 -
    /// 04 -
    /// 05 -
    /// 06 -
    /// 07 N
    /// 08 N
    /// 09 N
    /// 0A N
    /// 0B N
    /// 0C N
    #[test]
    #[ignore]
    fn nmi_off_timing() {
        run!("roms/ppu_vbl_nmi/nmi_off_timing.nes");
    }

    /// Tests clock skipped on every other PPU frame when BG rendering
    /// is enabled.
    ///
    /// Tries pattern of BG enabled/disabled during a sequence of
    /// 5 frames, then finds how many clocks were skipped. Prints
    /// number skipped clocks to help find problems.
    ///
    /// Correct output: 00 01 01 02
    #[test]
    fn even_odd_frames() {
        run!("roms/ppu_vbl_nmi/even_odd_frames.nes");
    }

    /// Tests timing of skipped clock every other frame
    /// when BG is enabled.
    ///
    /// Output: 08 08 09 07
    ///
    /// 2) Clock is skipped too soon, relative to enabling BG
    /// 3) Clock is skipped too late, relative to enabling BG
    /// 4) Clock is skipped too soon, relative to disabling BG
    /// 5) Clock is skipped too late, relative to disabling BG
    #[test]
    fn even_odd_timing() {
        run!("roms/ppu_vbl_nmi/even_odd_timing.nes");
    }
}

const RESET_DELAY: Cycles = 310_000;

fn run_test_rom(buf: &[u8]) {
    let mut nes = Nes::from_buf(buf);
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
