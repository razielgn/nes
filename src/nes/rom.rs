use crate::bits::BitOps;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::u8,
};
use std::{fs, path::Path};

const PRG_SIZE: usize = 0x4000;
const CHR_SIZE: usize = 0x2000;
const SRAM_SIZE: usize = 0x2000;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mirroring {
    Horizontal,
    Vertical,
}

#[derive(Clone)]
pub struct Rom {
    pub prg: Vec<u8>,
    pub prg_banks: usize,
    pub chr: Vec<u8>,
    pub chr_banks: usize,
    pub mapper_id: u8,
    pub sram: [u8; SRAM_SIZE],
    pub size_prg_ram: u8,
    pub mirroring: Mirroring,
}

impl Rom {
    #[must_use]
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        let buf = fs::read(path).expect("failed to read rom file");
        Self::from_buf(&buf)
    }

    #[must_use]
    pub fn from_buf(buf: &[u8]) -> Self {
        let (_, rom) = Self::parse(buf).expect("failed to parse rom"); // TODO: error handling
        rom
    }

    fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        let (i, _) = tag("NES\x1A")(i)?;
        let (i, prg_banks) = u8(i)?;
        let (i, chr_banks) = u8(i)?;
        let (i, flags_6) = u8(i)?;
        let (i, flags_7) = u8(i)?;
        let (i, size_prg_ram) = u8(i)?;
        let (i, _flags_9) = u8(i)?;
        let (i, _flags_10) = u8(i)?;
        let (i, _) = take(5usize)(i)?;
        let (i, prg) = take(PRG_SIZE * prg_banks as usize)(i)?;
        let (i, chr) = take(CHR_SIZE * chr_banks as usize)(i)?;

        let mirroring = if flags_6.is_bit_set(0) {
            Mirroring::Vertical
        } else {
            Mirroring::Horizontal
        };

        Ok((
            i,
            Self {
                prg_banks: prg_banks as usize,
                prg: prg.to_vec(),
                chr_banks: chr_banks as usize,
                chr: if chr.is_empty() {
                    vec![0; CHR_SIZE] // Provide if missing.
                } else {
                    chr.to_vec()
                },
                mapper_id: assemble_mapper_id(flags_6, flags_7),
                sram: [0; SRAM_SIZE],
                size_prg_ram: if size_prg_ram == 0 { 1 } else { size_prg_ram },
                mirroring,
            },
        ))
    }
}

fn assemble_mapper_id(flags_6: u8, flags_7: u8) -> u8 {
    (flags_6 >> 4) | (flags_7 & 0xF0)
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_mapper_0_with_chr() {
        let rom = Rom::from_buf(include_bytes!("../../tests/roms/nestest.nes"));

        assert_eq!(1, rom.size_prg_ram);
        assert_eq!(0, rom.mapper_id);
        assert_eq!(Mirroring::Horizontal, rom.mirroring);
        assert_eq!(1, rom.prg_banks);
        assert_eq!(1, rom.chr_banks);

        assert_eq!(PRG_SIZE, rom.prg.len());
        assert_eq!(CHR_SIZE, rom.chr.len());

        assert_eq!(&b"\x4c\xf5\xc5\x60\x78\xd8\xa2\xff"[..], &rom.prg[0..8]);
        assert_eq!(&b"\x80\x80\xff\x80\x80\x00\x00\x00"[..], &rom.chr[32..40]);
    }

    #[test]
    fn parse_mapper_0_without_chr() {
        let rom = Rom::from_buf(include_bytes!(
            "../../tests/roms/ppu_sprite_hit/timing_basics.nes"
        ));

        assert_eq!(1, rom.size_prg_ram);
        assert_eq!(0, rom.mapper_id);
        assert_eq!(Mirroring::Horizontal, rom.mirroring);

        assert_eq!(1, rom.prg_banks);
        assert_eq!(0, rom.chr_banks);

        assert_eq!(PRG_SIZE, rom.prg.len());
        assert_eq!(CHR_SIZE, rom.chr.len());
    }

    #[test]
    fn parse_mapper_1() {
        let rom = Rom::from_buf(include_bytes!(
            "../../tests/roms/instr_test_v5/all_instrs.nes"
        ));

        assert_eq!(1, rom.size_prg_ram);
        assert_eq!(1, rom.mapper_id);
        assert_eq!(Mirroring::Vertical, rom.mirroring);

        assert_eq!(16, rom.prg_banks);
        assert_eq!(0, rom.chr_banks);

        assert_eq!(PRG_SIZE * 16, rom.prg.len());
        assert_eq!(CHR_SIZE, rom.chr.len());
    }
}
