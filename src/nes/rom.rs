use nom::le_u8;
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mirroring {
    Horizontal,
    Vertical,
}

#[derive(Clone)]
pub struct Rom {
    pub prg: Vec<u8>,
    pub chr: Vec<u8>,
    pub mapper_id: u8,
    pub sram: [u8; 0x2000],
    pub size_prg_ram: u8,
    pub mirroring: Mirroring,
}

impl Rom {
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        let buf = fs::read(path).unwrap();
        Self::from_buf(&buf)
    }

    pub fn from_buf(buf: &[u8]) -> Self {
        let (_, rom) = Self::parse(&buf).unwrap(); // TODO: error handling
        rom
    }

    named!(
        parse<Self>,
        do_parse!(
            tag!("NES\x1A")
                >> size_prg_rom: le_u8
                >> size_chr_rom: le_u8
                >> flags_6: le_u8
                >> flags_7: le_u8
                >> size_prg_ram: le_u8
                >> _flags_9: le_u8
                >> _flags_10: le_u8
                >> take!(5)
                >> prg: take!(16_384 * size_prg_rom as usize)
                >> chr: take!(8_192 * size_chr_rom as usize)
                >> ({
                    let lo_mapper_id = flags_6 & 0b1111_0000;
                    let hi_mapper_id = flags_7 & 0b1111_0000;
                    let mapper_id = hi_mapper_id << 4 | lo_mapper_id;

                    let mirroring = if flags_6 & 1 == 1 {
                        Mirroring::Horizontal
                    } else {
                        Mirroring::Vertical
                    };

                    Self {
                        prg: prg.to_vec(),
                        chr: chr.to_vec(),
                        mapper_id,
                        sram: [0; 0x2000],
                        size_prg_ram: if size_prg_ram == 0 {
                            1
                        } else {
                            size_prg_ram
                        },
                        mirroring,
                    }
                })
        )
    );
}
