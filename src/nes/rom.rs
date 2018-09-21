use nom::le_u8;
use std::fs;
use std::path::Path;

#[derive(Clone)]
pub struct Rom {
    pub flags_6: u8,      // TODO
    pub flags_7: u8,      // TODO
    pub size_prg_ram: u8, // TODO
    pub flags_9: u8,      // TODO
    pub flags_10: u8,     // TODO
    pub prg: Vec<u8>,
    pub chr: Vec<u8>,
    pub mapper: u8,
    pub sram: [u8; 0x2000],
}

impl Rom {
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        let buf = fs::read(path).unwrap();
        Rom::from_buf(&buf)
    }

    pub fn from_buf(buf: &[u8]) -> Self {
        let (_, rom) = Rom::parse(&buf).unwrap(); // TODO: error handling
        rom
    }

    named!(
        parse<Rom>,
        do_parse!(
            tag!("NES\x1A")
                >> size_prg_rom: le_u8
                >> size_chr_rom: le_u8
                >> flags_6: le_u8
                >> flags_7: le_u8
                >> size_prg_ram: le_u8
                >> flags_9: le_u8
                >> flags_10: le_u8
                >> take!(5)
                >> prg: take!(16_384 * size_prg_rom as usize)
                >> chr: take!(8_192 * size_chr_rom as usize)
                >> ({
                    let lo_mapper = flags_6 & 0x0F;
                    let hi_mapper = flags_7 & 0x78;

                    Rom {
                        flags_6,
                        flags_7,
                        size_prg_ram: if size_prg_ram == 0 {
                            1
                        } else {
                            size_prg_ram
                        },
                        flags_9,
                        flags_10,
                        prg: prg.to_vec(),
                        chr: chr.to_vec(),
                        mapper: lo_mapper | hi_mapper,
                        sram: [0; 0x2000],
                    }
                })
        )
    );
}
