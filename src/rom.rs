use nom::le_u8;

#[derive(Debug, PartialEq)]
pub struct Rom<'rom> {
    pub flags_6: u8, // TODO
    pub flags_7: u8, // TODO
    pub size_prg_ram: u8, // TODO
    pub flags_9: u8, // TODO
    pub flags_10: u8, // TODO
    pub prg: &'rom [u8],
    pub chr: &'rom [u8],
}

impl<'rom> Rom<'rom> {
    named!(pub parse<Rom>, do_parse!(
        tag!("NES\x1A") >>
        size_prg_rom: le_u8 >>
        size_chr_rom: le_u8 >>
        flags_6: le_u8 >>
        flags_7: le_u8 >>
        size_prg_ram: le_u8 >>
        flags_9: le_u8 >>
        flags_10: le_u8 >>
        take!(5) >>
        prg: take!(16_384 * size_prg_rom as usize) >>
        chr: take!(8_192 * size_chr_rom as usize) >>
        eof!() >>
        (Rom {
            flags_6: flags_6,
            flags_7: flags_7,
            size_prg_ram: if size_prg_ram == 0 { 1 } else { size_prg_ram },
            flags_9: flags_7,
            flags_10: flags_10,
            prg: prg,
            chr: chr,
        })
    ));
}

#[cfg(test)]
mod test {
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;
    use super::Rom;

    #[test]
    fn nestest_decode() {
        let mut f = File::open(Path::new("test/nestest.nes")).unwrap();
        let mut buf = Vec::new();
        f.read_to_end(&mut buf).unwrap();

        let (_, rom) = Rom::parse(buf.as_slice()).unwrap();

        assert_eq!(0, rom.flags_6);
        assert_eq!(0, rom.flags_7);
        assert_eq!(1, rom.size_prg_ram);
        assert_eq!(0, rom.flags_9);
        assert_eq!(0, rom.flags_10);

        assert_eq!(16_384, rom.prg.len());
        assert_eq!(&b"\x4c\xf5\xc5\x60\x78\xd8\xa2\xff"[..], &rom.prg[0..8]);

        assert_eq!(8_192, rom.chr.len());
        assert_eq!(&b"\x80\x80\xff\x80\x80\x00\x00\x00"[..], &rom.chr[32..40]);
    }
}
