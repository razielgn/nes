extern crate nes;

use nes::{Mirroring, Rom};

#[test]
fn nestest_decode() {
    let rom = Rom::from_buf(include_bytes!("roms/nestest.nes"));

    assert_eq!(1, rom.size_prg_ram);
    assert_eq!(0, rom.mapper_id);
    assert_eq!(Mirroring::Vertical, rom.mirroring);

    assert_eq!(16_384, rom.prg.len());
    assert_eq!(&b"\x4c\xf5\xc5\x60\x78\xd8\xa2\xff"[..], &rom.prg[0..8]);

    assert_eq!(8_192, rom.chr.len());
    assert_eq!(&b"\x80\x80\xff\x80\x80\x00\x00\x00"[..], &rom.chr[32..40]);
}
