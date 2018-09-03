extern crate nes;

use nes::Rom;

#[test]
fn nestest_decode() {
    let rom = Rom::from_buf(include_bytes!("roms/nestest.nes"));

    assert_eq!(0, rom.flags_6);
    assert_eq!(0, rom.flags_7);
    assert_eq!(1, rom.size_prg_ram);
    assert_eq!(0, rom.flags_9);
    assert_eq!(0, rom.flags_10);

    assert_eq!(0, rom.mapper);

    assert_eq!(16_384, rom.prg.len());
    assert_eq!(&b"\x4c\xf5\xc5\x60\x78\xd8\xa2\xff"[..], &rom.prg[0..8]);

    assert_eq!(8_192, rom.chr.len());
    assert_eq!(&b"\x80\x80\xff\x80\x80\x00\x00\x00"[..], &rom.chr[32..40]);
}
