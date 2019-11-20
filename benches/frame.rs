use criterion::*;
use nes::Nes;

fn criterion_benchmark(c: &mut Criterion) {
    let rom = include_bytes!("../tests/roms/nestest.nes");

    c.bench_function("ppu tile rendering", move |b| {
        b.iter(|| {
            let mut nes = Nes::from_buf(rom);

            while !nes.ppu_frame_ready_latch() {
                nes.step();
            }
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
