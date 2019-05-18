use criterion::*;
use nes::Nes;

fn criterion_benchmark(c: &mut Criterion) {
    let rom = include_bytes!("../tests/roms/nestest.nes");

    c.bench_function("frame", move |b| {
        b.iter(|| {
            let mut nes = Nes::from_buf(rom);

            nes.step_frame();
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
