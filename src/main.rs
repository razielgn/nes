extern crate nes;

use nes::Nes;
use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    rect::Rect,
};
use std::env;

fn main() {
    init_logger();

    let path = env::args().nth(1).unwrap();
    let mut nes = Nes::from_path(path);

    const CYCLES_FULL_FRAME: usize = 341 * 262;

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let scale = 4;
    let margin = 20 * scale;

    let screen_rect = Rect::new(0, 0, 256 * scale, 240 * scale);
    let chr_left_rect = Rect::new(
        screen_rect.top_right().x() + margin as i32,
        screen_rect.top_right().y(),
        128 * scale,
        128 * scale,
    );
    let chr_right_rect = Rect::new(
        chr_left_rect.x(),
        chr_left_rect.bottom_left().y() + margin as i32,
        chr_left_rect.width(),
        chr_left_rect.height(),
    );
    let palette_rect = Rect::new(
        chr_left_rect.top_right().x() + margin as i32,
        screen_rect.top_right().y(),
        16 * 4 * scale,
        16 * 8 * scale,
    );

    let window = video_subsystem
        .window(
            "NES",
            screen_rect.width()
                + margin
                + chr_left_rect.width()
                + margin
                + palette_rect.width(),
            chr_left_rect.height() + margin + chr_right_rect.width(),
        )
        .allow_highdpi()
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window.into_canvas().build().unwrap();

    let texture_creator = canvas.texture_creator();
    let mut chr_left = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 128, 128)
        .expect("failed to create chr left texture");
    let mut chr_right = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 128, 128)
        .expect("failed to create chr right texture");
    let mut palette = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 4, 8)
        .expect("failed to create palette texture");
    let mut screen = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 256, 240)
        .expect("failed to create screen texture");

    let mut event_pump = sdl_context.event_pump().unwrap();

    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => break 'running,
                _ => {}
            }
        }

        for _ in 0..CYCLES_FULL_FRAME {
            nes.step();
        }

        chr_left
            .with_lock(None, |buf, _pitch| {
                nes.render_chr_left(buf).unwrap();
            })
            .expect("failed to update chr left texture");

        chr_right
            .with_lock(None, |buf, _pitch| {
                nes.render_chr_right(buf).unwrap();
            })
            .expect("failed to update chr right texture");

        screen
            .with_lock(None, |buf, _pitch| {
                nes.render_screen(buf).unwrap();
            })
            .expect("failed to update screen texture");

        palette
            .with_lock(None, |buf, _pitch| {
                nes.render_palette(buf).unwrap();
            })
            .expect("failed to update palette texture");

        canvas.clear();
        canvas.set_draw_color(Color::RGB(30, 30, 30));

        canvas
            .copy(&chr_left, None, chr_left_rect)
            .expect("failed to render chr left texture");

        canvas
            .copy(&chr_right, None, chr_right_rect)
            .expect("failed to render chr right texture");

        canvas
            .copy(&screen, None, screen_rect)
            .expect("failed to render screen texture");

        canvas
            .copy(&palette, None, palette_rect)
            .expect("failed to render chr right texture");

        canvas.present();
    }
}

fn init_logger() {
    use simplelog::*;

    TermLogger::init(LevelFilter::Info, Config::default())
        .expect("failed to init terminal logger");
}
