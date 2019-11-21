use nes::{Button, Nes};
use sdl2::{
    event::Event,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    rect::Rect,
    EventPump,
};
use std::time::Instant;

pub fn run(nes: Nes, debug: bool, scale: u32) {
    if debug {
        run_debug(nes, scale);
    } else {
        run_normal(nes, scale);
    }
}

fn run_normal(mut nes: Nes, scale: u32) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let screen_rect = Rect::new(0, 0, 256 * scale, 240 * scale);

    let window = video_subsystem
        .window("NES", screen_rect.width(), screen_rect.height())
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window
        .into_canvas()
        .accelerated()
        .present_vsync()
        .build()
        .unwrap();

    let texture_creator = canvas.texture_creator();
    let mut screen = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 256, 240)
        .expect("failed to create screen texture");

    let mut event_pump = sdl_context.event_pump().unwrap();

    let mut t1 = Instant::now();
    let mut frame_counter = 0usize;

    loop {
        nes.step();

        if nes.ppu_frame_ready_latch() {
            frame_counter += 1;

            if frame_counter % 10 == 0 {
                let frame_elapsed = t1.elapsed();
                canvas
                    .window_mut()
                    .set_title(&format!(
                        "NES (frame {:.1}ms)",
                        frame_elapsed.as_secs_f32() * 1000.0
                    ))
                    .expect("failed to set window title");
            }

            if handle_input(&mut nes, &mut event_pump) {
                break;
            }

            screen
                .with_lock(None, |buf, _pitch| {
                    nes.render_screen(buf).unwrap();
                })
                .expect("failed to update screen texture");

            canvas.clear();
            canvas.set_draw_color(Color::RGB(30, 30, 30));

            canvas
                .copy(&screen, None, screen_rect)
                .expect("failed to render screen texture");

            canvas.present();

            t1 = Instant::now();
        }
    }
}

fn run_debug(mut nes: Nes, scale: u32) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

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
        .position_centered()
        .build()
        .unwrap();

    let mut canvas = window
        .into_canvas()
        .accelerated()
        .present_vsync()
        .build()
        .unwrap();

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

    let mut t1 = Instant::now();
    let mut frame_counter = 0usize;

    loop {
        nes.step();

        if nes.ppu_frame_ready_latch() {
            frame_counter += 1;

            if frame_counter % 10 == 0 {
                let frame_elapsed = t1.elapsed();
                canvas
                    .window_mut()
                    .set_title(&format!(
                        "NES (frame {:.1}ms)",
                        frame_elapsed.as_secs_f32() * 1000.0
                    ))
                    .expect("failed to set window title");
            }

            if handle_input(&mut nes, &mut event_pump) {
                break;
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

            t1 = Instant::now();
        }
    }
}

fn handle_input(nes: &mut Nes, event_pump: &mut EventPump) -> bool {
    for event in event_pump.poll_iter() {
        match event {
            Event::Quit { .. }
            | Event::KeyDown {
                keycode: Some(Keycode::Escape),
                ..
            } => {
                return true;
            }
            Event::KeyDown {
                keycode: Some(Keycode::Down),
                ..
            } => {
                nes.controller_set(Button::Down);
            }
            Event::KeyUp {
                keycode: Some(Keycode::Down),
                ..
            } => {
                nes.controller_unset(Button::Down);
            }
            Event::KeyDown {
                keycode: Some(Keycode::Up),
                ..
            } => {
                nes.controller_set(Button::Up);
            }
            Event::KeyUp {
                keycode: Some(Keycode::Up),
                ..
            } => {
                nes.controller_unset(Button::Up);
            }
            Event::KeyDown {
                keycode: Some(Keycode::Right),
                ..
            } => {
                nes.controller_set(Button::Right);
            }
            Event::KeyUp {
                keycode: Some(Keycode::Right),
                ..
            } => {
                nes.controller_unset(Button::Right);
            }
            Event::KeyDown {
                keycode: Some(Keycode::Left),
                ..
            } => {
                nes.controller_set(Button::Left);
            }
            Event::KeyUp {
                keycode: Some(Keycode::Left),
                ..
            } => {
                nes.controller_unset(Button::Left);
            }
            Event::KeyUp {
                keycode: Some(Keycode::Return),
                ..
            } => {
                nes.controller_set(Button::Start);
            }
            Event::KeyDown {
                keycode: Some(Keycode::Return),
                ..
            } => {
                nes.controller_unset(Button::Start);
            }
            Event::KeyUp {
                keycode: Some(Keycode::RShift),
                ..
            } => {
                nes.controller_set(Button::Select);
            }
            Event::KeyDown {
                keycode: Some(Keycode::RShift),
                ..
            } => {
                nes.controller_unset(Button::Select);
            }
            Event::KeyUp {
                keycode: Some(Keycode::A),
                ..
            } => {
                nes.controller_set(Button::A);
            }
            Event::KeyDown {
                keycode: Some(Keycode::A),
                ..
            } => {
                nes.controller_unset(Button::A);
            }
            Event::KeyUp {
                keycode: Some(Keycode::S),
                ..
            } => {
                nes.controller_set(Button::B);
            }
            Event::KeyDown {
                keycode: Some(Keycode::S),
                ..
            } => {
                nes.controller_unset(Button::B);
            }
            Event::KeyDown {
                keycode: Some(Keycode::R),
                ..
            } => {
                nes.reset();
            }
            _ => {}
        }
    }

    false
}
