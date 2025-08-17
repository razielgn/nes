use log::{error, info, warn};
use nes::{Button, Nes};
use sdl2::{
    EventPump,
    controller::Button as ControllerButton,
    event::Event,
    gfx::framerate::FPSManager,
    keyboard::Keycode,
    pixels::{Color, PixelFormatEnum},
    rect::Rect,
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

    sdl2::hint::set("SDL_JOYSTICK_THREAD", "1");
    let game_controller_subsystem = sdl_context.game_controller().unwrap();

    let available = game_controller_subsystem.num_joysticks().unwrap_or(0);

    for id in 0..available {
        if !game_controller_subsystem.is_game_controller(id) {
            warn!("Gamepad #{} is not a game controller", id);
            continue;
        }

        info!("Attempting to open controller #{}", id);

        match game_controller_subsystem.open(id) {
            Ok(c) => {
                info!("Opened gamepad #{}: `{}`", id, c.name());
            }
            Err(e) => {
                error!("Failed to open gamepad #{}: {:?}", id, e);
            }
        }
    }

    let video_subsystem = sdl_context.video().unwrap();
    video_subsystem.text_input().stop();

    let screen_rect = Rect::new(0, 0, 256 * scale, 240 * scale);

    let mut window = video_subsystem
        .window("NES", screen_rect.width(), screen_rect.height())
        .position_centered()
        .build()
        .unwrap();

    window.raise();

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
    let mut fps_manager = FPSManager::new();
    fps_manager.set_framerate(60).expect("failed to set fps");

    loop {
        nes.step();

        if nes.ppu_frame_ready_latch() {
            if fps_manager.get_frame_count() % 30 == 0 {
                let frame_elapsed = t1.elapsed();

                canvas
                    .window_mut()
                    .set_title(&format!(
                        "NES (frame {:1}ms, FPS {})",
                        frame_elapsed.as_secs_f32() * 1000.0,
                        fps_manager.get_framerate()
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

            t1 = Instant::now();

            canvas.clear();
            canvas.set_draw_color(Color::RGB(30, 30, 30));

            canvas
                .copy(&screen, None, screen_rect)
                .expect("failed to render screen texture");

            canvas.present();

            fps_manager.delay();
        }
    }
}

fn run_debug(mut nes: Nes, scale: u32) {
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    video_subsystem.text_input().stop();

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

    let mut window = video_subsystem
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

    window.raise();

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
    let mut fps_manager = FPSManager::new();
    fps_manager.set_framerate(60).expect("failed to set fps");

    loop {
        nes.step();

        if nes.ppu_frame_ready_latch() {
            if fps_manager.get_frame_count() % 30 == 0 {
                let frame_elapsed = t1.elapsed();

                canvas
                    .window_mut()
                    .set_title(&format!(
                        "NES (frame {:1}ms, FPS {})",
                        frame_elapsed.as_secs_f32() * 1000.0,
                        fps_manager.get_framerate()
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

            t1 = Instant::now();

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

            fps_manager.delay();
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
                keycode: Some(Keycode::R),
                ..
            } => {
                nes.reset();
            }
            Event::KeyDown {
                keycode: Some(keycode),
                ..
            } => {
                if let Some(button) = map_keyboard_button(keycode) {
                    nes.controller1_set(button);
                }
            }
            Event::KeyUp {
                keycode: Some(keycode),
                ..
            } => {
                if let Some(button) = map_keyboard_button(keycode) {
                    nes.controller1_unset(button);
                }
            }
            Event::ControllerButtonDown { button, which, .. } => {
                if let Some(button) = map_controller_button(button) {
                    if which == 0 {
                        nes.controller1_set(button);
                    } else {
                        nes.controller2_set(button);
                    }
                }
            }
            Event::ControllerButtonUp { button, which, .. } => {
                if let Some(button) = map_controller_button(button) {
                    if which == 0 {
                        nes.controller1_unset(button);
                    } else {
                        nes.controller2_unset(button);
                    }
                }
            }
            _ => {}
        }
    }

    false
}

fn map_controller_button(b: ControllerButton) -> Option<Button> {
    match b {
        ControllerButton::A => Some(Button::A),
        ControllerButton::B => Some(Button::B),
        ControllerButton::DPadDown => Some(Button::Down),
        ControllerButton::DPadUp => Some(Button::Up),
        ControllerButton::DPadLeft => Some(Button::Left),
        ControllerButton::DPadRight => Some(Button::Right),
        ControllerButton::Start => Some(Button::Start),
        ControllerButton::Back => Some(Button::Select),
        _ => None,
    }
}

fn map_keyboard_button(k: Keycode) -> Option<Button> {
    match k {
        Keycode::A => Some(Button::A),
        Keycode::B => Some(Button::B),
        Keycode::Down => Some(Button::Down),
        Keycode::Up => Some(Button::Up),
        Keycode::Left => Some(Button::Left),
        Keycode::Right => Some(Button::Right),
        Keycode::Return => Some(Button::Start),
        Keycode::RShift => Some(Button::Select),
        _ => None,
    }
}
