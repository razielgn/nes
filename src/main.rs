extern crate nes;

use clap::{
    crate_authors, crate_description, crate_name, crate_version, App, Arg,
};
use nes::Nes;
use std::{fs::File, str::FromStr};

mod sdl;

fn cli_app<'a, 'b>() -> App<'a, 'b> {
    App::new(crate_name!())
        .author(crate_authors!())
        .version(crate_version!())
        .about(crate_description!())
        .arg(
            Arg::with_name("ROM")
                .required(true)
                .takes_value(true)
                .validator(|s| {
                    File::open(&s).map(|_| ()).map_err(|e| e.to_string())
                })
                .help("Path to ROM to load"),
        )
        .arg(
            Arg::with_name("debug")
                .long("debug")
                .short("d")
                .help("Turns on debugging mode"),
        )
        .arg(
            Arg::with_name("log_level")
                .long("log-level")
                .short("l")
                .possible_values(&["error", "warn", "info", "debug", "trace"])
                .default_value("info")
                .help("Sets logging level"),
        )
        .arg(
            Arg::with_name("scale")
                .group("video")
                .long("scale")
                .short("s")
                .default_value("2")
                .validator(|s| {
                    u32::from_str(&s).map(|_| ()).map_err(|e| e.to_string())
                })
                .help("Sets video scale"),
        )
}

fn main() {
    let app = cli_app();
    let matches = app.get_matches();

    init_logger(matches.value_of("log_level").unwrap());

    let path = matches.value_of("ROM").unwrap();
    let debug = matches.is_present("debug");
    let scale = u32::from_str(matches.value_of("scale").unwrap()).unwrap();

    let nes = Nes::from_path(path);
    sdl::run(nes, debug, scale)
}

fn init_logger(level: &str) {
    use simplelog::*;

    let level_filter = match level {
        "error" => LevelFilter::Error,
        "warn" => LevelFilter::Warn,
        "info" => LevelFilter::Info,
        "debug" => LevelFilter::Debug,
        "trace" => LevelFilter::Trace,
        _ => LevelFilter::Off,
    };

    TermLogger::init(
        level_filter,
        Config::default(),
        TerminalMode::Stderr,
        ColorChoice::Auto,
    )
    .expect("failed to init terminal logger");
}
