extern crate nes;

use clap::Parser;
use nes::Nes;
use std::path::PathBuf;

mod sdl;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to ROM to load
    rom: PathBuf,

    #[clap(short, long)]
    debug: bool,

    #[clap(short, long, default_value_t = log::LevelFilter::Info)]
    log_level: log::LevelFilter,

    #[clap(short, long, default_value_t = 2)]
    scale: u32,
}

fn main() {
    let args = Args::parse();

    init_logger(args.log_level);

    let nes = Nes::from_path(args.rom);
    sdl::run(nes, args.debug, args.scale)
}

fn init_logger(level: log::LevelFilter) {
    use simplelog::*;

    TermLogger::init(
        level,
        Config::default(),
        TerminalMode::Stderr,
        ColorChoice::Auto,
    )
    .expect("failed to init terminal logger");
}
