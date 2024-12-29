use std::fs::File;

use clap::Parser;
use thiserror::Error;
use tracing::{error, info};

use wyovm::Machine;

#[derive(Parser)]
struct App {
    image_path: std::path::PathBuf,
}

#[derive(Error, Debug)]
enum AppError {
    #[error("I/O Error")]
    Io(#[from] std::io::Error),
}

fn main() -> Result<(), AppError> {
    tracing_subscriber::fmt::init();
    // TODO: https://www.jmeiners.com/lc3-vm/#:setup

    let app = App::parse();
    let mut machine = Machine::new();
    machine.load_image(File::open(app.image_path)?);
    machine.run();

    info!("done!");

    Ok(())
}
