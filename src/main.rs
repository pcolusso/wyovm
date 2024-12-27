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
    let code = std::fs::read(app.image_path)?;

    let mut machine = Machine::new();

    info!("done!");

    Ok(())
}
