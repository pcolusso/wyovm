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

    #[cfg(feature = "ui")]
    {
        use iced::Task;
        use wyovm::ui::*;

        iced::application("wyovm", State::update, State::view)
            .theme(|_s| iced::Theme::CatppuccinMacchiato)
            .settings(iced::Settings {
                default_text_size: iced::Pixels(22.0),
                default_font: iced::Font::MONOSPACE,
                ..Default::default()
            })
            .run_with(move || {
                let state = State { machine };
                (state, Task::none())
            })
            .expect("Failed to start UI");
    }

    #[cfg(not(feature = "ui"))]
    {
        machine.run();
    }

    info!("done!");

    Ok(())
}
