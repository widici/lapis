use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// File path to the file to run
    pub(crate) path: PathBuf,
}

pub fn parse() -> Args {
    return Args::parse();
}
