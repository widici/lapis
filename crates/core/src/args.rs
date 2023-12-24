use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
pub(crate) struct Args {
    /// File path to the file to run
    pub(crate) path: PathBuf
}

pub(crate) fn parse() -> Args {
    return Args::parse()
}