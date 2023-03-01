use clap::{Parser, Subcommand};
mod cases;

use cases::Case;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Replace string in filenames
    Replace {
        old: String,
        new: String,
        files: Vec<String>,
    },
    /// Change file extensions
    ExtRen {
        old_ext: String,
        new_ext: String,
        files: Vec<String>,
    },
    /// Canonicalize name
    NameCanon {
        #[arg(value_enum, default_value_t = Case::default())]
        canon: Case,
        files: Vec<String>,
    },
    /// Change only basename
    BaseRen {
        file: Vec<String>,
        new_basename: String,
    },
}

fn main() {
    let args = Args::parse();
    println!("Hello, world!");
}
