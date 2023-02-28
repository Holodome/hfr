use clap::{Parser, Subcommand, ValueEnum};

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
        #[arg(value_enum, default_value_t = NameCanon::default())]
        canon: NameCanon,
        files: Vec<String>,
    },
    /// Change only basename
    BaseRen {
        file: Vec<String>,
        new_basename: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, ValueEnum, Default)]
enum NameCanon {
    /// snake_case
    #[default]
    SnakeCase,
    /// kebab-case
    KebabCase,
    /// PascalCase
    PascalCase,
    /// SCREAMING_SNAKE_CASE
    ScreamingSnakeCase,
    /// camelCase
    CamelCase,
}

fn main() {
    let args = Args::parse();
    println!("Hello, world!");
}
