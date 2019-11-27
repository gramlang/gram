mod cli;
mod error;
mod format;
mod token;
mod tokenizer;

use crate::{
    cli::cli,
    error::{throw_reason, Error},
    format::CodeStr,
    tokenizer::tokenize,
};
use atty::Stream;
use std::{fs::read_to_string, path::PathBuf, process::exit};

// Program entrypoint
fn entry() -> Result<(), Error> {
    // Determine whether to print colored output based on whether STDOUT is connected to a
    // terminal.
    colored::control::set_override(atty::is(Stream::Stdout));

    // Parse command-line arguments.
    let matches = cli().get_matches();

    // Decide what to do based on the subcommand.
    match matches.subcommand_name() {
        // [tag:run-subcommand]
        Some(cli::RUN_SUBCOMMAND) => {
            // Read in the source file.
            let source_path = PathBuf::from(
                matches
                    .subcommand_matches(cli::RUN_SUBCOMMAND)
                    .unwrap() // [ref:run-subcommand]
                    .value_of(cli::PATH_OPTION)
                    .unwrap_or("main.g"),
            );

            let source = read_to_string(&source_path).map_err(throw_reason(format!(
                "Error when reading file {}.",
                source_path.to_string_lossy().code_str(),
            )))?;

            // Tokenize the source file.
            for token in tokenize(Some(&source_path), &source)? {
                println!("{}", token);
            }
        }

        // This branch should not be reachable due to [ref:subcommand-required].
        Some(_) | None => panic!(),
    }

    // If we made it this far, nothing went wrong.
    Ok(())
}

// Let the fun begin!
fn main() {
    // Jump to the entrypoint and report any resulting errors.
    if let Err(e) = entry() {
        eprintln!("{}", e);
        exit(1);
    }
}
