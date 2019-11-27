mod error;
mod format;
mod token;
mod tokenizer;

use crate::{
    error::{throw_reason, Error},
    format::CodeStr,
    tokenizer::tokenize,
};
use atty::Stream;
use clap::{
    App,
    AppSettings::{
        ColoredHelp, SubcommandRequiredElseHelp, UnifiedHelpMessage, VersionlessSubcommands,
    },
    Arg, ArgMatches, SubCommand,
};
use std::{fs::read_to_string, path::PathBuf, process::exit};

// The program version
const VERSION: &str = env!("CARGO_PKG_VERSION");

// Command-line option and subcommand names
const RUN_SUBCOMMAND: &str = "run";
const PATH_OPTION: &str = "path";

// Parse command-line arguments.
fn opts<'a>() -> ArgMatches<'a> {
    App::new("Gram")
        .version(VERSION)
        .version_short("v")
        .about("")
        .about(
            " \
             Gram is high-level programming language. Visit https://www.gram.org for more \
             information. \
             "
            .trim(),
        )
        .setting(ColoredHelp)
        .setting(SubcommandRequiredElseHelp) // [tag:subcommand-required]
        .setting(UnifiedHelpMessage)
        .setting(VersionlessSubcommands)
        .subcommand(
            SubCommand::with_name(RUN_SUBCOMMAND)
                .about("Runs a program")
                .arg(
                    Arg::with_name(PATH_OPTION)
                        .value_name("PATH")
                        .help("Sets the path of the program entrypoint")
                        .takes_value(true)
                        .number_of_values(1),
                ),
        )
        .get_matches()
}

// Program entrypoint
fn entry() -> Result<(), Error> {
    // Determine whether to print colored output based on whether STDOUT is connected to a
    // terminal.
    colored::control::set_override(atty::is(Stream::Stdout));

    // Parse command-line arguments.
    let matches = opts();

    // Decide what to do based on the subcommand.
    match matches.subcommand_name() {
        // [tag:run-subcommand]
        Some(RUN_SUBCOMMAND) => {
            // Read in the source file.
            let source_path = PathBuf::from(
                matches
                    .subcommand_matches(RUN_SUBCOMMAND)
                    .unwrap() // [ref:run-subcommand]
                    .value_of(PATH_OPTION)
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
