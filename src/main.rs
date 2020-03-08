mod de_bruijn;
mod equality;
mod error;
mod evaluator;
mod format;
mod normalizer;
mod parser;
mod term;
mod token;
mod tokenizer;
mod type_checker;

use crate::{
    error::{lift, Error},
    evaluator::evaluate,
    format::CodeStr,
    parser::parse,
    tokenizer::tokenize,
    type_checker::type_check,
};
use atty::Stream;
use clap::{
    App,
    AppSettings::{ArgRequiredElseHelp, ColoredHelp, UnifiedHelpMessage, VersionlessSubcommands},
    Arg, Shell, SubCommand,
};
use std::{fs::read_to_string, io::stdout, path::Path, process::exit, rc::Rc};

// The program version
const VERSION: &str = env!("CARGO_PKG_VERSION");

// The name of the program binary
const BIN_NAME: &str = "gram";

// Command-line option and subcommand names
const PATH_OPTION: &str = "path";
const SHELL_COMPLETION_SUBCOMMAND: &str = "shell-completion";
const SHELL_COMPLETION_SUBCOMMAND_SHELL_OPTION: &str = "shell-completion-shell";
const RUN_SUBCOMMAND: &str = "run";
const RUN_SUBCOMMAND_PATH_OPTION: &str = "run-path";

// Set up the command-line interface.
fn cli<'a, 'b>() -> App<'a, 'b> {
    App::new("Gram")
        .version(VERSION)
        .version_short("v")
        .about(
            " \
             Gram is programming language for distributed systems. Visit https://www.gram.org for \
             more information. \
             "
            .trim(),
        )
        .setting(ArgRequiredElseHelp) // [tag:arg_required_else_help]
        .setting(ColoredHelp)
        .setting(UnifiedHelpMessage)
        .setting(VersionlessSubcommands)
        .arg(
            Arg::with_name(PATH_OPTION)
                .value_name("PATH")
                .help("Sets the path of the program entrypoint")
                .takes_value(true)
                .number_of_values(1),
        )
        .subcommand(
            SubCommand::with_name(RUN_SUBCOMMAND)
                .about("Runs a program")
                .arg(
                    Arg::with_name(RUN_SUBCOMMAND_PATH_OPTION)
                        .value_name("PATH")
                        .help("Sets the path of the program entrypoint")
                        .required(true) // [tag:run_subcommand_shell_required]
                        .takes_value(true)
                        .number_of_values(1),
                ),
        )
        .subcommand(
            SubCommand::with_name(SHELL_COMPLETION_SUBCOMMAND)
                .about(
                    " \
                     Prints a shell completion script. Supports Zsh, Fish, Zsh, PowerShell, and \
                     Elvish. \
                     "
                    .trim(),
                )
                .arg(
                    Arg::with_name(SHELL_COMPLETION_SUBCOMMAND_SHELL_OPTION)
                        .value_name("SHELL")
                        .help("Bash, Fish, Zsh, PowerShell, or Elvish")
                        .required(true) // [tag:shell_completion_subcommand_shell_required]
                        .takes_value(true)
                        .number_of_values(1),
                ),
        )
}

// Run a program.
fn run(source_path: &Path) -> Result<(), Error> {
    // Read the source file.
    let source_contents = read_to_string(source_path).map_err(lift(format!(
        "Error when reading file {}.",
        source_path.to_string_lossy().code_str(),
    )))?;

    // Tokenize the source file.
    let tokens = tokenize(Some(source_path), &source_contents)?;

    // Parse the term.
    let term = parse(Some(source_path), &source_contents, &tokens[..], &[])?;

    // Type check the term.
    let mut typing_context = vec![];
    let mut definitions_context = vec![];
    let _ = type_check(
        Some(source_path),
        &source_contents,
        &term,
        &mut typing_context,
        &mut definitions_context,
    )?;

    // Evaluate the term.
    let value = evaluate(Rc::new(term))?;
    println!("{}", value.to_string().code_str());

    // If we made it this far, nothing went wrong.
    Ok(())
}

// Print a shell completion script to STDOUT.
fn shell_completion(shell: &str) -> Result<(), Error> {
    // Determine which shell the user wants the shell completion for.
    let shell_variant = match shell.trim().to_lowercase().as_ref() {
        "bash" => Shell::Bash,
        "fish" => Shell::Fish,
        "zsh" => Shell::Zsh,
        "powershell" => Shell::PowerShell,
        "elvish" => Shell::Elvish,
        _ => {
            return Err(Error {
                message: format!(
                    "Unknown shell {}. Must be one of Bash, Fish, Zsh, PowerShell, or Elvish.",
                    shell.code_str(),
                ),
                reason: None,
            });
        }
    };

    // Write the script to STDOUT.
    cli().gen_completions_to(BIN_NAME, shell_variant, &mut stdout());

    // If we made it this far, nothing went wrong.
    Ok(())
}

// Program entrypoint
fn entry() -> Result<(), Error> {
    // Determine whether to print colored output based on whether STDOUT is connected to a
    // terminal.
    colored::control::set_override(atty::is(Stream::Stdout));

    // Parse command-line arguments.
    let matches = cli().get_matches();

    // Check if the user provided a path as the first argument.
    if let Some(source_path) = matches.value_of(PATH_OPTION) {
        // We got a path. Run the program at that path.
        run(Path::new(source_path))?;
    } else {
        // Decide what to do based on the subcommand.
        match matches.subcommand_name() {
            // [tag:run_subcommand]
            Some(subcommand) if subcommand == RUN_SUBCOMMAND => {
                // Determine the path to the source file.
                let source_path = Path::new(
                    matches
                        .subcommand_matches(RUN_SUBCOMMAND)
                        .unwrap() // [ref:run_subcommand]
                        .value_of(RUN_SUBCOMMAND_PATH_OPTION)
                        // [ref:run_subcommand_shell_required]
                        .unwrap(),
                );

                // Run the program.
                run(source_path)?;
            }

            // [tag:shell_completion_subcommand]
            Some(subcommand) if subcommand == SHELL_COMPLETION_SUBCOMMAND => {
                shell_completion(
                    matches
                        .subcommand_matches(SHELL_COMPLETION_SUBCOMMAND)
                        .unwrap() // [ref:shell_completion_subcommand]
                        .value_of(SHELL_COMPLETION_SUBCOMMAND_SHELL_OPTION)
                        // [ref:shell_completion_subcommand_shell_required]
                        .unwrap(),
                )?;
            }

            // We should never end up in this branch, provided we handled all the subcommands
            // above.
            Some(_) => panic!("Subcommand not implemented."),

            // If no path or subcommand was provided, the help message should have been printed
            // [ref:arg_required_else_help].
            None => panic!("The help message should have been printed."),
        }
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
