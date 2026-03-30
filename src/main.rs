mod assertions;
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
mod unifier;

use {
    crate::{
        error::{Error, throw},
        evaluator::evaluate,
        format::CodeStr,
        parser::parse,
        tokenizer::tokenize,
        type_checker::type_check,
    },
    clap::{Arg, ArgAction, Command},
    clap_complete::{Shell, generate},
    std::{fs::read_to_string, io::stdout, path::Path, process::exit, thread},
};

// The program version
const VERSION: &str = env!("CARGO_PKG_VERSION");

// The name of the program binary
const BIN_NAME: &str = "gram";

// Command-line option and subcommand names
const PATH_OPTION: &str = "path";
const CHECK_SUBCOMMAND: &str = "check";
const CHECK_SUBCOMMAND_PATH_OPTION: &str = "check-path";
const RUN_SUBCOMMAND: &str = "run";
const RUN_SUBCOMMAND_PATH_OPTION: &str = "run-path";
const SHELL_COMPLETION_SUBCOMMAND: &str = "shell-completion";
const SHELL_COMPLETION_SUBCOMMAND_SHELL_OPTION: &str = "shell-completion-shell";

// The stack size in bytes.
const STACK_SIZE: usize = 16 * 1024 * 1024; // 16 mebibytes (MiB)

// Set up the command-line interface.
fn cli() -> Command {
    Command::new("Gram")
        .version(VERSION)
        .author("Stephan Boyer <stephan@stephanboyer.com>")
        .about(
            " \
             Gram is programming language for distributed systems. Visit https://www.gram.org for \
             more information. \
             "
            .trim(),
        )
        .arg_required_else_help(true) // [tag:arg_required_else_help]
        .disable_version_flag(true)
        .next_line_help(true)
        .arg(
            Arg::new("version")
                .short('v')
                .long("version")
                .help("Print version information")
                .action(ArgAction::Version),
        )
        .arg(
            Arg::new(PATH_OPTION)
                .value_name("PATH")
                .help("Sets the path of the program entrypoint"),
        )
        .subcommand(
            Command::new(CHECK_SUBCOMMAND)
                .about("Checks a program")
                .arg(
                    Arg::new(CHECK_SUBCOMMAND_PATH_OPTION)
                        .value_name("PATH")
                        .help("Sets the path of the program entrypoint")
                        .required(true), // [tag:check_subcommand_shell_required]
                ),
        )
        .subcommand(
            Command::new(RUN_SUBCOMMAND).about("Runs a program").arg(
                Arg::new(RUN_SUBCOMMAND_PATH_OPTION)
                    .value_name("PATH")
                    .help("Sets the path of the program entrypoint")
                    .required(true), // [tag:run_subcommand_shell_required]
            ),
        )
        .subcommand(
            Command::new(SHELL_COMPLETION_SUBCOMMAND)
                .about(
                    " \
                     Prints a shell completion script. Supports Zsh, Fish, Zsh, PowerShell, and \
                     Elvish. \
                     "
                    .trim(),
                )
                .arg(
                    Arg::new(SHELL_COMPLETION_SUBCOMMAND_SHELL_OPTION)
                        .value_name("SHELL")
                        .help("Bash, Fish, Zsh, PowerShell, or Elvish")
                        .required(true), // [tag:shell_completion_subcommand_shell_required]
                ),
        )
}

// Run a program.
fn run(source_path: &Path, check_only: bool) -> Result<(), Error> {
    // Here is a helper function for mapping a `Vec<Error>` to a single `Error`.
    let collect_errors = |errors: Vec<Error>| Error {
        message: errors
            .iter()
            .fold(String::new(), |acc, error| {
                format!(
                    "{}\n{}{}",
                    acc,
                    // Only render an empty line between errors here if the previous line
                    // doesn't already visually look like an empty line. See
                    // [ref:overline_u203e].
                    if acc
                        .split('\n')
                        .next_back()
                        .unwrap()
                        .chars()
                        .all(|c| c == ' ' || c == '\u{203e}')
                    {
                        ""
                    } else {
                        "\n"
                    },
                    error,
                )
            })
            .trim()
            .to_owned(),
        reason: None,
    };

    // Read the file.
    let source_contents = read_to_string(source_path).map_err(|error| {
        throw(
            &format!(
                "Error when reading file {}.",
                source_path.to_string_lossy().code_str(),
            ),
            None,
            None,
            Some(error),
        )
    })?;

    // Tokenize the source.
    let tokens = tokenize(Some(source_path), &source_contents).map_err(collect_errors)?;

    // Parse the tokens.
    let term =
        parse(Some(source_path), &source_contents, &tokens[..], &[]).map_err(collect_errors)?;

    // Type check the term.
    let mut typing_context = vec![];
    let mut definitions_context = vec![];
    let (elaborated_term, elaborated_type) = type_check(
        Some(source_path),
        &source_contents,
        &term,
        &mut typing_context,
        &mut definitions_context,
    )
    .map_err(collect_errors)?;

    // Evaluate the term if applicable.
    if check_only {
        println!(
            "Elaborated term:\n\n{}",
            elaborated_term.to_string().code_str(),
        );
        println!(
            "\nElaborated type:\n\n{}",
            elaborated_type.to_string().code_str(),
        );
    } else {
        let value = evaluate(&elaborated_term)?;
        println!("{}", value.to_string().code_str());
    }

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
    let mut command = cli();
    generate(shell_variant, &mut command, BIN_NAME, &mut stdout());

    // If we made it this far, nothing went wrong.
    Ok(())
}

// Program entrypoint
fn entry() -> Result<(), Error> {
    // Parse command-line arguments.
    let matches = cli().get_matches();

    // Check if the user provided a path as the first argument.
    if let Some(source_path) = matches.get_one::<String>(PATH_OPTION) {
        // We got a path. Run the program at that path.
        run(Path::new(source_path), false)?;
    } else {
        // Decide what to do based on the subcommand.
        match matches.subcommand() {
            // [tag:check_subcommand]
            Some((CHECK_SUBCOMMAND, subcommand_matches)) => {
                // Determine the path to the source file.
                let source_path = Path::new(
                    subcommand_matches
                        .get_one::<String>(CHECK_SUBCOMMAND_PATH_OPTION)
                        // [ref:check_subcommand_shell_required]
                        .unwrap(),
                );

                // Check the program.
                run(source_path, true)?;
            }

            // [tag:run_subcommand]
            Some((RUN_SUBCOMMAND, subcommand_matches)) => {
                // Determine the path to the source file.
                let source_path = Path::new(
                    subcommand_matches
                        .get_one::<String>(RUN_SUBCOMMAND_PATH_OPTION)
                        // [ref:run_subcommand_shell_required]
                        .unwrap(),
                );

                // Run the program.
                run(source_path, false)?;
            }

            // [tag:shell_completion_subcommand]
            Some((SHELL_COMPLETION_SUBCOMMAND, subcommand_matches)) => {
                shell_completion(
                    subcommand_matches
                        .get_one::<String>(SHELL_COMPLETION_SUBCOMMAND_SHELL_OPTION)
                        // [ref:shell_completion_subcommand_shell_required]
                        .unwrap(),
                )?;
            }

            // We should never end up in this branch, provided we handled all the subcommands
            // above.
            Some((subcommand, _)) => panic!("Subcommand not implemented: {subcommand}."),

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
    // Run everything in a new thread with a big stack.
    thread::Builder::new()
        .stack_size(STACK_SIZE)
        .spawn(|| {
            // Jump to the entrypoint and report any resulting errors.
            if let Err(e) = entry() {
                eprintln!("{e}");
                exit(1);
            }
        })
        .unwrap_or_else(|e| {
            eprintln!("Error spawning thread: {e:?}");
            exit(1);
        })
        .join()
        .unwrap_or_else(|e| {
            eprintln!("Error joining thread: {e:?}");
            exit(1);
        });
}
