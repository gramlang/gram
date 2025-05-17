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
    clap::{App, AppSettings, Arg, Shell, SubCommand},
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
fn cli<'a, 'b>() -> App<'a, 'b> {
    App::new("Gram")
        .version(VERSION)
        .version_short("v")
        .author("Stephan Boyer <stephan@stephanboyer.com>")
        .about(
            " \
             Gram is programming language for distributed systems. Visit https://www.gram.org for \
             more information. \
             "
            .trim(),
        )
        .setting(AppSettings::ArgRequiredElseHelp) // [tag:arg_required_else_help]
        .setting(AppSettings::ColoredHelp)
        .setting(AppSettings::NextLineHelp)
        .setting(AppSettings::UnifiedHelpMessage)
        .setting(AppSettings::VersionlessSubcommands)
        .arg(
            Arg::with_name(PATH_OPTION)
                .value_name("PATH")
                .help("Sets the path of the program entrypoint"),
        )
        .subcommand(
            SubCommand::with_name(CHECK_SUBCOMMAND)
                .about("Checks a program")
                .arg(
                    Arg::with_name(CHECK_SUBCOMMAND_PATH_OPTION)
                        .value_name("PATH")
                        .help("Sets the path of the program entrypoint")
                        .required(true), // [tag:check_subcommand_shell_required]
                ),
        )
        .subcommand(
            SubCommand::with_name(RUN_SUBCOMMAND)
                .about("Runs a program")
                .arg(
                    Arg::with_name(RUN_SUBCOMMAND_PATH_OPTION)
                        .value_name("PATH")
                        .help("Sets the path of the program entrypoint")
                        .required(true), // [tag:run_subcommand_shell_required]
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
    cli().gen_completions_to(BIN_NAME, shell_variant, &mut stdout());

    // If we made it this far, nothing went wrong.
    Ok(())
}

// Program entrypoint
fn entry() -> Result<(), Error> {
    // Parse command-line arguments.
    let matches = cli().get_matches();

    // Check if the user provided a path as the first argument.
    if let Some(source_path) = matches.value_of(PATH_OPTION) {
        // We got a path. Run the program at that path.
        run(Path::new(source_path), false)?;
    } else {
        // Decide what to do based on the subcommand.
        match matches.subcommand_name() {
            // [tag:check_subcommand]
            Some(subcommand) if subcommand == CHECK_SUBCOMMAND => {
                // Determine the path to the source file.
                let source_path = Path::new(
                    matches
                        .subcommand_matches(CHECK_SUBCOMMAND)
                        .unwrap() // [ref:check_subcommand]
                        .value_of(CHECK_SUBCOMMAND_PATH_OPTION)
                        // [ref:check_subcommand_shell_required]
                        .unwrap(),
                );

                // Check the program.
                run(source_path, true)?;
            }

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
                run(source_path, false)?;
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
