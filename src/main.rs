mod ast;
mod de_bruijn;
mod error;
mod format;
mod normalizer;
mod parser;
mod token;
mod tokenizer;
mod type_checker;

use crate::{
    ast::Node,
    error::{lift, Error},
    format::CodeStr,
    normalizer::normalize,
    parser::parse,
    tokenizer::tokenize,
    type_checker::{type_check, TYPE},
};
use atty::Stream;
use clap::{
    App,
    AppSettings::{ColoredHelp, UnifiedHelpMessage, VersionlessSubcommands},
    Arg, Shell, SubCommand,
};
use std::{
    borrow::Borrow, collections::HashMap, fs::read_to_string, io::stdout, path::Path,
    process::exit, rc::Rc,
};

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

// The default path to the entrypoint.
const DEFAULT_ENTRYPOINT_PATH: &str = "main.g";

// Set up the command-line interface.
fn cli<'a, 'b>() -> App<'a, 'b> {
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
                        .required(true) // [tag:shell-completion-subcommand-shell-required]
                        .takes_value(true)
                        .number_of_values(1),
                ),
        )
}

// Run a program.
fn run<T: Borrow<Path>>(source_path: T) -> Result<(), Error> {
    // Read the source file.
    let source_contents = read_to_string(source_path.borrow()).map_err(lift(format!(
        "Error when reading file {}.",
        source_path.borrow().to_string_lossy().code_str(),
    )))?;

    // Tokenize the source file.
    let tokens = tokenize(Some(source_path.borrow()), &source_contents)?;

    // Print the tokens.
    println!("# Tokens:\n");

    for token in &tokens {
        println!("{}", token);
    }

    println!();

    // Construct a hash table to represent the variables in scope during parsing.
    let mut parsing_context = HashMap::<&str, usize>::new();
    parsing_context.insert(TYPE, 0);

    // Parse the source file.
    let node = parse(
        Some(source_path.borrow()),
        &source_contents,
        &tokens[..],
        &mut parsing_context,
    )?;

    // Print the AST.
    println!("# Term:\n\n{}\n", node);

    // Construct a vector to represent the variables in scope during type checking.
    let mut type_checking_context = vec![Rc::new(Node {
        source_range: None,
        group: false,
        variant: ast::Variant::Variable(TYPE, 0),
    })];

    // Normalize the term.
    let normal_form = normalize(Some(source_path.borrow()), &source_contents, &node);

    // Print the normal form.
    println!("# Normal form:\n\n{}\n", normal_form);

    // Type check the AST.
    let node_type = type_check(
        Some(source_path.borrow()),
        &source_contents,
        &node,
        &mut type_checking_context,
    )?;

    // Print the type.
    println!("# Type:\n\n{}", node_type);

    // If we made it this far, nothing went wrong.
    Ok(())
}

// Print a shell completion script to STDOUT.
fn shell_completion<T: Borrow<str>>(shell: T) -> Result<(), Error> {
    // Determine which shell the user wants the shell completion for.
    let shell_variant = match shell.borrow().trim().to_lowercase().as_ref() {
        "bash" => Shell::Bash,
        "fish" => Shell::Fish,
        "zsh" => Shell::Zsh,
        "powershell" => Shell::PowerShell,
        "elvish" => Shell::Elvish,
        _ => {
            return Err(Error {
                message: format!(
                    "Unknown shell {}. Must be one of Bash, Fish, Zsh, PowerShell, or Elvish.",
                    shell.borrow().code_str(),
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
            // [tag:run-subcommand]
            Some(subcommand) if subcommand == RUN_SUBCOMMAND => {
                // Determine the path to the source file.
                let source_path = Path::new(
                    matches
                        .subcommand_matches(RUN_SUBCOMMAND)
                        .unwrap() // [ref:run-subcommand]
                        .value_of(RUN_SUBCOMMAND_PATH_OPTION)
                        .unwrap_or(DEFAULT_ENTRYPOINT_PATH),
                );

                // Run the program.
                run(source_path)?;
            }

            // [tag:shell-completion-subcommand]
            Some(subcommand) if subcommand == SHELL_COMPLETION_SUBCOMMAND => {
                shell_completion(
                    matches
                        .subcommand_matches(SHELL_COMPLETION_SUBCOMMAND)
                        .unwrap() // [ref:shell-completion-subcommand]
                        .value_of(SHELL_COMPLETION_SUBCOMMAND_SHELL_OPTION)
                        .unwrap(), // [ref:shell-completion-subcommand-shell-required]
                )?;
            }

            // We should never end up in this branch, provided we handled all the subcommands
            // above.
            Some(_) => panic!(),

            // If no path or subcommand was provided, look for a program at the default path.
            None => run(Path::new(DEFAULT_ENTRYPOINT_PATH))?,
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
