mod assertions;
mod de_bruijn;
mod equality;

// Retain the complete shared error API even though Gram does not consume editor source ranges.
#[allow(dead_code)]
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

use crate::{
    error::{Error, format_errors},
    evaluator::evaluate,
    format::{CodePath, CodeStr},
    parser::parse,
    tokenizer::tokenize,
    type_checker::type_check,
};
use clap::{ArgAction, Args, CommandFactory, Parser, Subcommand as ClapSubcommand};
use clap_complete::{Shell, generate};
use std::{
    fs::read_to_string,
    io::stdout,
    path::{Path, PathBuf},
    process::exit,
    rc::Rc,
    thread,
};

// The name of the program binary
const BIN_NAME: &str = "gram";

// The stack size in bytes.
const STACK_SIZE: usize = 16 * 1024 * 1024; // 16 mebibytes (MiB)

const ABOUT: &str = concat!(
    env!("CARGO_PKG_DESCRIPTION"),
    "\n\n",
    "More information can be found at: ",
    env!("CARGO_PKG_HOMEPAGE"),
);

// This struct represents the command-line arguments.
#[derive(Parser)]
#[command(
    about = ABOUT,
    version,
    arg_required_else_help = true, // [tag:arg_required_else_help]
    disable_version_flag = true
)]
struct Cli {
    #[arg(short, long, help = "Print version", action = ArgAction::Version)]
    _version: Option<bool>,

    #[arg(help = "Set the path to the program entrypoint")]
    path: Option<PathBuf>,

    #[command(subcommand)]
    command: Option<GramCommand>,
}

#[derive(Args)]
struct ProgramPathArg {
    #[arg(help = "Set the path to the program entrypoint")]
    path: PathBuf,
}

#[derive(Args)]
struct ShellCompletionArgs {
    #[arg(value_enum, ignore_case = true)]
    shell: Shell,
}

#[derive(ClapSubcommand)]
enum GramCommand {
    #[command(about = "Check a program")]
    Check(ProgramPathArg),

    #[command(about = "Run a program")]
    Run(ProgramPathArg),

    #[command(
        name = "shell-completion",
        about = "Print a shell completion script. Supports Bash, Fish, Zsh, PowerShell, and Elvish."
    )]
    ShellCompletion(ShellCompletionArgs),
}

// Run a program.
fn run(source_path: &Path, check_only: bool) -> Result<(), Vec<Error>> {
    // Read the file.
    let source_contents = read_to_string(source_path).map_err(|error| {
        vec![Error::new(
            &format!("Error when reading file {}.", source_path.code_path()),
            None,
            None,
            Some(Rc::new(error)),
        )]
    })?;

    // Tokenize the source.
    let tokens = tokenize(Some(source_path), &source_contents)?;

    // Parse the tokens.
    let term = parse(Some(source_path), &source_contents, &tokens[..], &[])?;

    // Type check the term.
    let mut typing_context = vec![];
    let mut definitions_context = vec![];
    let (elaborated_term, elaborated_type) = type_check(
        Some(source_path),
        &source_contents,
        &term,
        &mut typing_context,
        &mut definitions_context,
    )?;

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
        let value = evaluate(&elaborated_term).map_err(|error| vec![error])?;
        println!("{}", value.to_string().code_str());
    }

    // If we made it this far, nothing went wrong.
    Ok(())
}

// Print a shell completion script to STDOUT.
fn shell_completion(shell: Shell) {
    // Write the script to STDOUT.
    let mut command = Cli::command();
    generate(shell, &mut command, BIN_NAME, &mut stdout());
}

// Program entrypoint
fn entry() -> Result<(), Vec<Error>> {
    // Parse command-line arguments.
    let cli = Cli::parse();

    // Check if the user provided a path as the first argument.
    if let Some(source_path) = cli.path.as_deref() {
        // We got a path. Run the program at that path.
        run(source_path, false)?;
    } else {
        // Decide what to do based on the subcommand.
        match cli.command {
            Some(GramCommand::Check(args)) => {
                // Check the program.
                run(&args.path, true)?;
            }

            Some(GramCommand::Run(args)) => {
                // Run the program.
                run(&args.path, false)?;
            }

            Some(GramCommand::ShellCompletion(args)) => {
                shell_completion(args.shell);
            }

            // If no path or subcommand was provided, the [ref:arg_required_else_help] setting
            // should have already printed the help message.
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
            if let Err(errors) = entry() {
                eprintln!("{}", format_errors(&errors));
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

#[cfg(test)]
mod tests {
    use super::Cli;
    use clap::CommandFactory;

    #[test]
    fn verify_cli() {
        Cli::command().debug_assert();
    }
}
