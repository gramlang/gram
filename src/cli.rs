use clap::{
    App,
    AppSettings::{
        ColoredHelp, SubcommandRequiredElseHelp, UnifiedHelpMessage, VersionlessSubcommands,
    },
    Arg, SubCommand,
};

// The program version
const VERSION: &str = env!("CARGO_PKG_VERSION");

// Command-line option and subcommand names
pub const RUN_SUBCOMMAND: &str = "run";
pub const PATH_OPTION: &str = "path";

// Set up the command-line interface.
pub fn cli<'a, 'b>() -> App<'a, 'b> {
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
}
