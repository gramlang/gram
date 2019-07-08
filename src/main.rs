use clap::{
    App,
    AppSettings::{ColoredHelp, UnifiedHelpMessage},
};

// The program version
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    // Parse command-line arguments.
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
        .get_matches();
}
