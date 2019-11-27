use clap::Shell;
use std::{env::var_os, fs::create_dir_all, io::Result, path::Path};

include!("src/cli.rs");

fn main() -> Result<()> {
    let mut app = cli();
    let bin_name = "gram";
    let out_dir = Path::new(&var_os("OUT_DIR").unwrap()).join("completions");

    create_dir_all(&out_dir)?;

    app.gen_completions(bin_name, Shell::Bash, &out_dir);
    app.gen_completions(bin_name, Shell::Fish, &out_dir);
    app.gen_completions(bin_name, Shell::Zsh, &out_dir);
    app.gen_completions(bin_name, Shell::PowerShell, &out_dir);
    app.gen_completions(bin_name, Shell::Elvish, &out_dir);

    Ok(())
}
