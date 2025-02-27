#![feature(closure_lifetime_binder)]

mod flags;

mod codegen;
mod xtest;

use anyhow::Result;
use colored::Colorize;
use std::{env, path::PathBuf};
use xshell::Shell;

fn main() -> Result<()> {
    let (flags, extra_args) = {
        let mut cli_args = std::env::args_os().skip(1).collect::<Vec<_>>();
        let extra = match cli_args.first() {
            Some(arg) if arg == "test" => {
                cli_args.drain(1..).map(|s| s.to_string_lossy().to_string()).collect()
            }
            _ => Vec::new(),
        };
        let flags = flags::Xtask::from_vec(cli_args)?;
        (flags, extra)
    };
    let sh = Shell::new()?;
    match flags.subcommand {
        flags::XtaskCmd::Codegen(cmd) => cmd.run(&sh),
        flags::XtaskCmd::Test(cmd) => cmd.run(&sh, extra_args),
    }
}

fn project_root() -> PathBuf {
    let dir =
        env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned());
    PathBuf::from(dir).parent().unwrap().to_owned()
}

enum Level {
    Info,
    Warn,
    Error,
}

fn message<S: AsRef<str>>(level: Level, msg: S) {
    let msg = msg.as_ref();
    let prefix = match level {
        Level::Info => "info".blue(),
        Level::Warn => "warn".yellow(),
        Level::Error => "error".red(),
    };
    let mut lines = msg.lines();
    println!("[{}] {}", prefix, lines.next().unwrap_or(""));
    for line in lines {
        let spaces = " ".repeat(prefix.len() + 3);
        println!("{}{}", spaces, line);
    }
}
