#![feature(closure_lifetime_binder)]

mod codegen;
mod flags;

use anyhow::Result;
use colored::Colorize;
use std::{env, path::PathBuf};
use xshell::Shell;

fn main() -> Result<()> {
    let flags = flags::Xtask::from_env_or_exit();
    let sh = Shell::new()?;
    sh.change_dir(project_root());
    match flags.subcommand {
        flags::XtaskCmd::Codegen(cmd) => cmd.run(&sh),
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
}

fn message<S: AsRef<str>>(level: Level, msg: S) {
    let msg = msg.as_ref();
    let prefix = match level {
        Level::Info => "info".blue(),
        Level::Warn => "warn".yellow(),
    };
    let mut lines = msg.lines();
    println!("[{}] {}", prefix, lines.next().unwrap_or(""));
    for line in lines {
        let spaces = " ".repeat(prefix.len() + 3);
        println!("{}{}", spaces, line);
    }
}
