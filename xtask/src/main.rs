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

#[allow(dead_code)]
enum Level {
    Info,
    Warn,
    Success,
}

fn message(level: Level, msg: &str) {
    let prefix = match level {
        Level::Info => "info".blue(),
        Level::Warn => "warn".yellow(),
        Level::Success => "success".green(),
    };
    let mut lines = msg.lines();
    println!("[{}] {}", prefix, lines.next().unwrap_or(""));
    for line in lines {
        let spaces = " ".repeat(prefix.len() + 3);
        println!("{}{}", spaces, line);
    }
}
