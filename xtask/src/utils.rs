use colored::Colorize;
use std::{env, path::PathBuf};

pub(crate) fn project_root() -> PathBuf {
    let dir =
        env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| env!("CARGO_MANIFEST_DIR").to_owned());
    PathBuf::from(dir).parent().unwrap().to_owned()
}

pub(crate) enum Level {
    Info,
    Warn,
    Error,
}

pub(crate) fn message<S: AsRef<str>>(level: Level, msg: S) {
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
