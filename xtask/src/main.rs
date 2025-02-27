#![feature(closure_lifetime_binder)]

mod flags;

mod utils;
use utils::*;

mod commands;

use anyhow::Result;
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
