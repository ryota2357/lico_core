use crate::{Level, flags, message, project_root};
use anyhow::Result;
use xshell::{Shell, cmd};

impl flags::Test {
    pub(crate) fn run(self, sh: &Shell, extra: Vec<String>) -> Result<()> {
        if !has_nextest(sh)? {
            message(
                Level::Error,
                "cargo-nextest not found; install it with `cargo install nextest --locked`",
            );
            return Ok(());
        }

        let config_file = project_root().join(".cargo").join("nextest.toml");
        cmd!(sh, "cargo nextest run {extra...} --config-file {config_file}").run()?;
        Ok(())
    }
}

fn has_nextest(sh: &Shell) -> Result<bool> {
    let output = cmd!(sh, "cargo nextest --version").ignore_status().output()?;
    if !output.status.success() {
        return Ok(false);
    }
    let stdout = String::from_utf8(output.stdout)?;
    if !stdout.trim().starts_with("cargo-nextest") {
        return Ok(false);
    }
    Ok(true)
}
