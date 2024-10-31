mod syntax;

use crate::{flags, message, project_root, Level};
use anyhow::Result;
use std::{fs, path::Path};
use xshell::Shell;

impl flags::Codegen {
    pub(crate) fn run(self, sh: &Shell) -> Result<()> {
        let kind = self.kind.unwrap_or_default();
        match kind {
            flags::CodegenKind::All => syntax::generate(sh),
            flags::CodegenKind::Syntax => syntax::generate(sh),
        }
    }
}

fn rustfmt(sh: &Shell, text: String) -> Result<String> {
    let cmd = sh.cmd("rustfmt");
    Ok(cmd
        .arg("--config")
        .arg("fn_single_line=true")
        .stdin(text)
        .read()?)
}

fn ensure_file_contents(path: &Path, contents: &str) -> Result<()> {
    let display_path = path.strip_prefix(project_root()).unwrap_or(path);
    if let Ok(old_contents) = fs::read_to_string(path) {
        if old_contents == contents {
            message(
                Level::Info,
                &format!("{} is up to date", display_path.display()),
            );
            return Ok(());
        }
    }

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    message(Level::Warn, &format!("updating {}", display_path.display()));
    fs::write(path, contents)?;
    Ok(())
}
