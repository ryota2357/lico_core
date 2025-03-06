use super::{display_path, ensure_file_contents, rustfmt};
use crate::{Level, message, project_root};
use anyhow::Result;
use ignore::Walk;
use quote::{format_ident, quote};
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};
use xshell::Shell;

pub(crate) fn generate(sh: &Shell, check: bool) -> Result<()> {
    let src_path = project_root().join("crates/parser/src");
    let data_path = project_root().join("crates/parser/test_data/inline");
    let tests_path = project_root().join("crates/parser/tests");

    let rust_files = list_files_by_extension(&src_path, "rs")?;
    let tests = {
        let mut tests = Vec::new();
        for file in rust_files {
            let text = fs::read_to_string(&file)?;
            tests.extend(extract_test(&text));
        }
        tests
    };

    // Group tests by group name
    let grouped_tests: HashMap<_, Vec<_>> =
        tests.into_iter().fold(HashMap::new(), |mut map, test| {
            map.entry(test.group.clone()).or_default().push(test);
            map
        });

    // Remove old test data (source) files and directories
    if data_path.exists() {
        for entry in fs::read_dir(&data_path)? {
            let path = entry?.path();
            if !path.is_dir() {
                continue;
            }
            let Some(dir_name) = path.file_stem().and_then(|n| n.to_str()) else {
                anyhow::bail!("Invalid directory name");
            };
            if !grouped_tests.contains_key(dir_name) {
                message(Level::Warn, format!("removing {}", display_path(&path)));
                fs::remove_dir_all(path)?;
            } else {
                for entry in fs::read_dir(&path)? {
                    let path = entry?.path();
                    if !path.is_file() {
                        continue;
                    }
                    let Some(name) = path.file_stem().and_then(|n| n.to_str()) else {
                        anyhow::bail!("Invalid file name");
                    };
                    if path.extension().is_some_and(|ext| ext == "lico")
                        && !grouped_tests
                            .get(dir_name)
                            .is_some_and(|tests| tests.iter().any(|t| t.name == name))
                    {
                        message(Level::Warn, format!("removing {}", display_path(&path)));
                        fs::remove_file(path)?;
                    }
                }
            }
        }
    }

    // Write test data (source) files
    for (group, tests) in &grouped_tests {
        let group_path = data_path.join(group);
        fs::create_dir_all(&group_path)?;
        for test in tests {
            ensure_file_contents(
                &group_path.join(format!("{}.lico", test.name)),
                &test.code,
                check,
            )?;
        }
    }

    // Generate test files for each group
    for (group, group_tests) in &grouped_tests {
        let test_file_name = format!("inline_{}.rs", group);
        ensure_file_contents(
            &tests_path.join(&test_file_name),
            &{
                let header = [
                    "//! This file is generated by `cargo codegen parser`, do not edit by hand.",
                    "#![cfg_attr(rustfmt, rustfmt::skip)]",
                    "",
                    "mod utils;",
                    &format!("const GROUP: &str = \"inline/{}\";", group),
                    "",
                ]
                .join("\n");
                let code = {
                    let name_idents = group_tests.iter().map(|test| format_ident!("{}", test.name));
                    let name_strs = group_tests.iter().map(|test| test.name.as_str());
                    let paths = group_tests
                        .iter()
                        .map(|test| format!("../test_data/inline/{}/{}.lico", group, test.name));
                    quote! {
                        #(
                            #[test]
                            fn #name_idents() {
                                let source = include_str!(#paths);
                                let snapshot = utils::make_snapshot(#name_strs, source);
                                utils::assert_snapshot!(GROUP, snapshot);
                            }
                        )*
                    }
                };
                format!("{}\n{}", header, rustfmt(sh, code.to_string())?)
            },
            check,
        )?;
    }

    Ok(())
}

fn list_files_by_extension(path: &Path, ext: &str) -> Result<Vec<PathBuf>> {
    let mut res = Vec::new();
    for entry in Walk::new(path) {
        let entry = entry?;
        if entry.path().extension() == Some(ext.as_ref()) {
            res.push(entry.path().to_path_buf());
        }
    }
    res.sort();
    Ok(res)
}

struct Test {
    group: String,
    name: String,
    code: String,
}

fn extract_test(text: &str) -> Vec<Test> {
    let mut res = Vec::new();
    let mut group = String::new();
    let mut name = String::new();
    let mut code = String::new();
    for line in text.lines().map(str::trim_start) {
        let Some(line) = line.strip_prefix("//") else {
            if !name.is_empty() {
                res.push(Test { group: group.clone(), name: name.clone(), code: code.clone() });
                name.clear();
                code.clear();
            }
            continue;
        };
        let line = line.strip_prefix(' ').unwrap_or(line);
        match line.strip_prefix(":test") {
            Some(rest) => {
                if !name.is_empty() {
                    res.push(Test { group: group.clone(), name: name.clone(), code: code.clone() });
                    code.clear();
                }
                let parts: Vec<&str> = rest.split_whitespace().collect();
                if parts.len() >= 2 {
                    group = parts[0].to_string();
                    name = parts[1].to_string();
                } else {
                    message(Level::Warn, format!("invalid test directive: {}", rest));
                }
            }
            _ if !name.is_empty() => {
                if !code.is_empty() {
                    code.push('\n');
                }
                code.push_str(line);
            }
            _ => {}
        }
    }
    if !name.is_empty() {
        res.push(Test { group, name, code });
    }
    res
}
