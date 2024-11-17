use std::fmt::Write;
use syntax::SyntaxNode;

pub fn make_snapshot(name: &str, source: &str) -> String {
    let (green, err) = parser::parse(lexer::tokenize(source));
    let mut f = String::new();
    writeln!(f, "# {}\n", name).unwrap();
    writeln!(f, "## Input").unwrap();
    writeln!(f, "\n```").unwrap();
    writeln!(f, "{}\n```\n", source).unwrap();
    writeln!(f, "## CST").unwrap();
    writeln!(f, "\n```").unwrap();
    writeln!(f, "{:#?}```\n", SyntaxNode::new_root(green)).unwrap();
    if !err.is_empty() {
        writeln!(f, "## Syntax Error").unwrap();
        writeln!(f, "\n```").unwrap();
        writeln!(f, "{:#?}\n```\n", err).unwrap();
    }
    format!("{}vim:ft=markdown", f)
}

#[macro_export]
#[doc(hidden)]
macro_rules! __insta_assert_snapshot_wrapper {
    ($group:expr, $snapshot:expr) => {
        insta::with_settings!({
            prepend_module_to_snapshot => false,
            snapshot_path => format!("snapshots/{}", $group),
            omit_expression => true,
        }, {
            insta::assert_snapshot!($snapshot);
        });
    };
}
pub use __insta_assert_snapshot_wrapper as assert_snapshot;
