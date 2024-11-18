use std::fmt::Write;
use syntax::{
    ast::{AstNode, SourceFile},
    SyntaxNode,
};

pub fn make_snapshot(name: &str, source: &str) -> String {
    let (green, err) = parser::parse(lexer::tokenize(source));
    let syntax_node = SyntaxNode::new_root(green);
    let ast = SourceFile::cast(syntax_node.clone()).unwrap();

    let mut f = String::new();
    fn h(f: &mut String, level: u8, text: &str) {
        for _ in 0..level {
            write!(f, "#").unwrap();
        }
        writeln!(f, " {}\n", text).unwrap();
    }
    fn code(f: &mut String, text: &str) {
        writeln!(f, "```").unwrap();
        writeln!(f, "{}", text).unwrap();
        writeln!(f, "```\n").unwrap();
    }

    h(&mut f, 1, name);
    h(&mut f, 2, "Input");
    code(&mut f, source);
    h(&mut f, 2, "CST");
    code(&mut f, format!("{:#?}", syntax_node).trim());
    if !err.is_empty() {
        h(&mut f, 2, "Syntax Error");
        code(&mut f, &format!("{:#?}", err));
    }
    h(&mut f, 2, "AST");
    code(&mut f, format!("{:#?}", ast).trim());

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
