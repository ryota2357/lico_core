use crate::{parser::*, TokenSet};
use syntax::{SyntaxKind, SyntaxKind::*, T};

mod expression;
mod parameter;
mod pattern;
mod statement;

pub(crate) fn source_file(p: &mut Parser) {
    let m = p.start();

    p.eat(SHEBANG);
    p.eat_trivia();

    while let Some(current) = p.current() {
        if statement::STMT_FIRST.contains(current) {
            statement::stmt(p);
        } else {
            p.error_with(|p| {
                let m = p.start();
                p.bump_any();
                m.complete(p, ERROR);
                "Expected <stmt> or <expr>"
            });
        }
        p.eat_trivia();
    }

    m.complete(p, SOURCE_FILE);
}

fn name(p: &mut Parser) {
    let m = p.start();
    p.bump(T![ident]);
    m.complete(p, NAME);
}
