use super::*;

pub(super) const ATOM_EXPR_FIRST: TokenSet = LITERA_FIRST.union(TokenSet::new(&[
    IDENT,    // func_expr
    T!['('],  // paren_expr
    T!['['],  // array_expr
    T!['{'],  // table_expr
    T![do],   // do_expr
    T![func], // func_expr
    T![if],   // if_expr
]));

pub(super) fn atom_expr(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_ts(ATOM_EXPR_FIRST));

    // SAFETY: `p.current()` is not `None` because `p.at_ts(ATOM_EXPR_FIRST)` is `true`.
    match unsafe { p.current().unwrap_unchecked() } {
        IDENT => local(p),
        T!['('] => paren_expr(p),
        T!['['] => array_expr(p),
        T!['{'] => table_expr(p),
        T![do] => do_expr(p),
        T![func] => func_expr(p),
        T![if] => if_expr(p),
        c if LITERA_FIRST.contains(c) => literal(p),
        _ => unreachable!(),
    }
}

const LITERA_FIRST: TokenSet = TokenSet::new(&[INT, FLOAT, STRING, T![true], T![false], T![nil]]);

// :test expr_literals
// var _ = 1
// var _ = 2.3
// var _ = "foo"
// var _ = 'bar'
// var _ = true
// var _ = false
// var _ = nil
fn literal(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_ts(LITERA_FIRST));
    let m = p.start();
    p.bump_any();
    m.complete(p, LITERAL)
}

// :test local
// var _ = foo_bar
fn local(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.bump(IDENT);
    m.complete(p, LOCAL)
}

pub fn paren_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.bump(T!['(']);
    p.eat_trivia();

    if p.at_ts(EXPR_FIRST) {
        expr(p);
        p.eat_trivia();
    } else {
        p.error("Expected <expr>");
    }

    if !p.eat(T![')']) {
        p.error("Expected ')' to close the parenthesized expression");
    }

    m.complete(p, PAREN_EXPR)
}

pub fn array_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.bump(T!['[']);
    p.eat_trivia();

    while p.current().map(|t| t != T![']']).unwrap_or(false) {
        if p.at(T![,]) {
            p.error("Missing <expr>");
            p.bump(T![,]);
            p.eat_trivia();
            continue;
        }
        if p.at_ts(EXPR_FIRST) {
            expr(p);
            p.eat_trivia();
            if p.eat(T![,]) {
                p.eat_trivia();
                continue;
            } else if p.at_ts(EXPR_FIRST) {
                p.error("Missing `,`");
                continue;
            }
        }
        break;
    }

    if !p.eat(T![']']) {
        p.error("Expected `]` to close the array expression");
    }

    m.complete(p, ARRAY_EXPR)
}

fn table_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.bump(T!['{']);
    p.eat_trivia();

    while p.current().map(|t| t != T!['}']).unwrap_or(false) {
        if p.at(T![,]) {
            p.error("Missing <table_field>");
            p.bump(T![,]);
            p.eat_trivia();
            continue;
        }
        if p.at_ts(TABLE_FIELD_FIRST) {
            table_field(p);
            p.eat_trivia();
            if p.eat(T![,]) {
                p.eat_trivia();
                continue;
            } else if p.at_ts(TABLE_FIELD_FIRST) {
                p.error("Missing `,`");
                continue;
            }
        }
        break;
    }

    m.complete(p, TABLE_EXPR)
}

const TABLE_FIELD_FIRST: TokenSet = TokenSet::new(&[IDENT, T!['[']]);

fn table_field(p: &mut Parser) {
    let m = p.start();

    table_field_name(p);
    p.eat_trivia();

    match p.current() {
        Some(T![=]) => {
            p.eat_trivia();
            expr(p);
        }
        // Some(T![:]) => {
        //     TODO: fallback with error
        // }
        _ => {
            p.error("Expected `=` to separate table field name and value");
        }
    }

    m.complete(p, TABLE_FIELD);
}

fn table_field_name(p: &mut Parser) {
    assert!(p.at_ts(TABLE_FIELD_FIRST));
    // SAFETY: `p.current()` is not `None` because above assertion is `true`.
    match unsafe { p.current().unwrap_unchecked() } {
        IDENT => p.bump(IDENT),
        T!['['] => table_field_name_expr(p),
        _ => unreachable!(),
    }
}

fn table_field_name_expr(p: &mut Parser) {
    let m = p.start();

    p.bump(T!['[']);
    p.eat_trivia();

    if p.at_ts(EXPR_FIRST) {
        expr(p);
        p.eat_trivia();
    } else {
        p.error("Expected <expr> as table field name");
    }

    if !p.eat(T![']']) {
        p.error("Expected `]` to close the table field name");
    }

    m.complete(p, TABLE_FIELD_NAME_EXPR);
}

pub(crate) fn do_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.bump(T![do]);
    p.eat_trivia();

    while let Some(current) = p.current() {
        match current {
            T![end] => {
                p.bump(T![end]);
                break;
            }
            _ => {
                statement::stmt(p);
                p.eat_trivia();
            }
        }
    }

    m.complete(p, DO_EXPR)
}

fn func_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.bump(T![func]);
    p.eat_trivia();

    if p.at(IDENT) {
        p.error_with(|p| {
            p.bump(IDENT);
            "Function expressions cannot have name"
        });
        p.eat_trivia();
    }

    if p.at(T!['(']) {
        parameter::param_list(p);
    } else {
        p.error("Expected `(`: Functions must have parameters");
    }

    while let Some(current) = p.current() {
        match current {
            T![end] => {
                p.bump(T![end]);
                break;
            }
            _ => {
                statement::stmt(p);
                p.eat_trivia();
            }
        }
    }

    m.complete(p, FUNC_EXPR)
}

pub(crate) fn if_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();

    p.bump(T![if]);
    p.eat_trivia();

    if p.at_ts(EXPR_FIRST) {
        expr(p);
        p.eat_trivia();
    } else {
        p.error("Expected <expr> after `if`");
    }

    if !p.eat(T![then]) {
        p.error("Expected `then` after `if <expression>");
    }
    p.eat_trivia();

    while let Some(current) = p.current() {
        match current {
            T![elif] | T![else] | T![end] => break,
            t if statement::STMT_FIRST.contains(t) => {
                statement::stmt(p);
            }
            _ => {
                p.error_with(|p| {
                    let m = p.start();
                    p.bump_any();
                    m.complete(p, ERROR);
                    "Expected <stmt>, <expr>, `elif`, `else`, or `end`"
                });
            }
        }
        p.eat_trivia();
    }

    while p.eat(T![elif]) {
        elif_branche(p);
        p.eat_trivia();
    }
    p.eat_trivia();

    if p.eat(T![else]) {
        else_branch(p);
        p.eat_trivia();
    }

    if !p.eat(T![end]) {
        p.error("Expected `end` to close the `if` expression");
    }

    m.complete(p, IF_EXPR)
}

fn elif_branche(p: &mut Parser) {
    let m = p.start();

    p.bump(T![elif]);
    p.eat_trivia();

    if p.at_ts(EXPR_FIRST) {
        expr(p);
        p.eat_trivia();
    } else {
        p.error("Expected <expr> after `elif`");
    }

    if !p.eat(T![then]) {
        p.error("Expected `then` after `elif <expression>");
    }
    p.eat_trivia();

    while let Some(current) = p.current() {
        match current {
            T![end] | T![elif] | T![else] => break,
            t if statement::STMT_FIRST.contains(t) => {
                statement::stmt(p);
            }
            _ => {
                p.error_with(|p| {
                    let m = p.start();
                    p.bump_any();
                    m.complete(p, ERROR);
                    "Expected <stmt>, <expr>, `elif`, or `else`"
                });
            }
        }
        p.eat_trivia();
    }

    m.complete(p, ELIF_BRANCH);
}

fn else_branch(p: &mut Parser) {
    let m = p.start();

    p.bump(T![else]);
    p.eat_trivia();

    while let Some(current) = p.current() {
        match current {
            T![end] => break,
            t if statement::STMT_FIRST.contains(t) => {
                statement::stmt(p);
            }
            _ => {
                p.error_with(|p| {
                    let m = p.start();
                    p.bump_any();
                    m.complete(p, ERROR);
                    "Expected <stmt>, <expr> or `end`"
                });
            }
        }
        p.eat_trivia();
    }

    m.complete(p, ELSE_BRANCH);
}
