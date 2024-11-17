use super::*;

pub(super) const STMT_FIRST: TokenSet = expression::EXPR_FIRST.unions(&[
    T![@],        // attr_stmt
    T![break],    // break_stmt
    T![continue], // continue_stmt
    T![for],      // for_stmt
    T![func],     // func_stmt
    T![return],   // return_stmt
    T![var],      // var_stmt
    T![while],    // while_stmt
]);

pub(super) fn stmt(p: &mut Parser) {
    assert!(p.at_ts(STMT_FIRST));

    // SAFETY: `p.at_ts(..)` is true, so `p.current()` is not None.
    match unsafe { p.current().unwrap_unchecked() } {
        T![@] => attr_stmt(p),
        T![break] => break_stmt(p),
        T![continue] => continue_stmt(p),
        T![for] => for_stmt(p),
        T![func] => func_stmt(p),
        T![return] => return_stmt(p),
        T![var] => var_stmt(p),
        T![while] => while_stmt(p),
        t if expression::EXPR_FIRST.contains(t) => {
            expression::expr(p);
        }
        _ => unreachable!(),
    }
}

// :test attr_stmt
// @![global_attr]
// @[local_attr]
fn attr_stmt(p: &mut Parser) {
    let m = p.start();

    p.bump(T![@]);
    p.eat(T![!]);

    if p.eat(T!['[']) {
        p.eat_trivia();

        if !p.eat(IDENT) {
            p.error("expected <name>");
        }
        p.eat_trivia();

        if !p.eat(T![']']) {
            p.error("expected `]`");
        }
    } else {
        p.error("expected `[`");
    }

    m.complete(p, ATTR_STMT);
}

// :test break_stmt
// break
fn break_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(T![break]);
    m.complete(p, BREAK_STMT);
}

// :test continue_stmt
// continue
fn continue_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(T![continue]);
    m.complete(p, CONTINUE_STMT);
}

fn for_stmt(p: &mut Parser) {
    let m = p.start();

    p.bump(T![for]);
    p.eat_trivia();

    if p.at_ts(pattern::PAT_FIRST) {
        pattern::pat(p);
        p.eat_trivia();
    } else {
        p.error("Expected <pattern>");
    }

    if !p.eat(T![in]) {
        p.error("Expected `in`");
    }
    p.eat_trivia();

    expression::do_expr(p);

    m.complete(p, FOR_STMT);
}

// :test func
// func foo() end
// func() end
fn func_stmt(p: &mut Parser) {
    let m = p.start();

    p.bump(T![func]);
    p.eat_trivia();

    let is_expr = if p.at(IDENT) {
        name(p);
        p.eat_trivia();
        false
    } else {
        true
    };

    if p.at(T!['(']) {
        parameter::param_list(p);
        p.eat_trivia();
    } else {
        p.error("Expected `(`: Functions must have parameters");
    }

    while let Some(current) = p.current() {
        match current {
            T![end] => {
                p.bump(T![end]);
                break;
            }
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

    if is_expr {
        m.complete(p, FUNC_EXPR);
    } else {
        m.complete(p, FUNC_STMT);
    }
}

// :test return_stmt
// return
// return 1
fn return_stmt(p: &mut Parser) {
    let m = p.start();

    p.bump(T![return]);

    if p.next_non_trivia().is_some_and(|t| expression::EXPR_FIRST.contains(t)) {
        p.eat_trivia();
        expression::expr(p);
    }

    m.complete(p, RETURN_STMT);
}

// :test var_stmt
// var x = 1
fn var_stmt(p: &mut Parser) {
    let m = p.start();
    p.bump(T![var]);
    p.eat_trivia();

    if p.at_ts(pattern::PAT_FIRST) {
        pattern::pat(p);
        p.eat_trivia();
    } else {
        p.error("Expected <pattern>");
    }

    if p.eat(T![=]) {
        p.eat_trivia();
        expression::expr(p);
    } else {
        p.error("Missing `= <expr>`: Variables must be initialized");
    }

    m.complete(p, VAR_STMT);
}

// :test while_stmt
// while true do end
fn while_stmt(p: &mut Parser) {
    let m = p.start();

    p.bump(T![while]);
    p.eat_trivia();

    if p.at_ts(expression::EXPR_FIRST) {
        expression::expr(p);
        p.eat_trivia();
    } else {
        p.error("Expected <expr> for condition");
    }

    if p.at(T![do]) {
        expression::do_expr(p);
    } else {
        p.error("Expected `do` ~ `end` block");
    }

    m.complete(p, WHILE_STMT);
}
