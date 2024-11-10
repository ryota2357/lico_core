use super::*;

mod atom;

pub(super) use atom::do_expr;

pub(super) const EXPR_FIRST: TokenSet = LHS_FIRST;

pub(super) fn expr(p: &mut Parser) {
    assert!(p.at_ts(EXPR_FIRST));
    expr_bp(p, 0);
}

fn expr_bp(p: &mut Parser, min_bp: u8) -> CompletedMarker {
    assert!(p.at_ts(EXPR_FIRST));

    let mut lhs = lhs(p);
    loop {
        let Some(next) = p.next_non_trivia() else {
            break;
        };
        let (r_bp, m) = match infix_op_binding_power(next) {
            Some((l_bp, r_bp)) => {
                p.eat_trivia();
                if l_bp < min_bp {
                    break;
                }
                let m = lhs.precede(p);
                p.bump_any();
                (r_bp, m)
            }
            None => match next {
                T![~] if p.nth_at(1, T![=]) => {
                    p.eat_trivia();
                    let (l_bp, r_bp) = infix_op_binding_power(T![!=]).unwrap();
                    if l_bp < min_bp {
                        break;
                    }
                    let m = lhs.precede(p);
                    p.error_with(|p| {
                        // TODO: fallback to T![!=]
                        let m = p.start();
                        p.bump(T![~]);
                        p.bump(T![=]);
                        m.complete(p, ERROR);
                        "Should use `!=` instead of `~=`"
                    });
                    (r_bp, m)
                }
                _ => break,
            },
        };
        if p.next_non_trivia().is_some_and(|next| EXPR_FIRST.contains(next)) {
            p.eat_trivia();
            expr_bp(p, r_bp);
        } else {
            p.error("Expected <expr>");
        }
        lhs = m.complete(p, BINARY_EXPR);
    }
    lhs
}

const LHS_FIRST: TokenSet = atom::ATOM_EXPR_FIRST.union(TokenSet::new(&[
    T![+],      // prefix-op
    T![-],      // prefix-op
    T![~],      // prefix-op
    T![not],    // prefix-op
    T![typeof], // prefix-op
    T![!],      // Invalid prefix-op, for error recovery
]));

fn lhs(p: &mut Parser) -> CompletedMarker {
    assert!(p.at_ts(EXPR_FIRST));

    // The order of precedence (binding power) of operators in Lico is prefix-op is the strongest,
    // followed by postfix-op, and finally infix-op. Therefore, we don't care about precedence to
    // parse prefix-op and postfix-op, we can just parse them in the order we encounter them.

    // SAFETY: `at_ts(EXPR_FIRST)` is true, so `p.current()` is not None.
    match unsafe { p.current().unwrap_unchecked() } {
        T![+] | T![-] | T![~] | T![not] | T![typeof] => {
            let m = p.start();
            p.bump_any();
            if p.next_non_trivia().is_some_and(|t| EXPR_FIRST.contains(t)) {
                p.eat_trivia();
                expr_bp(p, 255); // prefix-op has the highest precedence
            } else {
                p.error("Expected <expr>");
            }
            m.complete(p, PREFIX_EXPR)
        }
        T![!] => {
            let m = p.start();
            p.error_with(|p| {
                p.bump(T![!]); // TODO: fallback to T![not]
                "Should use `not` for logical negation or `~` for bitwise negation"
            });
            if p.next_non_trivia().is_some_and(|t| EXPR_FIRST.contains(t)) {
                p.eat_trivia();
                expr_bp(p, 255); // prefix-op has the highest precedence
            } else {
                p.error("Expected <expr>");
            }
            m.complete(p, PREFIX_EXPR)
        }
        _ => {
            let mut lhs = atom::atom_expr(p);
            while let Some(next) = p.next_non_trivia() {
                const POSTFIX_FIRST: TokenSet = TokenSet::new(&[T![.], T!['['], T!['('], T![->]]);
                if !POSTFIX_FIRST.contains(next) {
                    break;
                }
                p.eat_trivia();
                lhs = match next {
                    T![.] => field_expr(p, lhs),
                    T!['['] => index_expr(p, lhs),
                    T!['('] => call_expr(p, lhs),
                    T![->] => method_call_expr(p, lhs),
                    _ => unreachable!(),
                };
            }
            lhs
        }
    }
}

/// |        Precedence        | Associativity |     Operators     |
/// | -----------------------  | ------------- | ----------------- |
/// | 13: Unary Postfix        |    postfix    | .x, [], (), ->x() |
/// | 12: Unary Prefix         |    prefix     | +, -, not         |
/// | 11: Multiplicative       |   left infix  | *, /, %           |
/// | 10: Additive             |   left infix  | +, -              |
/// |  9: String concatenation |  right infix  | ..                |
/// |  8: Shift                |   left infix  | <<, >>            |
/// |  7: Bitwise-AND          |   left infix  | &                 |
/// |  6: Bitwise-XOR          |   left infix  | ^                 |
/// |  5: Bitwise-OR           |   left infix  | |                 |
/// |  4: Relational           |   left infix  | <, <=, >, >=      |
/// |  3: Equality             |   left infix  | ==, !=            |
/// |  2: Logical-AND          |   left infix  | and               |
/// |  1: Logical-OR           |   left infix  | or                |
/// |  0: Assignment           |   right infix | =                 |
const fn infix_op_binding_power(kind: SyntaxKind) -> Option<(u8, u8)> {
    const fn left(precedence: u8) -> (u8, u8) {
        (2 * precedence + 1, 2 * precedence + 2)
    }
    const fn right(precedence: u8) -> (u8, u8) {
        (2 * precedence + 2, 2 * precedence + 1)
    }
    #[rustfmt::skip]
    let bp = match kind {
        T![*] | T![/] | T![%]           => left(11),
        T![+] | T![-]                   => left(10),
        T![..]                          => right(9),
        T![<<] | T![>>]                 => left(8),
        T![&]                           => left(7),
        T![^]                           => left(6),
        T![|]                           => left(5),
        T![<] | T![<=] | T![>] | T![>=] => left(4),
        T![==] | T![!=]                 => left(3),
        T![and]                         => left(2),
        T![or]                          => left(1),
        T![=]                           => right(0),
        _ => return None,
    };
    Some(bp)
}

fn field_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);

    p.bump(T![.]);
    p.eat_trivia();

    if p.at(IDENT) {
        name(p);
    } else {
        p.error("Expected <name> after `.`")
    }

    m.complete(p, FIELD_EXPR)
}

fn index_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);

    p.bump(T!['[']);
    p.eat_trivia();

    expr(p);
    p.eat_trivia();

    if !p.eat(T![']']) {
        p.error("Expected `]` to close index expression");
    }

    m.complete(p, INDEX_EXPR)
}

fn call_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);
    arg_list(p);
    m.complete(p, CALL_EXPR)
}

fn method_call_expr(p: &mut Parser, lhs: CompletedMarker) -> CompletedMarker {
    let m = lhs.precede(p);

    p.bump(T![->]);
    p.eat_trivia();

    if p.at(IDENT) {
        name(p);
        p.eat_trivia();
    } else {
        p.error("Expected <name> after `->`")
    }

    if p.at(T!['(']) {
        arg_list(p);
    } else {
        p.error("Expected arguments for method call");
    }

    m.complete(p, METHOD_CALL_EXPR)
}

fn arg_list(p: &mut Parser) {
    let m = p.start();

    p.bump(T!['(']);
    p.eat_trivia();

    while p.current().map(|t| t != T![')']).unwrap_or(false) {
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

    if !p.eat(T![')']) {
        p.error("Expected `)` to close argument list");
    }

    m.complete(p, ARG_LIST);
}
