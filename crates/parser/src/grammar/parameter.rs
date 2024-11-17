use super::*;

// :test param_list
// func f(a) end
// func f(a, b) end
// func f(a, b,) end
pub(super) fn param_list(p: &mut Parser) {
    let m = p.start();

    p.bump(T!['(']);
    p.eat_trivia();

    while p.current().map(|t| t != T![')']).unwrap_or(false) {
        if p.at(T![,]) {
            p.error("Missing <param>");
            p.bump(T![,]);
            p.eat_trivia();
            continue;
        }
        if p.at(IDENT) {
            param(p);
            p.eat_trivia();
            if p.eat(T![,]) {
                p.eat_trivia();
                continue;
            } else if p.at(IDENT) {
                p.error("Missing `,`");
                continue;
            }
        }
        break;
    }

    if !p.eat(T![')']) {
        p.error("Expected `)`");
    }

    m.complete(p, PARAM_LIST);
}

fn param(p: &mut Parser) {
    assert!(p.at(IDENT));

    let m = p.start();
    name(p);
    m.complete(p, PARAM);
}
