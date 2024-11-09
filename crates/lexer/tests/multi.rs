mod common;
use common::*;

#[test]
fn spaced_one_ident() {
    assert_token!("a ", [Ident(1), Whitespace(1)]);
    assert_token!(" b", [Whitespace(1), Ident(1)]);
    assert_token!(" c ", [Whitespace(1), Ident(1), Whitespace(1)]);
    assert_token!("あ ", [Ident(3), Whitespace(1)]);
    assert_token!("\nい", [Whitespace(1), Ident(3)]);
    assert_token!(" う\n", [Whitespace(1), Ident(3), Whitespace(1)]);
}

#[test]
fn spaced_multi_ident() {
    assert_token!("foo ", [Ident(3), Whitespace(1)]);
    assert_token!("　bar", [Whitespace(3), Ident(3)]);
    assert_token!("　baz ", [Whitespace(3), Ident(3), Whitespace(1)]);
    assert_token!("あ ", [Ident(3), Whitespace(1)]);
    assert_token!("\nい", [Whitespace(1), Ident(3)]);
    assert_token!(" う\n", [Whitespace(1), Ident(3), Whitespace(1)]);
}

#[test]
fn spaced_literal() {
    assert_token!("true ", [True(4), Whitespace(1)]);
    assert_token!("false ", [False(5), Whitespace(1)]);
    assert_token!("nil ", [Nil(3), Whitespace(1)]);
}

#[test]
fn spaced_keyword() {
    assert_token!("and ", [And(3), Whitespace(1)]);
    assert_token!("break ", [Break(5), Whitespace(1)]);
    assert_token!("continue ", [Continue(8), Whitespace(1)]);
    assert_token!("do ", [Do(2), Whitespace(1)]);
    assert_token!("elif ", [Elif(4), Whitespace(1)]);
    assert_token!("else ", [Else(4), Whitespace(1)]);
    assert_token!("end ", [End(3), Whitespace(1)]);
    assert_token!("for ", [For(3), Whitespace(1)]);
    assert_token!("func ", [Func(4), Whitespace(1)]);
    assert_token!("if ", [If(2), Whitespace(1)]);
    assert_token!("in ", [In(2), Whitespace(1)]);
    assert_token!("not ", [Not(3), Whitespace(1)]);
    assert_token!("or ", [Or(2), Whitespace(1)]);
    assert_token!("return ", [Return(6), Whitespace(1)]);
    assert_token!("then ", [Then(4), Whitespace(1)]);
    assert_token!("typeof ", [TypeOf(6), Whitespace(1)]);
    assert_token!("var ", [Var(3), Whitespace(1)]);
    assert_token!("while ", [While(5), Whitespace(1)]);
}

#[test]
fn literal_like() {
    // "true"
    assert_token!("tru ", [Ident(3), Whitespace(1)]);
    assert_token!("tr@", [Ident(2), At(1)]);

    // "false"
    assert_token!("fals ", [Ident(4), Whitespace(1)]);
    assert_token!("fal@", [Ident(3), At(1)]);

    // "nil"
    assert_token!("ni ", [Ident(2), Whitespace(1)]);
    assert_token!("n@", [Ident(1), At(1)]);
}

#[test]
fn keyword_like() {
    // "and"
    assert_token!("an ", [Ident(2), Whitespace(1)]);
    assert_token!("a@", [Ident(1), At(1)]);
    assert_token!("anda", [Ident(4)]);

    // "break"
    assert_token!("brea ", [Ident(4), Whitespace(1)]);
    assert_token!("bre@", [Ident(3), At(1)]);
    assert_token!("breakb", [Ident(6)]);

    // "continue"
    assert_token!("continu ", [Ident(7), Whitespace(1)]);
    assert_token!("cont@", [Ident(4), At(1)]);
    assert_token!("continuec", [Ident(9)]);

    // "do"
    assert_token!("d ", [Ident(1), Whitespace(1)]);
    assert_token!("d@", [Ident(1), At(1)]);
    assert_token!("dod", [Ident(3)]);

    // "elif"
    assert_token!("eli ", [Ident(3), Whitespace(1)]);
    assert_token!("el@", [Ident(2), At(1)]);
    assert_token!("elife", [Ident(5)]);

    // "else"
    assert_token!("els ", [Ident(3), Whitespace(1)]);
    assert_token!("el@", [Ident(2), At(1)]);
    assert_token!("elsee", [Ident(5)]);

    // "end"
    assert_token!("en ", [Ident(2), Whitespace(1)]);
    assert_token!("e@", [Ident(1), At(1)]);
    assert_token!("ende", [Ident(4)]);

    // "for"
    assert_token!("fo ", [Ident(2), Whitespace(1)]);
    assert_token!("f@", [Ident(1), At(1)]);
    assert_token!("forf", [Ident(4)]);

    // "func"
    assert_token!("fun ", [Ident(3), Whitespace(1)]);
    assert_token!("fu@", [Ident(2), At(1)]);
    assert_token!("funcf", [Ident(5)]);

    // "if"
    assert_token!("i ", [Ident(1), Whitespace(1)]);
    assert_token!("i@", [Ident(1), At(1)]);
    assert_token!("ifi", [Ident(3)]);

    // "in"
    assert_token!("i ", [Ident(1), Whitespace(1)]);
    assert_token!("i@", [Ident(1), At(1)]);
    assert_token!("ini", [Ident(3)]);

    // "not"
    assert_token!("no ", [Ident(2), Whitespace(1)]);
    assert_token!("n@", [Ident(1), At(1)]);
    assert_token!("notn", [Ident(4)]);

    // "or"
    assert_token!("o ", [Ident(1), Whitespace(1)]);
    assert_token!("o@", [Ident(1), At(1)]);
    assert_token!("oro", [Ident(3)]);

    // "return"
    assert_token!("retur ", [Ident(5), Whitespace(1)]);
    assert_token!("ret@", [Ident(3), At(1)]);
    assert_token!("returnr", [Ident(7)]);

    // "then"
    assert_token!("the ", [Ident(3), Whitespace(1)]);
    assert_token!("th@", [Ident(2), At(1)]);
    assert_token!("thent", [Ident(5)]);

    // "typeof"
    assert_token!("typeo ", [Ident(5), Whitespace(1)]);
    assert_token!("type@", [Ident(4), At(1)]);
    assert_token!("typeoft", [Ident(7)]);

    // "var"
    assert_token!("va ", [Ident(2), Whitespace(1)]);
    assert_token!("v@", [Ident(1), At(1)]);
    assert_token!("varv", [Ident(4)]);

    // "while"
    assert_token!("whil ", [Ident(4), Whitespace(1)]);
    assert_token!("wh@", [Ident(2), At(1)]);
    assert_token!("whilew", [Ident(6)]);
}
