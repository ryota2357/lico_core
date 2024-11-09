mod common;
use common::*;

#[test]
fn line_comment() {
    assert_token!("#", [LineComment(1)]);
    assert_token!("# comment", [LineComment(9)]);
    assert_token!("##", [LineComment(2)]);
}

#[test]
fn whitespace() {
    assert_token!(" \t\n", [Whitespace(3)]);
}

#[test]
fn whitespace2() {
    // ref: https://ufcpp.net/blog/2022/12/rawstringwhitespace/
    let ws = [
        0x0009, 0x000B, 0x000C, 0x20, 0xA0, 0x1680, 0x2000, 0x2001, 0x2002, 0x2003, 0x2004, 0x2005,
        0x2006, 0x2007, 0x2008, 0x2009, 0x200A, 0x202F, 0x205F, 0x3000,
    ]
    .iter()
    .map(|x| char::from_u32(*x).unwrap().to_string());
    for c in ws {
        let len = c.len() as u32;
        let lexed = lexer::tokenize(&c).collect::<Vec<_>>();
        pretty_assertions::assert_eq!(lexed, vec![Token { len, kind: TokenKind::Whitespace }]);
    }
}

#[test]
fn int() {
    assert_token!("0", [Int { 1, base: NumBase::Decimal, empty_int: false }]);
    assert_token!("42", [Int { 2, base: NumBase::Decimal, empty_int: false }]);
    assert_token!("0123456789", [Int { 10, base: NumBase::Decimal, empty_int: false }]);

    assert_token!("0b", [Int { 2, base: NumBase::Binary, empty_int: true }]);
    assert_token!("0b0", [Int { 3, base: NumBase::Binary, empty_int: false }]);
    assert_token!("0b1010", [Int { 6, base: NumBase::Binary, empty_int: false }]);
    assert_token!("0b0101", [Int { 6, base: NumBase::Binary, empty_int: false }]);
    assert_token!("0b123456789", [Int { 11, base: NumBase::Binary, empty_int: false }]);

    assert_token!("0o", [Int { 2, base: NumBase::Octal, empty_int: true }]);
    assert_token!("0o0", [Int { 3, base: NumBase::Octal, empty_int: false }]);
    assert_token!("0o01234567", [Int { 10, base: NumBase::Octal, empty_int: false }]);
    assert_token!("0o76543210", [Int { 10, base: NumBase::Octal, empty_int: false }]);
    assert_token!("0o123456789", [Int { 11, base: NumBase::Octal, empty_int: false }]);

    assert_token!("0x", [Int { 2, base: NumBase::Hexadecimal, empty_int: true }]);
    assert_token!("0x0", [Int { 3, base: NumBase::Hexadecimal, empty_int: false }]);
    assert_token!("0x0123456789abcdef", [Int { 18, base: NumBase::Hexadecimal, empty_int: false }]);
    assert_token!("0xfedcba9876543210", [Int { 18, base: NumBase::Hexadecimal, empty_int: false }]);
    assert_token!("0x0123456789ABCDEF", [Int { 18, base: NumBase::Hexadecimal, empty_int: false }]);
    assert_token!("0xFEDCBA9876543210", [Int { 18, base: NumBase::Hexadecimal, empty_int: false }]);
}

#[test]
fn float() {
    assert_token!("0.", [Float { 2, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("0.0", [Float { 3, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("0e0", [Float { 3, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("0e+0", [Float { 4, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("0e-0", [Float { 4, base: NumBase::Decimal, empty_exponent: false }]);

    assert_token!("3.", [Float { 2, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("3.14", [Float { 4, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!(
        "0123456789.0123456789",
        [Float { 21, base: NumBase::Decimal, empty_exponent: false }]
    );

    assert_token!("2e3", [Float { 3, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("2.718e3", [Float { 7, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("01.2e+30", [Float { 8, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("01.2e+03", [Float { 8, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("01.2e-30", [Float { 8, base: NumBase::Decimal, empty_exponent: false }]);
    assert_token!("01.2e-03", [Float { 8, base: NumBase::Decimal, empty_exponent: false }]);
}

#[test]
fn string() {
    assert_token!(r#""""#, [String { 2, terminated: true, quote_kind: QuoteKind::Double }]);
    assert_token!(r#"''"#, [String { 2, terminated: true, quote_kind: QuoteKind::Single }]);

    assert_token!(r#""foo""#, [String { 5, terminated: true, quote_kind: QuoteKind::Double }]);
    assert_token!(
        r#"'piyo piyo'"#,
        [String { 11, terminated: true, quote_kind: QuoteKind::Single }]
    );

    assert_token!(
        r#""foo
bar""#,
        [String { 9, terminated: true, quote_kind: QuoteKind::Double }]
    );
}

#[test]
fn string_escaped() {
    assert_token!(
        r#""hoge\"fuga""#,
        [String { 12, terminated: true, quote_kind: QuoteKind::Double }]
    );
    assert_token!(
        r#"'bar\'baz'"#,
        [String { 10, terminated: true, quote_kind: QuoteKind::Single }]
    );

    assert_token!(
        r#""\
""#,
        [String { 4, terminated: true, quote_kind: QuoteKind::Double }]
    );
    assert_token!(
        r#"'\
'"#,
        [String { 4, terminated: true, quote_kind: QuoteKind::Single }]
    );
}

#[test]
fn bool() {
    assert_token!("true", [True(4)]);
    assert_token!("false", [False(5)]);
}

#[test]
fn nil() {
    assert_token!("nil", [Nil(3)]);
}

#[test]
fn keyword() {
    assert_token!("and", [And(3)]);
    assert_token!("break", [Break(5)]);
    assert_token!("continue", [Continue(8)]);
    assert_token!("do", [Do(2)]);
    assert_token!("elif", [Elif(4)]);
    assert_token!("else", [Else(4)]);
    assert_token!("end", [End(3)]);
    assert_token!("for", [For(3)]);
    assert_token!("func", [Func(4)]);
    assert_token!("if", [If(2)]);
    assert_token!("in", [In(2)]);
    assert_token!("not", [Not(3)]);
    assert_token!("or", [Or(2)]);
    assert_token!("return", [Return(6)]);
    assert_token!("then", [Then(4)]);
    assert_token!("typeof", [TypeOf(6)]);
    assert_token!("var", [Var(3)]);
    assert_token!("while", [While(5)]);
}

#[test]
fn symbol() {
    assert_token!("!", [Bang(1)]);
    assert_token!("!=", [BangEq(2)]);
    assert_token!("%", [Percent(1)]);
    assert_token!("&", [Amp(1)]);
    assert_token!("(", [OpenParen(1)]);
    assert_token!(")", [CloseParen(1)]);
    assert_token!("*", [Star(1)]);
    assert_token!("+", [Plus(1)]);
    assert_token!(",", [Comma(1)]);
    assert_token!("-", [Minus(1)]);
    assert_token!("->", [Arrow(2)]);
    assert_token!(".", [Dot(1)]);
    assert_token!("..", [Dot2(2)]);
    assert_token!("/", [Slash(1)]);
    assert_token!("<", [Lt(1)]);
    assert_token!("<<", [Lt2(2)]);
    assert_token!("<=", [Le(2)]);
    assert_token!("=", [Eq(1)]);
    assert_token!("==", [Eq2(2)]);
    assert_token!(">", [Gt(1)]);
    assert_token!(">=", [Ge(2)]);
    assert_token!(">>", [Gt2(2)]);
    assert_token!("@", [At(1)]);
    assert_token!("[", [OpenBracket(1)]);
    assert_token!("]", [CloseBracket(1)]);
    assert_token!("^", [Caret(1)]);
    assert_token!("{", [OpenBrace(1)]);
    assert_token!("|", [Pipe(1)]);
    assert_token!("}", [CloseBrace(1)]);
    assert_token!("~", [Tilde(1)]);
    assert_token!("_", [Underscore(1)]);
}

#[test]
fn ident() {
    assert_token!("x", [Ident(1)]);
    assert_token!("foo", [Ident(3)]);
    assert_token!("_bar", [Ident(4)]);
    assert_token!("__", [Ident(2)]);
}

#[test]
fn invalid_ident() {
    assert_token!("🦀", [InvalidIdent(4)]);
}
