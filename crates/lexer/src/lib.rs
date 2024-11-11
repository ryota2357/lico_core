mod cursor;
use cursor::Cursor;

mod is_x_char;
use is_x_char::*;

use std::{char, hint::unreachable_unchecked};
use syntax::token::{TokenKind::*, *};

struct TokenStreamImpl<'src, I: Iterator<Item = Token>> {
    source: &'src str,
    iter: I,
}

impl<I: Iterator<Item = Token>> Iterator for TokenStreamImpl<'_, I> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}
impl<'s, I: Iterator<Item = Token>> TokenStream<'s> for TokenStreamImpl<'s, I> {
    fn source(&self) -> &'s str {
        self.source
    }
}

pub fn tokenize(source: &str) -> impl TokenStream<'_> {
    let mut cursor = Cursor::new(source);
    TokenStreamImpl { source, iter: core::iter::from_fn(move || advance_token(&mut cursor)) }
}

fn advance_token(cursor: &mut Cursor) -> Option<Token> {
    #[rustfmt::skip]
    let kind = match cursor.next()? {
        '!' => match cursor.peek() { Some('=') => { cursor.next(); BangEq } _ => Bang, },
        '%' => Percent,
        '&' => Amp,
        '(' => OpenParen,
        ')' => CloseParen,
        '*' => Star,
        '+' => Plus,
        ',' => Comma,
        '-' => match cursor.peek() { Some('>') => { cursor.next(); Arrow } _ => Minus },
        '.' => match cursor.peek() { Some('.') => { cursor.next(); Dot2 } _ => Dot },
        '/' => Slash,
        '<' => match cursor.peek() { Some('=') => { cursor.next(); Le  } Some('<') => { cursor.next(); Lt2 } _ => Lt },
        '=' => match cursor.peek() { Some('=') => { cursor.next(); Eq2 } _ => Eq, },
        '>' => match cursor.peek() { Some('=') => { cursor.next(); Ge  } Some('>') => { cursor.next(); Gt2 } _ => Gt },
        '@' => At,
        '[' => OpenBracket,
        ']' => CloseBracket,
        '^' => Caret,
        '{' => OpenBrace,
        '|' => Pipe,
        '}' => CloseBrace,
        '~' => Tilde,
        '#'  => comment(cursor),
        '"'  => string::<'"'>(cursor),
        '\'' => string::<'\''>(cursor),
        c @ '0'..='9'               => number(cursor, c),
        c if is_whitespace_char(c)  => whitespace(cursor),
        c if is_ident_start_char(c) => ident(cursor, c),
        c if is_emoji_char(c)       => invalid_ident(cursor),
        _ => Unknown,
    };
    let token = cursor.bump(kind);
    Some(token)
}

fn comment(cursor: &mut Cursor) -> TokenKind {
    debug_assert!(cursor.prev() == '#');
    cursor.eat_while(|c| c != '\n');
    LineComment
}

fn string<const Q: char>(cursor: &mut Cursor) -> TokenKind {
    const { assert!(Q == '"' || Q == '\'') };
    debug_assert!(cursor.prev() == Q);

    let mut terminated = false;
    while let Some(c) = cursor.next() {
        match c {
            '\\' => {
                let peek = cursor.peek();
                if peek == Some(Q) || peek == Some('\\') {
                    cursor.next();
                }
            }
            q if q == Q => {
                terminated = true;
                break;
            }
            _ => {}
        }
    }
    String {
        terminated,
        quote_kind: match Q {
            '"' => QuoteKind::Double,
            '\'' => QuoteKind::Single,
            // SAFETY: `Q` is asserted to be either '"' or '\'' at first line of this function.
            _ => unsafe { unreachable_unchecked() },
        },
    }
}

fn number(cursor: &mut Cursor, first_digit: char) -> TokenKind {
    debug_assert!(first_digit.is_ascii_digit()); // 0..=9

    fn eat_decimal_digits(cursor: &mut Cursor) -> bool {
        let mut has_digits = false;
        while let Some(c) = cursor.peek() {
            match c {
                '_' => {
                    cursor.next();
                }
                '0'..='9' => {
                    has_digits = true;
                    cursor.next();
                }
                _ => break,
            }
        }
        has_digits
    }
    fn eat_hexadecimal_digits(cursor: &mut Cursor) -> bool {
        let mut has_digits = false;
        while let Some(c) = cursor.peek() {
            match c {
                '_' => {
                    cursor.next();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    cursor.next();
                }
                _ => break,
            }
        }
        has_digits
    }

    let mut base = NumBase::Decimal;
    if first_digit == '0' {
        match cursor.peek() {
            Some('b') => {
                base = NumBase::Binary;
                cursor.next();
                if !eat_decimal_digits(cursor) {
                    return Int { base, empty_int: true };
                }
            }
            Some('o') => {
                base = NumBase::Octal;
                cursor.next();
                if !eat_decimal_digits(cursor) {
                    return Int { base, empty_int: true };
                }
            }
            Some('x') => {
                base = NumBase::Hexadecimal;
                cursor.next();
                if !eat_hexadecimal_digits(cursor) {
                    return Int { base, empty_int: true };
                }
            }
            Some('0'..='9' | '_') => {
                eat_decimal_digits(cursor);
            }
            Some('.' | 'e' | 'E') => {}
            _ => {
                // "0"
                return Int { base, empty_int: false };
            }
        }
    } else {
        eat_decimal_digits(cursor);
    }

    fn eat_float_exponent(cursor: &mut Cursor) -> bool {
        assert!(matches!(cursor.next(), Some('e' | 'E')));
        if let Some('-') | Some('+') = cursor.peek() {
            cursor.next();
        }
        eat_decimal_digits(cursor)
    }

    match cursor.peek() {
        Some('.') => {
            cursor.next();
            let empty_exponent = match cursor.peek() {
                Some('0'..='9') => {
                    eat_decimal_digits(cursor);
                    match cursor.peek() {
                        Some('e' | 'E') => !eat_float_exponent(cursor),
                        _ => false,
                    }
                }
                _ => false,
            };
            Float { base, empty_exponent }
        }
        Some('e' | 'E') => Float { base, empty_exponent: !eat_float_exponent(cursor) },
        _ => Int { base, empty_int: false },
    }
}

fn whitespace(cursor: &mut Cursor) -> TokenKind {
    debug_assert!(is_whitespace_char(cursor.prev()));
    cursor.eat_while(is_whitespace_char);
    Whitespace
}

fn ident(cursor: &mut Cursor, first_char: char) -> TokenKind {
    debug_assert!(is_ident_start_char(cursor.prev()));

    fn keyword_trie_tree(cursor: &mut Cursor, first_char: char) -> Option<TokenKind> {
        fn next_if_s(
            cursor: &mut Cursor,
            s: &'static [char],
            kind: TokenKind,
        ) -> Option<TokenKind> {
            for c in s {
                if cursor.peek()? == *c {
                    cursor.next();
                } else {
                    return None;
                }
            }
            Some(kind)
        }

        fn next_if_c(cursor: &mut Cursor, c: char, kind: TokenKind) -> Option<TokenKind> {
            if cursor.peek()? == c {
                cursor.next();
                Some(kind)
            } else {
                None
            }
        }

        let pre_match = match first_char {
            'a' => next_if_s(cursor, &['n', 'd'], And),
            'b' => next_if_s(cursor, &['r', 'e', 'a', 'k'], Break),
            'c' => next_if_s(cursor, &['o', 'n', 't', 'i', 'n', 'u', 'e'], Continue),
            'd' => next_if_c(cursor, 'o', Do),
            'e' => match cursor.peek()? {
                'l' => {
                    cursor.next();
                    match cursor.peek()? {
                        'i' => {
                            cursor.next();
                            next_if_c(cursor, 'f', Elif)
                        }
                        's' => {
                            cursor.next();
                            next_if_c(cursor, 'e', Else)
                        }
                        _ => None,
                    }
                }
                'n' => {
                    cursor.next();
                    next_if_c(cursor, 'd', End)
                }
                _ => None,
            },
            'f' => match cursor.peek()? {
                'a' => {
                    cursor.next();
                    next_if_s(cursor, &['l', 's', 'e'], False)
                }
                'o' => {
                    cursor.next();
                    next_if_c(cursor, 'r', For)
                }
                'u' => {
                    cursor.next();
                    next_if_s(cursor, &['n', 'c'], Func)
                }
                _ => None,
            },
            'i' => match cursor.peek()? {
                'f' => {
                    cursor.next();
                    Some(If)
                }
                'n' => {
                    cursor.next();
                    Some(In)
                }
                _ => None,
            },
            'n' => match cursor.peek()? {
                'i' => {
                    cursor.next();
                    next_if_c(cursor, 'l', Nil)
                }
                'o' => {
                    cursor.next();
                    next_if_c(cursor, 't', Not)
                }
                _ => None,
            },
            'o' => next_if_c(cursor, 'r', Or),
            'r' => next_if_s(cursor, &['e', 't', 'u', 'r', 'n'], Return),
            't' => match cursor.peek()? {
                'h' => {
                    cursor.next();
                    next_if_s(cursor, &['e', 'n'], Then)
                }
                'r' => {
                    cursor.next();
                    next_if_s(cursor, &['u', 'e'], True)
                }
                'y' => {
                    cursor.next();
                    next_if_s(cursor, &['p', 'e', 'o', 'f'], TypeOf)
                }
                _ => None,
            },
            'v' => next_if_s(cursor, &['a', 'r'], Var),
            'w' => next_if_s(cursor, &['h', 'i', 'l', 'e'], While),
            _ => None,
        };
        if pre_match.is_some() {
            let Some(c) = cursor.peek() else {
                return pre_match;
            };
            if is_ident_continue_char(c) {
                None
            } else {
                pre_match
            }
        } else {
            None
        }
    }
    keyword_trie_tree(cursor, first_char).unwrap_or_else(|| match cursor.peek() {
        Some(c) if is_ident_continue_char(c) => {
            cursor.eat_while(is_ident_continue_char);
            Ident
        }
        _ if first_char == '_' => Underscore,
        _ => Ident,
    })
}

fn invalid_ident(cursor: &mut Cursor) -> TokenKind {
    debug_assert!(is_emoji_char(cursor.prev()));
    cursor.eat_while(|c| is_ident_continue_char(c) || is_emoji_char(c) || c == '\u{200D}');
    InvalidIdent
}
