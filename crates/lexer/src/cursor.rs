use core::str::Chars;
use syntax::token::*;

#[derive(Debug)]
pub(crate) struct Cursor<'src> {
    chars: Chars<'src>,
    remaining_len: usize,
    #[cfg(debug_assertions)]
    prev: char,
}

impl<'src> Cursor<'src> {
    pub(crate) fn new(source: &'src str) -> Self {
        if source.len() > u32::MAX as usize {
            panic!("Source code is too large");
        }
        Cursor {
            chars: source.chars(),
            remaining_len: source.len(),
            #[cfg(debug_assertions)]
            prev: '\0',
        }
    }

    /// Returns the last eaten symbol. (For debug assertions only.)
    pub(crate) fn prev(&self) -> char {
        #[cfg(debug_assertions)]
        {
            self.prev
        }
        #[cfg(not(debug_assertions))]
        {
            unreachable!("Cursor::prev() is used outside of debug mode")
        }
    }

    /// Consumes the next symbol if it satisfies the predicate or until the end of the input.
    pub(crate) fn eat_while(&mut self, pred: impl Fn(char) -> bool) {
        while let Some(c) = self.peek() {
            if pred(c) {
                self.next();
            } else {
                break;
            }
        }
    }

    /// Peeks the next symbol from the input stream without consuming it.
    pub(crate) fn peek(&self) -> Option<char> {
        // `.next()` optimizes better than `.nth(0)`
        self.chars.clone().next()
    }

    pub(crate) fn bump(&mut self, kind: TokenKind) -> Token {
        let current_len = self.chars.as_str().len();
        let len = (self.remaining_len - current_len) as u32;
        self.remaining_len = current_len;
        Token { kind, len }
    }
}

impl Iterator for Cursor<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.chars.next()?;
        #[cfg(debug_assertions)]
        {
            self.prev = c;
        }
        Some(c)
    }
}
