use rowan::GreenNodeBuilder;
use std::borrow::Cow;
use syntax::{
    token::{QuoteKind, TokenKind, TokenStream},
    GreenNode, SyntaxError, SyntaxKind, TextRange, T,
};

pub(crate) struct TreeBuilder<'s> {
    source: &'s str,
    start_offsets: Vec<u32>,

    syntax_errors: Vec<SyntaxError>,
    range_error_start: Option<u32>,

    builder: GreenNodeBuilder<'static>,
    offset_index: usize,
}

impl<'s> TreeBuilder<'s> {
    pub(crate) fn from_token_stream(token_stream: impl TokenStream<'s>) -> (Self, Vec<SyntaxKind>) {
        let source = token_stream.source();
        if source.len() > u32::MAX as usize {
            panic!("source is too large");
        }

        let mut syntax_kinds = Vec::new();
        let mut start_offsets = Vec::new();
        let mut syntax_errors = Vec::new();
        let mut offset = 0;
        for token in token_stream {
            let (kind, err) = convert_kind(token.kind);
            syntax_kinds.push(kind);
            start_offsets.push(offset);
            if let Some(err) = err {
                let range = TextRange::at(offset.into(), token.len.into());
                syntax_errors.push(SyntaxError::new(err, range))
            }
            offset += token.len;
        }

        assert_eq!(offset, source.len() as u32);
        start_offsets.push(offset);

        let tree_builder = TreeBuilder {
            source,
            start_offsets,
            syntax_errors,
            range_error_start: None,
            builder: GreenNodeBuilder::new(),
            offset_index: 0,
        };
        (tree_builder, syntax_kinds)
    }

    pub(crate) fn push_token(&mut self, kind: SyntaxKind) {
        let (start, end) = self.next_range();
        let text = &self.source[start as usize..end as usize];
        self.builder.token(kind.into(), text);
    }

    pub(crate) fn start_node(&mut self, kind: SyntaxKind) {
        self.builder.start_node(kind.into());
    }

    pub(crate) fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    pub(crate) fn push_error(&mut self, message: Cow<'static, str>) {
        let (start, _) = self.next_range();
        let range = TextRange::empty(start.into());
        self.syntax_errors.push(SyntaxError::new(message, range));
    }

    pub(crate) fn start_error(&mut self) {
        let (start, _) = self.current_range();
        if self.range_error_start.is_some() {
            panic!("nested error is detected while parsing");
        }
        self.range_error_start = Some(start);
    }

    pub(crate) fn finish_error(&mut self, message: Cow<'static, str>) {
        let Some(start) = self.range_error_start.take() else {
            unreachable!();
        };
        let (end, _) = self.current_range();
        let range = TextRange::new(start.into(), end.into());
        self.syntax_errors.push(SyntaxError::new(message, range));
    }

    pub(crate) fn finish(self) -> (GreenNode, Vec<SyntaxError>) {
        (self.builder.finish(), self.syntax_errors)
    }

    fn current_range(&self) -> (u32, u32) {
        let start = self.start_offsets[self.offset_index];
        let end = self.start_offsets[self.offset_index + 1];
        (start, end)
    }

    fn next_range(&mut self) -> (u32, u32) {
        let res = self.current_range();
        self.offset_index += 1;
        res
    }
}

fn convert_kind(kind: TokenKind) -> (SyntaxKind, Option<&'static str>) {
    let mut error = None;
    let kind = match kind {
        TokenKind::LineComment => SyntaxKind::COMMENT,
        TokenKind::Whitespace => SyntaxKind::WHITESPACE,
        TokenKind::Ident => T![ident],
        TokenKind::InvalidIdent => {
            error = Some("Identifiers contains invalid characters");
            T![ident]
        }
        TokenKind::Int { base: _, empty_int } => {
            if empty_int {
                error = Some("Missing digits after the integer base prefix");
            }
            T![int]
        }
        TokenKind::Float { base: _, empty_exponent } => {
            if empty_exponent {
                error = Some("Missing digits after the exponent symbol");
            }
            T![float]
        }
        TokenKind::String { terminated, quote_kind } => {
            if !terminated {
                error = Some(match quote_kind {
                    QuoteKind::Single => {
                        "Missing trailing `'` symbol to terminate the string literal"
                    }
                    QuoteKind::Double => {
                        "Missing trailing `\"` symbol to terminate the string literal"
                    }
                });
            }
            T![string]
        }
        TokenKind::True => T![true],
        TokenKind::False => T![false],
        TokenKind::Nil => T![nil],
        TokenKind::And => T![and],
        TokenKind::Break => T![break],
        TokenKind::Continue => T![continue],
        TokenKind::Do => T![do],
        TokenKind::Elif => T![elif],
        TokenKind::Else => T![else],
        TokenKind::End => T![end],
        TokenKind::For => T![for],
        TokenKind::Func => T![func],
        TokenKind::If => T![if],
        TokenKind::In => T![in],
        TokenKind::Not => T![not],
        TokenKind::Or => T![or],
        TokenKind::Return => T![return],
        TokenKind::Then => T![then],
        TokenKind::TypeOf => T![typeof],
        TokenKind::Var => T![var],
        TokenKind::While => T![while],
        TokenKind::Amp => T![&],
        TokenKind::Arrow => T![->],
        TokenKind::At => T![@],
        TokenKind::Bang => T![!],
        TokenKind::BangEq => T![!=],
        TokenKind::Caret => T![^],
        TokenKind::CloseBrace => T!['}'],
        TokenKind::CloseBracket => T![']'],
        TokenKind::CloseParen => T![')'],
        TokenKind::Comma => T![,],
        TokenKind::Dot => T![.],
        TokenKind::Dot2 => T![..],
        TokenKind::Eq => T![=],
        TokenKind::Eq2 => T![==],
        TokenKind::Ge => T![>=],
        TokenKind::Gt => T![>],
        TokenKind::Gt2 => T![>>],
        TokenKind::Le => T![<=],
        TokenKind::Lt => T![<],
        TokenKind::Lt2 => T![<<],
        TokenKind::Minus => T![-],
        TokenKind::OpenBrace => T!['{'],
        TokenKind::OpenBracket => T!['['],
        TokenKind::OpenParen => T!['('],
        TokenKind::Percent => T![%],
        TokenKind::Pipe => T![|],
        TokenKind::Plus => T![+],
        TokenKind::Slash => T![/],
        TokenKind::Star => T![*],
        TokenKind::Tilde => T![~],
        TokenKind::Underscore => T![_],
        TokenKind::Unknown => SyntaxKind::ERROR,
    };
    (kind, error)
}
