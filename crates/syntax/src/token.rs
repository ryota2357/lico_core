#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

fn _size_check() {
    const {
        assert!(size_of::<Token>() == 8);
        assert!(size_of::<Token>() == size_of::<Option<Token>>());
    }
}

pub trait TokenStream<'src>: Iterator<Item = Token> {
    fn source(&self) -> &'src str;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// "# comment"
    LineComment,

    /// Any whitespace character sequence.
    Whitespace,

    /// Identifier that is not classified as a keyword or literal. e.g. "foo"
    Ident,

    /// Like the `Ident`, but containing invalid unicode codepoints.
    InvalidIdent,

    //===== Literal =====
    /// Integer literal. e.g. 42
    Int { base: NumBase, empty_int: bool },
    /// Float literal. e.g. 3.14
    Float { base: NumBase, empty_exponent: bool },
    /// String literal. e.g. "foo"
    String { terminated: bool, quote_kind: QuoteKind },
    /// `true` boolean literal
    True,
    /// `false` boolean literal
    False,
    /// `nil` literal
    Nil,

    //===== Keyword =====
    /// `and` keyword
    And,
    /// `break` keyword
    Break,
    /// `continue` keyword
    Continue,
    /// `do` keyword
    Do,
    /// `elif` keyword
    Elif,
    /// `else` keyword
    Else,
    /// `end` keyword
    End,
    /// `for` keyword
    For,
    /// `func` keyword
    Func,
    /// `if` keyword
    If,
    /// `in` keyword
    In,
    /// `not` keyword
    Not,
    /// `or` keyword
    Or,
    /// `return` keyword
    Return,
    /// `then` keyword
    Then,
    /// `typeof` keyword
    TypeOf,
    /// `var` keyword
    Var,
    /// `while` keyword
    While,

    //===== Punctuation =====
    /// `&` punctuation
    Amp,
    /// `-` punctuation
    Arrow,
    /// `@` punctuation
    At,
    /// `!` punctuation
    Bang,
    /// `!=` punctuation
    BangEq,
    /// `^` punctuation
    Caret,
    /// `}` punctuation
    CloseBrace,
    /// `]` punctuation
    CloseBracket,
    /// `)` punctuation
    CloseParen,
    /// `,` punctuation
    Comma,
    /// `.` punctuation
    Dot,
    /// `..` punctuation
    Dot2,
    /// `=` punctuation
    Eq,
    /// `==` punctuation
    Eq2,
    /// `>=` punctuation
    Ge,
    /// `>` punctuation
    Gt,
    /// `>>` punctuation
    Gt2,
    /// `<=` punctuation
    Le,
    /// `<` punctuation
    Lt,
    /// `<<` punctuation
    Lt2,
    /// `-` punctuation
    Minus,
    /// `{` punctuation
    OpenBrace,
    /// `[` punctuation
    OpenBracket,
    /// `(` punctuation
    OpenParen,
    /// `%` punctuation
    Percent,
    /// `|` punctuation
    Pipe,
    /// `+` punctuation
    Plus,
    /// `/` punctuation
    Slash,
    /// `*` punctuation
    Star,
    /// `~` punctuation
    Tilde,
    /// `_` punctuation
    Underscore,

    /// Unknown character, not expected by the lexer.
    Unknown,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum NumBase {
    /// Binary integer literal that starts with "0b".
    Binary = 2,

    /// Octal integer literal that starts with "0o".
    Octal = 8,

    /// Decimal integer literal.
    Decimal = 10,

    /// Hexadecimal integer literal that starts with "0x".
    Hexadecimal = 16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum QuoteKind {
    /// Single quote string literal.
    Single,

    /// Double quote string literal.
    Double,
}
