//! This file is generated by `cargo codegen syntax`, do not edit by hand.
#![cfg_attr(rustfmt, rustfmt::skip)]

#[allow(bad_style)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u8)]
pub enum SyntaxKind {
    ERROR,
    COMMENT,
    WHITESPACE,
    IDENT,
    SHEBANG,
    INT,
    FLOAT,
    STRING,
    TRUE,
    FALSE,
    NIL,
    AND_KW,
    BREAK_KW,
    CONTINUE_KW,
    DO_KW,
    ELIF_KW,
    ELSE_KW,
    END_KW,
    FOR_KW,
    FUNC_KW,
    IF_KW,
    IN_KW,
    NOT_KW,
    OR_KW,
    RETURN_KW,
    THEN_KW,
    TYPEOF_KW,
    VAR_KW,
    WHILE_KW,
    AMP,
    ARROW,
    AT,
    BANG_EQ,
    CARET,
    CLOSE_BRACE,
    CLOSE_BRACKET,
    CLOSE_PAREN,
    COMMA,
    DOT,
    DOT_2,
    EQ,
    EQ_2,
    GE,
    GT,
    GT_2,
    LE,
    LT,
    LT_2,
    MINUS,
    OPEN_BRACE,
    OPEN_BRACKET,
    OPEN_PAREN,
    PERCENT,
    PIPE,
    PLUS,
    SLASH,
    STAR,
    TILDE,
    UNDERSCORE,
    ARG_LIST,
    ARRAY_EXPR,
    ATTR_STMT,
    BINARY_EXPR,
    BREAK_STMT,
    CALL_EXPR,
    CONTINUE_STMT,
    DO_EXPR,
    ELIF_BRANCH,
    ELSE_BRANCH,
    FIELD_EXPR,
    FOR_STMT,
    FUNC_EXPR,
    FUNC_STMT,
    IF_EXPR,
    INDEX_EXPR,
    LITERAL,
    LOCAL,
    METHOD_CALL_EXPR,
    NAME,
    PARAM,
    PARAM_LIST,
    PAREN_EXPR,
    PREFIX_EXPR,
    RETURN_STMT,
    SOURCE_FILE,
    TABLE_EXPR,
    TABLE_FIELD,
    TABLE_FIELD_NAME_EXPR,
    VAR_STMT,
    WHILE_STMT,
    WILDCARD_PAT,
}
const fn _static_assert_size() { const { assert!(size_of::<SyntaxKind>() == 1) } }
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self { Self(kind as u16) }
}
impl From<rowan::SyntaxKind> for SyntaxKind {
    fn from(kind: rowan::SyntaxKind) -> Self {
        assert!(kind.0 <= (91usize as u16), "bad SyntaxKind: {:?}", kind);
        unsafe { ::core::mem::transmute(kind.0 as u8) }
    }
}
impl SyntaxKind {
    pub fn is_literal(self) -> bool {
        matches!(
            self,
            SyntaxKind::INT
                | SyntaxKind::FLOAT
                | SyntaxKind::STRING
                | SyntaxKind::TRUE
                | SyntaxKind::FALSE
                | SyntaxKind::NIL
        )
    }
    pub fn is_keyword(self) -> bool {
        matches!(
            self,
            SyntaxKind::AND_KW
                | SyntaxKind::BREAK_KW
                | SyntaxKind::CONTINUE_KW
                | SyntaxKind::DO_KW
                | SyntaxKind::ELIF_KW
                | SyntaxKind::ELSE_KW
                | SyntaxKind::END_KW
                | SyntaxKind::FOR_KW
                | SyntaxKind::FUNC_KW
                | SyntaxKind::IF_KW
                | SyntaxKind::IN_KW
                | SyntaxKind::NOT_KW
                | SyntaxKind::OR_KW
                | SyntaxKind::RETURN_KW
                | SyntaxKind::THEN_KW
                | SyntaxKind::TYPEOF_KW
                | SyntaxKind::VAR_KW
                | SyntaxKind::WHILE_KW
        )
    }
    pub fn is_punctuation(self) -> bool {
        matches!(
            self,
            SyntaxKind::AMP
                | SyntaxKind::ARROW
                | SyntaxKind::AT
                | SyntaxKind::BANG_EQ
                | SyntaxKind::CARET
                | SyntaxKind::CLOSE_BRACE
                | SyntaxKind::CLOSE_BRACKET
                | SyntaxKind::CLOSE_PAREN
                | SyntaxKind::COMMA
                | SyntaxKind::DOT
                | SyntaxKind::DOT_2
                | SyntaxKind::EQ
                | SyntaxKind::EQ_2
                | SyntaxKind::GE
                | SyntaxKind::GT
                | SyntaxKind::GT_2
                | SyntaxKind::LE
                | SyntaxKind::LT
                | SyntaxKind::LT_2
                | SyntaxKind::MINUS
                | SyntaxKind::OPEN_BRACE
                | SyntaxKind::OPEN_BRACKET
                | SyntaxKind::OPEN_PAREN
                | SyntaxKind::PERCENT
                | SyntaxKind::PIPE
                | SyntaxKind::PLUS
                | SyntaxKind::SLASH
                | SyntaxKind::STAR
                | SyntaxKind::TILDE
                | SyntaxKind::UNDERSCORE
        )
    }
}
#[macro_export]
#[doc(hidden)]
macro_rules! __token_kind_fast_accsess {
    (shebang) => {
        $crate::SyntaxKind::SHEBANG
    };
    (@) => {
        $crate::SyntaxKind::AT
    };
    ('[') => {
        $crate::SyntaxKind::OPEN_BRACKET
    };
    (ident) => {
        $crate::SyntaxKind::IDENT
    };
    (']') => {
        $crate::SyntaxKind::CLOSE_BRACKET
    };
    (break) => {
        $crate::SyntaxKind::BREAK_KW
    };
    (continue) => {
        $crate::SyntaxKind::CONTINUE_KW
    };
    (for) => {
        $crate::SyntaxKind::FOR_KW
    };
    (in) => {
        $crate::SyntaxKind::IN_KW
    };
    (func) => {
        $crate::SyntaxKind::FUNC_KW
    };
    (end) => {
        $crate::SyntaxKind::END_KW
    };
    (return) => {
        $crate::SyntaxKind::RETURN_KW
    };
    (var) => {
        $crate::SyntaxKind::VAR_KW
    };
    (=) => {
        $crate::SyntaxKind::EQ
    };
    (while) => {
        $crate::SyntaxKind::WHILE_KW
    };
    (,) => {
        $crate::SyntaxKind::COMMA
    };
    (and) => {
        $crate::SyntaxKind::AND_KW
    };
    (or) => {
        $crate::SyntaxKind::OR_KW
    };
    (==) => {
        $crate::SyntaxKind::EQ_2
    };
    (!=) => {
        $crate::SyntaxKind::BANG_EQ
    };
    (<=) => {
        $crate::SyntaxKind::LE
    };
    (>=) => {
        $crate::SyntaxKind::GE
    };
    (<) => {
        $crate::SyntaxKind::LT
    };
    (>) => {
        $crate::SyntaxKind::GT
    };
    (+) => {
        $crate::SyntaxKind::PLUS
    };
    (*) => {
        $crate::SyntaxKind::STAR
    };
    (-) => {
        $crate::SyntaxKind::MINUS
    };
    (/) => {
        $crate::SyntaxKind::SLASH
    };
    (%) => {
        $crate::SyntaxKind::PERCENT
    };
    (<<) => {
        $crate::SyntaxKind::LT_2
    };
    (>>) => {
        $crate::SyntaxKind::GT_2
    };
    (..) => {
        $crate::SyntaxKind::DOT_2
    };
    (^) => {
        $crate::SyntaxKind::CARET
    };
    (|) => {
        $crate::SyntaxKind::PIPE
    };
    (&) => {
        $crate::SyntaxKind::AMP
    };
    ('(') => {
        $crate::SyntaxKind::OPEN_PAREN
    };
    (')') => {
        $crate::SyntaxKind::CLOSE_PAREN
    };
    (do) => {
        $crate::SyntaxKind::DO_KW
    };
    (.) => {
        $crate::SyntaxKind::DOT
    };
    (if) => {
        $crate::SyntaxKind::IF_KW
    };
    (then) => {
        $crate::SyntaxKind::THEN_KW
    };
    (elif) => {
        $crate::SyntaxKind::ELIF_KW
    };
    (else) => {
        $crate::SyntaxKind::ELSE_KW
    };
    (int) => {
        $crate::SyntaxKind::INT
    };
    (float) => {
        $crate::SyntaxKind::FLOAT
    };
    (string) => {
        $crate::SyntaxKind::STRING
    };
    (true) => {
        $crate::SyntaxKind::TRUE
    };
    (false) => {
        $crate::SyntaxKind::FALSE
    };
    (nil) => {
        $crate::SyntaxKind::NIL
    };
    (->) => {
        $crate::SyntaxKind::ARROW
    };
    (~) => {
        $crate::SyntaxKind::TILDE
    };
    (not) => {
        $crate::SyntaxKind::NOT_KW
    };
    (typeof) => {
        $crate::SyntaxKind::TYPEOF_KW
    };
    ('{') => {
        $crate::SyntaxKind::OPEN_BRACE
    };
    ('}') => {
        $crate::SyntaxKind::CLOSE_BRACE
    };
    (_) => {
        $crate::SyntaxKind::UNDERSCORE
    };
}
pub use __token_kind_fast_accsess as T;
