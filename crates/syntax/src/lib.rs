mod syntax_kind;
pub use syntax_kind::*;

mod syntax_error;
pub use syntax_error::*;

pub mod ast;
pub mod token;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LicoLanguage {}

impl rowan::Language for LicoLanguage {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        SyntaxKind::from(raw)
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<LicoLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<LicoLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<LicoLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<LicoLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<LicoLanguage>;
pub type Preorder = rowan::api::Preorder<LicoLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<LicoLanguage>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<LicoLanguage>;
pub use rowan::{GreenNode, TextLen, TextRange, TextSize};
