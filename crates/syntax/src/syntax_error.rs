use core::fmt;
use rowan::TextRange;
use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SyntaxError {
    message: Cow<'static, str>,
    range: TextRange,
}

impl SyntaxError {
    pub fn new(message: impl Into<Cow<'static, str>>, range: TextRange) -> Self {
        Self { message: message.into(), range }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn range(&self) -> TextRange {
        self.range
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{:?}: {}", self.range, &self.message)
    }
}
