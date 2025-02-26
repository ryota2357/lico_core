use std::{
    borrow::Borrow,
    ops::{Deref, DerefMut},
};

use lean_string::LeanString;

// newtype LString = LeanString
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct LString(LeanString);

impl LString {
    pub const fn new() -> Self {
        LString(LeanString::new())
    }
}

impl Deref for LString {
    type Target = LeanString;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for LString {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for LString {
    fn default() -> Self {
        LString::new()
    }
}

impl AsRef<str> for LString {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl Borrow<str> for LString {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}

impl<T: Into<LeanString>> From<T> for LString {
    fn from(s: T) -> Self {
        LString(s.into())
    }
}
