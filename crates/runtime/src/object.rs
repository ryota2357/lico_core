mod gc;
use gc::*;

mod l_string;
pub use l_string::LString;

mod l_array;
pub use l_array::LArray;

mod l_table;
pub use l_table::LTable;

mod l_function;
pub use l_function::{LFCapture, LFunction};

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
    String(LString),
    Array(LArray),
    Table(LTable),
    Function(LFunction),
}

fn _size_check() {
    const {
        assert!(size_of::<Object>() == 16);
        assert!(size_of::<Option<Object>>() == 16);
    }
}

impl From<i64> for Object {
    fn from(val: i64) -> Self {
        Object::Int(val)
    }
}

impl From<f64> for Object {
    fn from(val: f64) -> Self {
        Object::Float(val)
    }
}

impl From<bool> for Object {
    fn from(val: bool) -> Self {
        Object::Bool(val)
    }
}

impl From<()> for Object {
    fn from(_: ()) -> Self {
        Object::Nil
    }
}

impl From<LString> for Object {
    fn from(val: LString) -> Self {
        Object::String(val)
    }
}

impl From<LArray> for Object {
    fn from(val: LArray) -> Self {
        Object::Array(val)
    }
}

impl From<LTable> for Object {
    fn from(val: LTable) -> Self {
        Object::Table(val)
    }
}
