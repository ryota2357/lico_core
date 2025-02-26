use super::*;
use std::{cell::Cell, fmt, mem::forget, ptr::NonNull};

pub struct LArray {
    ptr: NonNull<LArrayInner>,
}

pub(super) struct LArrayInner {
    data: Vec<Object>,
    version: u64,
    ref_count: Cell<usize>,
    color: Cell<Color>,
}

gc_trait_impls! {
    impl GcObject, Clone, Drop for LArray;
    impl GcObjectInner for LArrayInner {
        fn iter_children_mut(&mut self) -> impl Iterator<Item = GcTraceMutRef<'_>> {
            self.data.iter_mut().filter_map(|x| match x {
                Object::Array(array) => Some(GcTraceMutRef::Array(array)),
                Object::Table(table) => Some(GcTraceMutRef::Table(table)),
                Object::Function(function) => Some(GcTraceMutRef::Function(function)),
                _ => None,
            })
        }
        fn release_children(&mut self) {
            for child in self.data.drain(..) {
                match child {
                    Object::Function(func) => forget(func),
                    Object::Array(array) => forget(array),
                    Object::Table(table) => forget(table),
                    _ => {}
                }
            }
        }
    }
}

impl LArray {
    pub fn new() -> Self {
        Self::from(Vec::new())
    }

    pub fn version(&self) -> u64 {
        self.inner().version
    }

    pub fn len(&self) -> usize {
        self.inner().data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner().data.is_empty()
    }

    pub fn get(&self, index: usize) -> Option<&Object> {
        self.inner().data.get(index)
    }

    pub fn set(&mut self, index: usize, value: Object) {
        self.inner_data_mut()[index] = value;
    }

    pub fn push(&mut self, value: Object) {
        self.inner_data_mut().push(value);
    }

    pub fn pop(&mut self) -> Option<Object> {
        self.inner_data_mut().pop()
    }

    pub fn insert(&mut self, index: usize, value: Object) {
        self.inner_data_mut().insert(index, value);
    }

    pub fn remove(&mut self, index: usize) -> Object {
        self.inner_data_mut().remove(index)
    }

    pub fn clear(&mut self) {
        self.inner_data_mut().clear();
    }

    pub fn contains(&self, value: &Object) -> bool {
        self.inner().data.contains(value)
    }

    fn inner_data_mut(&mut self) -> &mut Vec<Object> {
        let inner_mut = unsafe { self.inner_mut() };
        inner_mut.version += 1;
        &mut inner_mut.data
    }
}

impl<T: Into<Vec<Object>>> From<T> for LArray {
    fn from(data: T) -> Self {
        let ptr = Box::leak(Box::new(LArrayInner {
            data: data.into(),
            version: 0,
            ref_count: Cell::new(1),
            color: Cell::new(Color::Black),
        }));
        LArray { ptr: NonNull::from(ptr) }
    }
}

impl Default for LArray {
    fn default() -> Self {
        LArray::new()
    }
}

impl fmt::Debug for LArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.inner().data.iter()).finish()
    }
}

// TODO: cycle detection
impl PartialEq for LArray {
    fn eq(&self, other: &Self) -> bool {
        self.inner().data == other.inner().data
    }
}
