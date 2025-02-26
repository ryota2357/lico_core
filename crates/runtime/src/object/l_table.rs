use super::*;
use std::{borrow::Borrow, cell::Cell, fmt, hash::Hash, mem::forget, ptr::NonNull};

type HashMap<K, V> = hashbrown::HashMap<K, V, foldhash::quality::RandomState>;

pub struct LTable {
    ptr: NonNull<LTableInner>,
}

pub(super) struct LTableInner {
    data: HashMap<LString, Object>,
    // TODO: add metatable
    ref_count: Cell<usize>,
    color: Cell<Color>,
}

gc_trait_impls! {
    impl GcObject, Clone, Drop for LTable;
    impl GcObjectInner for LTableInner {
        fn iter_children_mut(&mut self) -> impl Iterator<Item = GcTraceMutRef<'_>> {
            self.data.iter_mut().filter_map(|(_, v)| match v {
                Object::Array(array) => Some(GcTraceMutRef::Array(array)),
                Object::Table(table) => Some(GcTraceMutRef::Table(table)),
                Object::Function(function) => Some(GcTraceMutRef::Function(function)),
                _ => None,
            })
        }
        fn release_children(&mut self) {
            for (_, child) in self.data.drain() {
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

impl LTable {
    pub fn new() -> Self {
        LTable::from([])
    }

    pub fn len(&self) -> usize {
        self.inner().data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner().data.is_empty()
    }

    pub fn get<Q>(&self, key: &Q) -> Option<&Object>
    where
        LString: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.inner().data.get(key)
    }

    pub fn insert(&mut self, key: LString, value: Object) -> Option<Object> {
        self.inner_data_mut().insert(key, value)
    }

    pub fn remove<Q>(&mut self, key: &Q) -> Option<Object>
    where
        LString: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.inner_data_mut().remove(key)
    }

    pub fn clear(&mut self) {
        self.inner_data_mut().clear();
    }

    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        LString: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.inner().data.contains_key(key)
    }

    fn inner_data_mut(&mut self) -> &mut HashMap<LString, Object> {
        unsafe { &mut self.inner_mut().data }
    }
}

impl FromIterator<(LString, Object)> for LTable {
    fn from_iter<T: IntoIterator<Item = (LString, Object)>>(iter: T) -> Self {
        let ptr = Box::leak(Box::new(LTableInner {
            data: iter.into_iter().collect(),
            ref_count: Cell::new(1),
            color: Cell::new(Color::Black),
        }));
        LTable { ptr: NonNull::from(ptr) }
    }
}

impl<const N: usize> From<[(LString, Object); N]> for LTable {
    fn from(data: [(LString, Object); N]) -> Self {
        data.into_iter().collect()
    }
}

impl Default for LTable {
    fn default() -> Self {
        LTable::new()
    }
}

impl fmt::Debug for LTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.inner().data.iter()).finish()
    }
}

// TODO: cycle detection
impl PartialEq for LTable {
    fn eq(&self, other: &Self) -> bool {
        self.inner().data == other.inner().data
    }
}
