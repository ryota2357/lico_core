use super::*;

pub(super) struct Context {
    pub(super) storage: hir::Storage,
    pub(super) bindings: BindingTable,
    pub(super) loops: LoopCounter,
    pub(super) errors: Vec<SyntaxError>,
}

impl Context {
    pub(super) fn new() -> Self {
        Context {
            storage: hir::Storage::new(),
            bindings: BindingTable::new(),
            loops: LoopCounter::new(),
            errors: Vec::new(),
        }
    }
}

pub(super) use loops::*;
mod loops {
    use core::mem::forget;

    pub(crate) struct LoopCounter {
        count: u32,
    }

    impl LoopCounter {
        pub(super) fn new() -> Self {
            LoopCounter { count: 0 }
        }

        pub(crate) fn is_in_loop(&self) -> bool {
            self.count > 0
        }

        pub(crate) fn start(&mut self) -> LoopMarker {
            self.count = self.count.checked_add(1).expect("overflow");
            LoopMarker { _0: () }
        }
    }

    #[must_use]
    pub(crate) struct LoopMarker {
        // This field is used to prevent the other modules from constructing this struct.
        _0: (),
    }

    impl LoopMarker {
        pub(crate) fn finish(self, counter: &mut LoopCounter) {
            // Never underflow because the `LoopMarker` is constructed by `start` method.
            counter.count -= 1;
            forget(self)
        }
    }

    impl Drop for LoopMarker {
        fn drop(&mut self) {
            if !std::thread::panicking() {
                panic!("LoopMarker must be completed with finish() method");
            }
        }
    }
}

pub(super) use binding::*;
mod binding {
    use core::{hash::Hash, hint, mem};
    use lean_string::LeanString;
    use rustc_hash::FxHashMap;
    use std::borrow::Borrow;
    use syntax::hir::SymbolId;

    // NOTE:
    // A simple implementation of BindingTable is as follows:
    //
    // pub struct BindingTable {
    //    scopes: Vec<HashMap<String, SymbolId>>,
    //    next_id: SymbolId,
    // }
    //
    // Where `scopes` is a stack of scopes, and `next_id` is the next symbol id.
    // When we add a new symbol, we add it to the top scope.
    // When we resolve a symbol, we iterate scopes from top to bottom.
    //
    // The following implementation is more efficient than the above implementation.

    struct Node {
        id: SymbolId,
        name: LeanString,
        prev: Option<u32>, // index of the `nodes` vec.
    }

    pub(crate) struct BindingTable {
        map: FxHashMap<LeanString, u32>, // index of the `nodes` vec.
        nodes: Vec<Node>,                // arena of nodes.
        stack: Vec<u32>,                 // scope separators of the `nodes` vec.
        next_id: SymbolId,
    }

    impl BindingTable {
        pub(super) fn new() -> Self {
            BindingTable {
                map: FxHashMap::default(),
                nodes: Vec::new(),
                stack: vec![0],
                next_id: SymbolId::new(0),
            }
        }

        pub(crate) fn start_scope(&mut self) -> ScopeMarker {
            self.stack.push(self.nodes.len() as u32);
            ScopeMarker { _0: () }
        }

        pub(crate) fn add(&mut self, name: LeanString) -> SymbolId {
            let id = self.next_id;
            let node = Node { id, name: name.clone(), prev: self.map.get(&name).copied() };

            // Ensure that the next_id doesn't overflow u32.
            // From this, the length of `self.nodes` also doesn't overflow u32 because it is equal
            // or less than `self.next_id`.
            self.next_id = SymbolId::new(self.next_id.raw().checked_add(1).expect("overflow"));
            self.map.insert(name, self.nodes.len() as u32);
            self.nodes.push(node);

            id
        }

        pub(crate) fn resolve<Q>(&self, name: &Q) -> Option<SymbolId>
        where
            Q: ?Sized + Hash + Eq,
            LeanString: Borrow<Q>,
        {
            let idx = *self.map.get(name)? as usize;
            Some(self.nodes[idx].id)
        }

        pub(crate) fn symbol_count(&self) -> usize {
            self.next_id.raw() as usize
        }
    }

    #[must_use]
    pub(crate) struct ScopeMarker {
        // This field is used to prevent the other modules from constructing this struct.
        _0: (),
    }

    impl ScopeMarker {
        pub(crate) fn finish(self, table: &mut BindingTable) {
            let Some(scope_start) = table.stack.pop() else {
                // This is a bug. because the `ScopeMarker` is constructed by `start_scope` method,
                // the stack must not be empty.
                if cfg!(debug_assertions) {
                    unreachable!("scope_start must be Some(_)");
                } else {
                    unsafe { hint::unreachable_unchecked() }
                }
            };
            let delete_nodes = table.nodes.drain(scope_start as usize..);
            for node in delete_nodes.rev() {
                if let Some(prev) = node.prev {
                    table.map.insert(node.name, prev);
                } else {
                    table.map.remove(&node.name);
                }
            }
            mem::forget(self);
        }
    }

    impl Drop for ScopeMarker {
        fn drop(&mut self) {
            if !std::thread::panicking() {
                panic!("ScopeMarker must be completed with finish() method");
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::BindingTable;
        use lean_string::LeanString;

        #[test]
        fn simple_variable_definition_and_resolution() {
            let mut table = BindingTable::new();
            let m = table.start_scope();
            let x_id = table.add(LeanString::from("x"));
            assert_eq!(table.resolve(&LeanString::from("x")), Some(x_id));
            m.finish(&mut table);
        }

        #[test]
        fn shadowing_in_nested_scope() {
            let mut table = BindingTable::new();

            let outer = table.start_scope();
            let x_id1 = table.add(LeanString::from("x"));
            assert_eq!(table.resolve(&LeanString::from("x")), Some(x_id1));

            let inner = table.start_scope();
            let x_id2 = table.add(LeanString::from("x"));
            assert_eq!(table.resolve(&LeanString::from("x")), Some(x_id2));

            let inner2 = table.start_scope();
            let x_id3 = table.add(LeanString::from("x"));
            assert_eq!(table.resolve(&LeanString::from("x")), Some(x_id3));

            inner2.finish(&mut table);
            assert_eq!(table.resolve(&LeanString::from("x")), Some(x_id2));

            inner.finish(&mut table);
            assert_eq!(table.resolve(&LeanString::from("x")), Some(x_id1));

            outer.finish(&mut table);
        }

        #[test]
        fn undefined_variable() {
            let table = BindingTable::new();
            assert_eq!(table.resolve(&LeanString::from("y")), None);
        }

        #[test]
        fn exit_scope_removes_bindings() {
            let mut table = BindingTable::new();

            let outer = table.start_scope();
            let a_id = table.add(LeanString::from("a"));

            let inner = table.start_scope();
            table.add(LeanString::from("b"));
            inner.finish(&mut table);

            assert_eq!(table.resolve(&LeanString::from("b")), None);
            assert_eq!(table.resolve(&LeanString::from("a")), Some(a_id));

            outer.finish(&mut table);
            assert_eq!(table.resolve(&LeanString::from("a")), None);
        }

        #[test]
        #[should_panic(expected = "ScopeMarker must be completed with finish() method")]
        fn scope_marker_must_call_finish() {
            let mut table = BindingTable::new();
            let _scope = table.start_scope();
        }

        #[test]
        fn multiple_variables() {
            let mut table = BindingTable::new();

            let outer = table.start_scope();
            let x_id = table.add(LeanString::from("x"));
            let y_id = table.add(LeanString::from("y"));

            assert_eq!(table.resolve(&LeanString::from("x")), Some(x_id));
            assert_eq!(table.resolve(&LeanString::from("y")), Some(y_id));

            let inner = table.start_scope();
            let z_id = table.add(LeanString::from("z"));
            assert_eq!(table.resolve(&LeanString::from("z")), Some(z_id));

            inner.finish(&mut table);
            assert_eq!(table.resolve(&LeanString::from("z")), None);

            outer.finish(&mut table);
            assert_eq!(table.resolve(&LeanString::from("x")), None);
            assert_eq!(table.resolve(&LeanString::from("y")), None);
        }
    }
}
