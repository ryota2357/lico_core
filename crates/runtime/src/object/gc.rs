use super::{
    l_array::LArrayInner,
    l_function::{LFCaptureInner, LFunctionInner},
    l_table::LTableInner,
    *,
};
use std::{
    alloc::{Layout, dealloc},
    cell::Cell,
    mem,
    ptr::{self, NonNull},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Color {
    Black,  // Active
    Purple, // Suspicious of circular references
    Gray,   // Checking for circular references
    White,  // Candidates for collection (deallocate)
}

pub(super) trait GcObject {
    type Inner: GcObjectInner;
    fn ptr(&self) -> NonNull<Self::Inner>;
    fn from_inner_ptr(ptr: NonNull<Self::Inner>) -> Self;
    fn inner(&self) -> &Self::Inner {
        debug_assert_ne!(self.ptr(), NonNull::dangling());
        unsafe { self.ptr().as_ref() }
    }
    unsafe fn inner_mut(&mut self) -> &mut Self::Inner {
        debug_assert_ne!(self.ptr(), NonNull::dangling());
        unsafe { self.ptr().as_mut() }
    }
}

pub(super) trait GcObjectInner {
    fn color(&self) -> Color;
    fn paint(&self, color: Color);
    fn ref_count(&self) -> usize;
    fn inc_ref_count(&self);
    fn dec_ref_count(&self);
    fn iter_children_mut(&mut self) -> impl Iterator<Item = GcTraceMutRef<'_>>;
    fn release_children(&mut self);
}

pub(super) enum GcTraceMutRef<'a> {
    Array(&'a mut LArray),
    Table(&'a mut LTable),
    Function(&'a mut LFunction),
    Capture(&'a mut LFCapture),
}

#[macro_export]
#[doc(hidden)]
macro_rules! __gc_object_impl {
    (
        impl GcObject, Clone, Drop for $object_ty:ty;
        impl GcObjectInner for $raw_ty:ty { $($gc_inner_remaining_impls:item)* }
    ) => {
        impl GcObject for $object_ty {
            type Inner = $raw_ty;
            fn ptr(&self) -> NonNull<$raw_ty> {
                self.ptr
            }
            fn from_inner_ptr(ptr: NonNull<$raw_ty>) -> Self {
                Self { ptr }
            }
        }
        impl GcObjectInner for $raw_ty {
            fn color(&self) -> Color {
                self.color.get()
            }
            fn paint(&self, color: Color) {
                self.color.set(color);
            }
            fn ref_count(&self) -> usize {
                self.ref_count.get()
            }
            fn inc_ref_count(&self) {
                let ref_count = self.ref_count().wrapping_add(1);
                self.ref_count.set(ref_count);
                if ref_count == 0 {
                    std::process::abort() // Reference count overflow
                }
            }
            fn dec_ref_count(&self) {
                let ref_count = self.ref_count().wrapping_sub(1);
                self.ref_count.set(ref_count);
                if ref_count == usize::MAX {
                    std::process::abort() // Reference count underflow
                }
            }
            $($gc_inner_remaining_impls)*
        }
        impl Clone for $object_ty {
            fn clone(&self) -> Self {
                self.inner().inc_ref_count();
                Self { ptr: self.ptr }
            }
        }
        impl Drop for $object_ty {
            fn drop(&mut self) {
                $crate::object::gc::custom_drop(self)
            }
        }
    };
}
pub(super) use __gc_object_impl as gc_trait_impls;

thread_local! {
    static REC_DROP_GUARD: Cell<bool> = const { Cell::new(false) };
}
struct RecursiveDropGuard;
impl RecursiveDropGuard {
    fn begin_drop() {
        REC_DROP_GUARD.with(|guard| {
            assert!(!guard.get(), "Recursive call to drop");
            guard.set(true);
        });
    }
    fn end_drop() {
        REC_DROP_GUARD.with(|guard| {
            assert!(guard.get(), "End drop without begin drop");
            guard.set(false);
        });
    }
}

pub(super) fn custom_drop<T: GcObject>(this: &mut T) {
    RecursiveDropGuard::begin_drop();

    if this.inner().color() != Color::Black {
        unreachable!("drop() is called during mark and sweep");
    }

    // First, decrement the reference count.
    this.inner().dec_ref_count();

    // If the reference count is not zero, we can't drop this object.
    // But there is a possibility that this object is a part of a cycle, so we need to do `mark_and_sweep` from this object.
    if this.inner().ref_count() > 0 {
        this.inner().paint(Color::Purple); // Mark as suspicious of cycle reference
        mark_and_sweep::run(this);
        RecursiveDropGuard::end_drop();
        return;
    }

    // If the reference count is zero, we can drop this object.

    // Mark this object as white as it is a candidate for collection.
    this.inner().paint(Color::White);

    // Next, we need to collect objects that can be traced from `this` that are suspected to be circular references.
    //
    // Q: Why do not we call `mem::drop_in_place()` on `this.prt()`?
    // A: It has a performance problems.
    //    Please imagine the following case:
    //      - Root node has only k leaves.
    //      - Each leaf has circular references between leaves.
    //      - `this` is the root node.
    //    In this case, we can drop all leaves, but we can't drop all leaves until we call `drop()` for the last leaf if we
    //    call `mem::drop_in_place()` on `this.ptr()`. At this time, O(k^2) for a graph with k complete leaves.
    //
    // To collect objects for which circular references are suspected, we use `PurpleCollector`.
    // `PurpleCollector` is a struct that collects objects for which circular references are suspected and marks them as purple.
    #[derive(Default)]
    struct PurpleCollector {
        // In the `Object` enum, only `Array` and `Table` are `GcObject`.
        // If you add other `GcObject` variants in the future, you will need to add them here as well.
        array: Vec<NonNull<LArrayInner>>,
        table: Vec<NonNull<LTableInner>>,
        function: Vec<NonNull<LFunctionInner>>,
        capture: Vec<NonNull<LFCaptureInner>>,
    }
    impl PurpleCollector {
        /// Collect purple objects (suspected of circular references) that can be traced from the object pointed to by `ptr`.
        /// White objects found during tracing are applied `gc::deallocate_inner()` recursively.
        ///
        /// Given `ptr` must be `ref_count() == 0`.
        fn collect<I: GcObjectInner>(&mut self, mut ptr: NonNull<I>) {
            unsafe {
                debug_assert_eq!(ptr.as_ref().ref_count(), 0);
                debug_assert_eq!(ptr.as_ref().color(), Color::White);

                // For each child that can be traced from `ptr`
                for next in ptr.as_mut().iter_children_mut() {
                    match next {
                        GcTraceMutRef::Array(array) => {
                            if let Some(array) = self._check_suspicious(array) {
                                self.array.push(array.ptr());
                            }
                        }
                        GcTraceMutRef::Table(table) => {
                            if let Some(table) = self._check_suspicious(table) {
                                self.table.push(table.ptr());
                            }
                        }
                        GcTraceMutRef::Function(function) => {
                            if let Some(function) = self._check_suspicious(function) {
                                self.function.push(function.ptr());
                            }
                        }
                        GcTraceMutRef::Capture(capture) => {
                            if let Some(capture) = self._check_suspicious(capture) {
                                self.capture.push(capture.ptr());
                            }
                        }
                    }
                }
            }
        }
        /// This function must be called only from `collect()`.
        unsafe fn _check_suspicious<'a, U: GcObject>(
            &mut self,
            item: &'a mut U,
        ) -> Option<&'a mut U> {
            if cfg!(debug_assertions) {
                let color = item.inner().color();
                assert!(
                    color == Color::Black || color == Color::Purple,
                    "Expected black or purple, but got {color:?}",
                );
            }

            // From the prerequisites (callee position) of this function, the parent object of `item` is `ref_count() == 0`.
            item.inner().dec_ref_count();

            if item.inner().ref_count() == 0 {
                item.inner().paint(Color::White);
                self.collect(item.ptr());
                unsafe { deallocate_inner(item) };
                None
            } else {
                // To avoid double collection, we need to check whether its color is purple.
                if item.inner().color() == Color::Purple {
                    return None;
                }
                item.inner().paint(Color::Purple);
                Some(item)
            }
        }
        #[allow(clippy::type_complexity)]
        fn finish(
            self,
        ) -> (
            Vec<NonNull<LArrayInner>>,
            Vec<NonNull<LTableInner>>,
            Vec<NonNull<LFunctionInner>>,
            Vec<NonNull<LFCaptureInner>>,
        ) {
            let Self { mut array, mut table, mut function, mut capture } = self;
            array.retain(|x| unsafe { x.as_ref() }.color() == Color::Purple);
            table.retain(|x| unsafe { x.as_ref() }.color() == Color::Purple);
            function.retain(|x| unsafe { x.as_ref() }.color() == Color::Purple);
            capture.retain(|x| unsafe { x.as_ref() }.color() == Color::Purple);
            (array, table, function, capture)
        }
    }

    // Collect purple objects and apply `mark_and_sweep::run()` for them.
    let mut purple_collector = PurpleCollector::default();
    purple_collector.collect(this.ptr());
    let purple_ptrs = purple_collector.finish();

    unsafe { deallocate_inner(this) };

    for array_ptr in purple_ptrs.0 {
        mark_and_sweep::run_with_inner_ptr::<LArray>(array_ptr);
    }
    for table_ptr in purple_ptrs.1 {
        mark_and_sweep::run_with_inner_ptr::<LTable>(table_ptr);
    }
    for function_ptr in purple_ptrs.2 {
        mark_and_sweep::run_with_inner_ptr::<LFunction>(function_ptr);
    }
    for capture_ptr in purple_ptrs.3 {
        mark_and_sweep::run_with_inner_ptr::<LFCapture>(capture_ptr);
    }

    RecursiveDropGuard::end_drop();
}

unsafe fn deallocate_inner<T: GcObject>(this: &mut T) {
    debug_assert_ne!(this.ptr(), NonNull::dangling());
    debug_assert_eq!(this.inner().ref_count(), 0);

    unsafe {
        let ptr = if cfg!(debug_assertions) {
            mem::replace(&mut this.ptr(), NonNull::dangling())
        } else {
            this.ptr()
        };
        let mut inner = ptr::read(ptr.as_ptr());
        inner.release_children();
        drop(inner);
        dealloc(ptr.as_ptr().cast(), Layout::for_value(ptr.as_ref()));
    }
}

mod mark_and_sweep {
    use super::*;

    pub(super) fn run_with_inner_ptr<T: GcObject>(ptr: NonNull<T::Inner>) {
        let mut object = T::from_inner_ptr(ptr);
        run(&mut object);
        mem::forget(object);
    }

    pub(super) fn run<T: GcObject>(item: &mut T) {
        if item.inner().color() != Color::Purple {
            return;
        }
        paint_gray(item);
        scan_gray(item);
        collect_white(item);
    }

    /// Tentatively removing. (試験削除)
    /// Paint the object (`item`) gray and decrement the reference count recursively.
    fn paint_gray<T: GcObject>(item: &mut T) {
        // Infinite recursion prevention.
        // If `item` is gray, the object that can be traced from `item` has already been tentatively removed, so nothing needs to be done.
        if item.inner().color() == Color::Gray {
            return;
        }
        item.inner().paint(Color::Gray);

        for next in unsafe { item.inner_mut() }.iter_children_mut() {
            match next {
                GcTraceMutRef::Array(array) => {
                    array.inner().dec_ref_count();
                    paint_gray(array);
                }
                GcTraceMutRef::Table(table) => {
                    table.inner().dec_ref_count();
                    paint_gray(table);
                }
                GcTraceMutRef::Function(function) => {
                    function.inner().dec_ref_count();
                    paint_gray(function);
                }
                GcTraceMutRef::Capture(capture) => {
                    capture.inner().dec_ref_count();
                    paint_gray(capture);
                }
            }
        }
    }

    /// Mark white if the object (`item`) is gray and its reference count is zero, and recursively apply `scan_gray()` to its children.
    /// The object that is marked gray but its reference count is not zero is passed to `paint_black()`.
    fn scan_gray<T: GcObject>(item: &mut T) {
        // We want to scan only gray objects.
        if item.inner().color() != Color::Gray {
            return;
        }

        if item.inner().ref_count() == 0 {
            // To prevent infinite recursion, we need to paint the object white before calling `scan_gray()` for its children.
            item.inner().paint(Color::White);

            for next in unsafe { item.inner_mut() }.iter_children_mut() {
                match next {
                    GcTraceMutRef::Array(array) => scan_gray(array),
                    GcTraceMutRef::Table(table) => scan_gray(table),
                    GcTraceMutRef::Function(function) => scan_gray(function),
                    GcTraceMutRef::Capture(capture) => scan_gray(capture),
                }
            }
        } else {
            // We can't remove this object (`item`) as its reference count is not zero.
            // Repaint `item` and its children black.
            paint_black(item);
        }
    }

    /// Paint the object (`item`) black and recursively paint its children black.
    fn paint_black<T: GcObject>(item: &mut T) {
        // Infinite recursion prevention.
        // - All PmsObject are initially black.
        // - From the implementations of `gc::custom_drop()`, the children of black objects are also black.
        // So do nothing if `item` is already black.
        if item.inner().color() == Color::Black {
            return;
        }
        item.inner().paint(Color::Black);

        for next in unsafe { item.inner_mut() }.iter_children_mut() {
            // In `paint_gray()`, we decremented the reference count for tentatively removing.
            // So we need to increment the reference count here.
            match next {
                GcTraceMutRef::Array(array) => {
                    array.inner().inc_ref_count();
                    paint_black(array);
                }
                GcTraceMutRef::Table(table) => {
                    table.inner().inc_ref_count();
                    paint_black(table);
                }
                GcTraceMutRef::Function(function) => {
                    function.inner().inc_ref_count();
                    paint_black(function);
                }
                GcTraceMutRef::Capture(capture) => {
                    capture.inner().inc_ref_count();
                    paint_black(capture);
                }
            }
        }
    }

    fn collect_white<T: GcObject>(item: &mut T) {
        // We want to collect only white objects.
        if item.inner().color() != Color::White {
            return;
        }

        // TODO: なぜ黒に塗るのか説明する。
        //       黒: OK、灰: 良さそうだけど、どう？、白: 無限再帰起こすからだめ、紫: mark_and_sweep::run() の繰り返し呼び出しで死ぬ
        item.inner().paint(Color::Black);
        for next in unsafe { item.inner_mut() }.iter_children_mut() {
            match next {
                GcTraceMutRef::Array(array) => collect_white(array),
                GcTraceMutRef::Table(table) => collect_white(table),
                GcTraceMutRef::Function(function) => collect_white(function),
                GcTraceMutRef::Capture(capture) => collect_white(capture),
            }
        }
        unsafe { deallocate_inner(item) };
    }
}
