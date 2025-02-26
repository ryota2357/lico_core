use super::*;
use crate::il::ExecUnit;
use std::{
    alloc::{Layout, dealloc},
    cell::Cell,
    fmt, iter,
    mem::{forget, replace},
    ptr::NonNull,
};

pub struct LFunction {
    ptr: NonNull<LFunctionInner>,
}

pub(super) struct LFunctionInner {
    start_pc: usize,
    exec_unit: ExecUnit,
    captures: Box<[LFCapture]>,
    ref_count: Cell<usize>,
    color: Cell<Color>,
}

gc_trait_impls! {
    impl GcObject, Clone, Drop for LFunction;
    impl GcObjectInner for LFunctionInner {
        fn iter_children_mut(&mut self) -> impl Iterator<Item = GcTraceMutRef<'_>> {
            self.captures.iter_mut().map(GcTraceMutRef::Capture)
        }
        fn release_children(&mut self) {
            let captures = replace(&mut self.captures, Box::new([]));
            let ptr = Box::into_raw(captures);
            unsafe {
                dealloc(ptr.cast(), Layout::for_value(&*ptr));
            }
        }
    }
}

impl LFunction {
    pub fn new(start_pc: usize, exec_unit: ExecUnit, captures: Box<[LFCapture]>) -> Self {
        let ptr = Box::leak(Box::new(LFunctionInner {
            start_pc,
            exec_unit,
            captures,
            ref_count: Cell::new(1),
            color: Cell::new(Color::Black),
        }));
        LFunction { ptr: NonNull::from(ptr) }
    }

    pub fn start_pc(&self) -> usize {
        self.inner().start_pc
    }

    pub fn exec_unit(&self) -> &ExecUnit {
        &self.inner().exec_unit
    }

    pub fn captures(&self) -> &[LFCapture] {
        &self.inner().captures
    }
}

impl fmt::Debug for LFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LFunction")
            .field("start_pc", &self.start_pc())
            .field("exec_unit", &self.exec_unit())
            .field("captures", &self.captures())
            .finish()
    }
}

impl PartialEq for LFunction {
    fn eq(&self, other: &Self) -> bool {
        self.start_pc() == other.start_pc()
            && self.exec_unit() == other.exec_unit()
            && self.captures() == other.captures()
    }
}

pub struct LFCapture {
    ptr: NonNull<LFCaptureInner>,
}

pub(super) struct LFCaptureInner {
    // data: ManuallyDrop<Object>,
    data: Object,
    ref_count: Cell<usize>,
    color: Cell<Color>,
}

gc_trait_impls! {
    impl GcObject, Clone, Drop for LFCapture;
    impl GcObjectInner for LFCaptureInner {
        fn iter_children_mut(&mut self) -> impl Iterator<Item = GcTraceMutRef<'_>> {
            iter::once(&mut self.data).filter_map(|x| match x {
                Object::Array(array) => Some(GcTraceMutRef::Array(array)),
                Object::Table(table) => Some(GcTraceMutRef::Table(table)),
                Object::Function(function) => Some(GcTraceMutRef::Function(function)),
                _ => None,
            })
        }
        fn release_children(&mut self) {
            let prev = replace(&mut self.data, Object::Nil);
            match prev {
                Object::Function(func) => forget(func),
                Object::Array(array) => forget(array),
                Object::Table(table) => forget(table),
                _ => {}
            }
        }
    }
}

impl LFCapture {
    pub fn new(data: Object) -> Self {
        let ptr = Box::leak(Box::new(LFCaptureInner {
            data,
            ref_count: Cell::new(1),
            color: Cell::new(Color::Black),
        }));
        LFCapture { ptr: NonNull::from(ptr) }
    }

    pub fn get(&self) -> &Object {
        &self.inner().data
    }

    pub fn set(&mut self, data: Object) {
        unsafe {
            self.inner_mut().data = data;
        }
    }
}

impl fmt::Debug for LFCapture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LCapture").field("data", &self.get()).finish()
    }
}

impl PartialEq for LFCapture {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}
