use lean_string::LeanString;
use std::{cell::Cell, fmt, hint, ops::Deref, ptr::NonNull, slice};

mod opcode;
pub use opcode::Opcode;

mod operand;
pub use operand::Operand;

mod instr;
pub use instr::Instr;

// NOTE:
// We can't implment Send and Sync for an ExecUnit because the ExecUnit not only use Cell<usize>
// for reference counting, but also provides `code_mut` method which has internal mutability.
// If we need to send to another thread, we may add another struct and methods.
pub struct ExecUnit {
    data_ptr: NonNull<LeanString>,
    data_len: usize,
    code_ptr: NonNull<Instr>,
    code_len: usize,
    ref_count: RefCountPtr,
}

fn _static_assert_not_impl_send_sync() {
    // This static assert is from the `assert_not_impl_all` macro in the `static_assertions` crate.
    trait AmbiguousIfImpl<A> {
        fn check() {}
    }
    impl<T> AmbiguousIfImpl<()> for T {}
    impl<T: Send> AmbiguousIfImpl<u8> for T {}
    impl<T: Sync> AmbiguousIfImpl<i8> for T {}
    // If the Rust compiler infers and determine the type of `A` to be one of `()`, `u8` or `i8`,
    // it means that it means that `ExecUnit` is neither `Send` nor `Sync`.
    // If it cannot determine one, a compilation error occurs, in which case `ExecUnit` is an
    // `ExecUnit` implements `Send` or `Sync`.
    let _ = <ExecUnit as AmbiguousIfImpl<_>>::check;
}

#[derive(Clone, Copy)]
struct RefCountPtr(NonNull<Cell<usize>>);

impl RefCountPtr {
    fn new(value: usize) -> Self {
        RefCountPtr(NonNull::from(Box::leak(Box::new(Cell::new(value)))))
    }
}

impl Deref for RefCountPtr {
    type Target = Cell<usize>;

    fn deref(&self) -> &Self::Target {
        // SAFETY: The pointer is always valid because it is created by Box::leak.
        unsafe { self.0.as_ref() }
    }
}

impl ExecUnit {
    pub fn new(data: Box<[LeanString]>, code: Box<[Instr]>) -> Self {
        fn into_parts<T>(ba: Box<[T]>) -> (NonNull<T>, usize) {
            let len = ba.len();
            let ptr = Box::into_raw(ba) as *mut T;
            // SAFETY: Box::into_raw returns a non-null pointer.
            unsafe { (NonNull::new_unchecked(ptr), len) }
        }
        let (data_ptr, data_len) = into_parts(data);
        let (code_ptr, code_len) = into_parts(code);
        ExecUnit { data_ptr, data_len, code_ptr, code_len, ref_count: RefCountPtr::new(1) }
    }

    pub fn data(&self) -> &[LeanString] {
        unsafe { slice::from_raw_parts(self.data_ptr.as_ptr(), self.data_len) }
    }

    pub fn code(&self) -> &[Instr] {
        unsafe { slice::from_raw_parts(self.code_ptr.as_ptr(), self.code_len) }
    }

    /// # Safety
    ///
    /// This method brings internal mutablility to [`ExecUnit`], but without the runtime checks
    /// that `RefCell` does.
    ///
    /// You should use this method only for inline-cache or other changes that not change the
    /// meaning of the code.
    pub unsafe fn code_mut(&mut self) -> &mut [Instr] {
        unsafe { slice::from_raw_parts_mut(self.code_ptr.as_ptr(), self.code_len) }
    }
}

impl Clone for ExecUnit {
    fn clone(&self) -> Self {
        let count = self.ref_count.get();

        // Insert an assume here to hint LLVM to optimize
        // SAFETY: The reference count will never be zero when this is called.
        unsafe {
            hint::assert_unchecked(count != 0);
        }

        let count = count.wrapping_add(1);
        self.ref_count.set(count);

        // We want to abort on overflow instead of dropping the value.
        if count == 0 {
            std::process::abort();
        }

        Self {
            data_ptr: self.data_ptr,
            data_len: self.data_len,
            code_ptr: self.code_ptr,
            code_len: self.code_len,
            ref_count: self.ref_count,
        }
    }
}

impl Drop for ExecUnit {
    fn drop(&mut self) {
        self.ref_count.update(|count| count - 1);
        if self.ref_count.get() == 0 {
            // SAFETY:
            // - The reference count is zero, so no other reference exists.
            // - The data and code pointers are non-null and valid for the its length.
            unsafe {
                drop(Box::from_raw(slice::from_raw_parts_mut(
                    self.data_ptr.as_ptr(),
                    self.data_len,
                )));
                drop(Box::from_raw(slice::from_raw_parts_mut(
                    self.code_ptr.as_ptr(),
                    self.code_len,
                )));
                drop(Box::from_raw(self.ref_count.0.as_ptr()));
            }
        }
    }
}

impl fmt::Debug for ExecUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExecUnit").field("data", &self.data()).field("code", &self.code()).finish()
    }
}

impl PartialEq for ExecUnit {
    fn eq(&self, other: &Self) -> bool {
        self.data() == other.data() && self.code() == other.code()
    }
}
