use std::sync::Arc;

use crate::{AsPtr, AssertShared, AssertUnique, Leak, Library};

#[derive(Debug, Clone)]
pub struct OwnedLibraryFunc<F> {
    pub(crate) ptr: F,
    pub(crate) _library: Arc<Library>,
}

impl<F: Copy> AsPtr for OwnedLibraryFunc<F> {
    type Pointer = F;
    unsafe fn as_ptr(&self) -> Self::Pointer {
        self.ptr
    }
}

impl<F> OwnedLibraryFunc<F> {
    /// Asserts that this is safely callable with the "C" calling convention,
    /// and that this symbol points to the correct type, and
    /// that it is the only pointer to its symbol.
    /// The function may be non-reentrant.
    pub unsafe fn assert_unique(self) -> AssertUnique<Self> {
        AssertUnique { inner: self }
    }
    /// Asserts that this is safely callable with the "C" calling convention,
    /// and that this symbol points to the correct type, and
    /// that there are no unique pointers to this symbol.
    /// The function must be reentrant.
    pub unsafe fn assert_shared(self) -> AssertShared<Self> {
        AssertShared { inner: self }
    }
    /// A reference to the library this symbol is from.
    pub fn library(&self) -> &Arc<Library> {
        &self._library
    }
}

#[cfg(feature = "fn_traits")]
macro_rules! make_fn_impls {
    ($($var:ident: $ty:ident),*) => {
        impl<$($ty ,)* R> FnOnce<($($ty,)*)> for AssertShared<OwnedLibraryFunc<unsafe extern "C" fn($($ty,)*) -> R>> {
            type Output = R;
            extern "rust-call" fn call_once(self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                unsafe { f($($var,)*) }
            }
        }
        impl<$($ty ,)* R> FnMut<($($ty,)*)> for AssertShared<OwnedLibraryFunc<unsafe extern "C" fn($($ty,)*) -> R>> {
            extern "rust-call" fn call_mut(&mut self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                unsafe { f($($var,)*) }
            }
        }
        impl<$($ty ,)* R> Fn<($($ty,)*)> for AssertShared<OwnedLibraryFunc<unsafe extern "C" fn($($ty,)*) -> R>> {
            extern "rust-call" fn call(&self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                unsafe { f($($var,)*) }
            }
        }

        impl<$($ty ,)* R> FnOnce<($($ty,)*)> for AssertUnique<OwnedLibraryFunc<unsafe extern "C" fn($($ty,)*) -> R>> {
            type Output = R;
            extern "rust-call" fn call_once(self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                unsafe { f($($var,)*) }
            }
        }
        impl<$($ty ,)* R> FnMut<($($ty,)*)> for AssertUnique<OwnedLibraryFunc<unsafe extern "C" fn($($ty,)*) -> R>> {
            extern "rust-call" fn call_mut(&mut self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                unsafe { f($($var,)*) }
            }
        }


        impl<$($ty ,)* R> FnOnce<($($ty,)*)> for AssertShared<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
            type Output = R;
            extern "rust-call" fn call_once(self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                { f($($var,)*) }
            }
        }
        impl<$($ty ,)* R> FnMut<($($ty,)*)> for AssertShared<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
            extern "rust-call" fn call_mut(&mut self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                { f($($var,)*) }
            }
        }
        impl<$($ty ,)* R> Fn<($($ty,)*)> for AssertShared<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
            extern "rust-call" fn call(&self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                { f($($var,)*) }
            }
        }

        impl<$($ty ,)* R> FnOnce<($($ty,)*)> for AssertUnique<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
            type Output = R;
            extern "rust-call" fn call_once(self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                { f($($var,)*) }
            }
        }
        impl<$($ty ,)* R> FnMut<($($ty,)*)> for AssertUnique<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
            extern "rust-call" fn call_mut(&mut self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                // SAFETY: self's lifetime will not end before the called function completes.
                let f = unsafe { self.as_ptr() };
                { f($($var,)*) }
            }
        }
    };
}

#[cfg(not(feature = "fn_traits"))]
macro_rules! make_fn_impls {
    ($($_:tt)*) => {};
}

make_fn_impls!();
make_fn_impls!(a: A);
make_fn_impls!(a: A, b: B);
make_fn_impls!(a: A, b: B, c: C);
make_fn_impls!(a: A, b: B, c: C, d: D);
make_fn_impls!(a: A, b: B, c: C, d: D, e: E);
make_fn_impls!(a: A, b: B, c: C, d: D, e: E, f: F);
make_fn_impls!(a: A, b: B, c: C, d: D, e: E, f: F, g: G);
make_fn_impls!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H);
make_fn_impls!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I);
make_fn_impls!(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J);

impl<F: Copy> Leak for OwnedLibraryFunc<F> {
    type Result = crate::ref_func::LibraryFunc<'static, F>;

    fn leak(self) -> Self::Result {
        let OwnedLibraryFunc { ptr, _library } = self;
        let _library = _library.leak();
        crate::ref_func::LibraryFunc { ptr, _library }
    }
}
