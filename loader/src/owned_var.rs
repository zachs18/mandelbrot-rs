use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::Arc,
};

use crate::{AsPtr, AssertShared, AssertUnique, Leak, Library};

#[derive(Debug, Clone)]
pub struct OwnedLibraryVar<T: ?Sized> {
    pub(crate) ptr: NonNull<T>,
    pub(crate) _library: Arc<Library>,
}

impl<T: ?Sized> AsPtr for OwnedLibraryVar<T> {
    type Pointer = *mut T;
    unsafe fn as_ptr(&self) -> Self::Pointer {
        self.ptr.as_ptr()
    }
}

impl<F> OwnedLibraryVar<F> {
    /// Asserts that this is the only pointer to its symbol,
    /// and that this symbol points to the correct type.
    pub unsafe fn assert_unique(self) -> AssertUnique<Self> {
        AssertUnique { inner: self }
    }
    /// Asserts that there are no unique pointers to this symbol,
    /// and that this symbol points to the correct type.
    pub unsafe fn assert_shared(self) -> AssertShared<Self> {
        AssertShared { inner: self }
    }
    /// A reference to the library this symbol is from.
    pub fn library(&self) -> &Arc<Library> {
        &self._library
    }
}

impl<T: ?Sized> Deref for AssertShared<OwnedLibraryVar<T>> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.inner.ptr.as_ptr() }
    }
}

impl<T: ?Sized> Deref for AssertUnique<OwnedLibraryVar<T>> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.inner.ptr.as_ptr() }
    }
}

impl<T: ?Sized> DerefMut for AssertUnique<OwnedLibraryVar<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.inner.ptr.as_ptr() }
    }
}

impl<T: ?Sized> Leak for OwnedLibraryVar<T> {
    type Result = crate::ref_var::LibraryVar<'static, T>;

    fn leak(self) -> Self::Result {
        let OwnedLibraryVar { ptr, _library } = self;
        let _library = _library.leak();
        crate::ref_var::LibraryVar { ptr, _library }
    }
}

#[cfg(feature = "unsize")]
mod unsize {
    use std::{marker::Unsize, ops::CoerceUnsized};

    use super::OwnedLibraryVar;

    impl<T: ?Sized, U: ?Sized + Unsize<T>> CoerceUnsized<OwnedLibraryVar<T>> for OwnedLibraryVar<U> {}
}
