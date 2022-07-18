use std::{ptr::NonNull, ops::{Deref, DerefMut}};

use crate::{Library, AsPtr, AssertUnique, AssertShared};

#[derive(Debug, Clone, Copy)]
pub struct LibraryVar<'a, T: ?Sized> {
    pub(crate) ptr: NonNull<T>,
    pub(crate) _library: &'a Library,
}

impl<'a, T: ?Sized> AsPtr for LibraryVar<'a, T> {
    type Pointer = *mut T;
    fn as_ptr(&self) -> Self::Pointer {
        self.ptr.as_ptr()
    }
}

impl<'a, F> LibraryVar<'a, F> {
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
    pub fn library(&self) -> &'a Library {
        self._library
    }
}

impl<'a, T: ?Sized> Deref for AssertShared<LibraryVar<'a, T>> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            &*self.inner.ptr.as_ptr()
        }
    }
}


impl<'a, T: ?Sized> Deref for AssertUnique<LibraryVar<'a, T>> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe {
            &*self.inner.ptr.as_ptr()
        }
    }
}

impl<'a, T: ?Sized> DerefMut for AssertUnique<LibraryVar<'a, T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            &mut *self.inner.ptr.as_ptr()
        }
    }
}

#[cfg(feature = "unsize")]
mod unsize {
    use std::{marker::Unsize, ops::CoerceUnsized};

    use super::LibraryVar;

    impl<'a, T: ?Sized, U: ?Sized + Unsize<T>> CoerceUnsized<LibraryVar<'a, T>> for LibraryVar<'a, U> {
    }
}
