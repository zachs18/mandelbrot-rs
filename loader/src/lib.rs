#![cfg_attr(feature = "unsize", feature(unsize))]
#![cfg_attr(feature = "unsize", feature(coerce_unsized))]
#![cfg_attr(feature = "fn_traits", feature(unboxed_closures))]
#![cfg_attr(feature = "fn_traits", feature(fn_traits))]
#![cfg_attr(feature = "fn_ptr_trait", feature(fn_ptr_trait))]
#![deny(unsafe_op_in_unsafe_fn)]

#[cfg(not(unix))]
compile_error!("Not supported on non-unix platforms");

#[cfg(feature = "fn_ptr_trait")]
use std::marker::FnPtr;

#[cfg(not(feature = "fn_ptr_trait"))]
mod fn_ptr_trait;
#[cfg(not(feature = "fn_ptr_trait"))]
use fn_ptr_trait::FnPtr;

pub mod owned_func;
pub mod owned_var;
pub mod ref_func;
pub mod ref_var;

use std::{
    ffi::{CStr, CString},
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::Arc,
};

use owned_func::OwnedLibraryFunc;
use owned_var::OwnedLibraryVar;
use ref_func::LibraryFunc;
use ref_var::LibraryVar;

#[macro_export]
macro_rules! c_str {
    ($s:literal) => {{
        static S: &'static str = concat!($s, "\0");
        ::std::ffi::CStr::from_bytes_with_nul(S.as_bytes()).unwrap()
    }};
}

struct WithDlerrorResult<R> {
    #[allow(unused)]
    previous_error: Option<CString>,
    value: R,
    error: Option<CString>,
}

unsafe fn get_dlerror_unlocked() -> Option<CString> {
    let error = unsafe { libc::dlerror() };
    if !error.is_null() {
        Some(unsafe { CStr::from_ptr(error) }.to_owned())
    } else {
        None
    }
}

/// The passed closure should invoke at most one `dlopen`-API function at most once.
unsafe fn with_dlerror<R>(f: impl FnOnce() -> R) -> WithDlerrorResult<R> {
    let _guard = {
        // dlerror is not required by POSIX-1.2017 to be thread-safe, so
        // employ a static mutex to ensure this library only calls it from one thread at a time.
        // NOTE: Other code making dlerror calls (not from this crate) will race with this.
        #[cfg(all(unix, not(any(target_os = "linux", target_os = "macos"))))]
        {
            static DLERROR_LOCK: Mutex<()> = Mutex::new(());
            DLERROR_LOCK.lock().expect("failed to lock dlerror mutex")
        }
        // dlerror is thread-safe on Linux and macOs,
        // but re-uses the buffer between calls on the same thread.
    };
    // Both to ensure `error` is updated, and to perhaps debug-check that no previous error occurred;
    let previous_error = unsafe { get_dlerror_unlocked() };
    let value = f();
    let error = unsafe { get_dlerror_unlocked() };
    WithDlerrorResult {
        previous_error,
        value,
        error,
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum LibraryError {
    Dlerror(CString),
    NullSymbol,
}

impl std::fmt::Display for LibraryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LibraryError::Dlerror(msg) => write!(f, "dlerror: {msg:?}"),
            LibraryError::NullSymbol => write!(f, "null symbol"),
        }
    }
}

impl std::error::Error for LibraryError {}

#[derive(Debug)]
pub struct Library {
    handle: NonNull<libc::c_void>,
}

impl Drop for Library {
    fn drop(&mut self) {
        let _result = unsafe { with_dlerror(|| libc::dlclose(self.handle.as_ptr())) };
        #[cfg(debug_assertions)]
        assert_eq!(_result.value, 0, "dlclose failed: {:?}", _result.error);
    }
}

impl Library {
    pub fn this_program() -> Result<Self, CString> {
        Library::dlopen_impl(None, true)
    }

    pub fn new(filename: &CStr, bind_lazy: bool) -> Result<Self, CString> {
        Library::dlopen_impl(Some(filename), bind_lazy)
    }

    fn dlopen_impl(filename: Option<&CStr>, bind_lazy: bool) -> Result<Self, CString> {
        let flags = if bind_lazy {
            libc::RTLD_LAZY
        } else {
            libc::RTLD_NOW
        };
        let filename = filename.map(CStr::as_ptr).unwrap_or(std::ptr::null());
        let handle = unsafe { with_dlerror(|| libc::dlopen(filename, flags)) };
        if let Some(handle) = NonNull::new(handle.value) {
            Ok(Library { handle })
        } else {
            Err(handle.error.unwrap())
        }
    }

    /// Returns Ok(None) if `symbol`'s value is NULL. Returns `Err(msg)` if dlsym failed with error `msg`.
    pub fn sym_var<'a, T>(&'a self, symbol: &CStr) -> Result<LibraryVar<'a, T>, LibraryError> {
        let symbol = unsafe { with_dlerror(|| libc::dlsym(self.handle.as_ptr(), symbol.as_ptr())) };
        if let Some(error) = symbol.error {
            Err(LibraryError::Dlerror(error))
        } else if let Some(symbol) = NonNull::new(symbol.value) {
            Ok(LibraryVar {
                ptr: symbol.cast(),
                _library: self,
            })
        } else {
            Err(LibraryError::NullSymbol)
        }
    }

    /// F must be a function pointer type
    ///
    /// Returns Ok(None) if `symbol`'s value is NULL. Returns `Err(msg)` if dlsym failed with error `msg`.
    pub fn sym_func<'a, F: FnPtr>(
        &'a self,
        symbol: &CStr,
    ) -> Result<LibraryFunc<'a, F>, LibraryError> {
        unsafe { self.sym_func_unchecked(symbol) }
    }

    /// F must be a function pointer type
    ///
    /// Returns Ok(None) if `symbol`'s value is NULL. Returns `Err(msg)` if dlsym failed with error `msg`.
    pub unsafe fn sym_func_unchecked<'a, F>(
        &'a self,
        symbol: &CStr,
    ) -> Result<LibraryFunc<'a, F>, LibraryError> {
        let symbol = unsafe { with_dlerror(|| libc::dlsym(self.handle.as_ptr(), symbol.as_ptr())) };
        if let Some(error) = symbol.error {
            Err(LibraryError::Dlerror(error))
        } else if let Some(symbol) = NonNull::new(symbol.value) {
            // Cannot use transmute because F is not statically guaranteed to be sizeof(pointer)
            // let ptr: F = unsafe { std::mem::transmute(symbol.as_ptr()) };
            assert_eq!(
                std::mem::size_of::<F>(),
                std::mem::size_of::<*mut libc::c_void>(),
                "Only function pointers can be used with LibraryFunc."
            );
            let ptr: F = unsafe { std::mem::transmute_copy(&symbol) };
            Ok(LibraryFunc {
                ptr,
                _library: self,
            })
        } else {
            Err(LibraryError::NullSymbol)
        }
    }

    /// Returns Ok(None) if `symbol`'s value is NULL. Returns `Err(msg)` if dlsym failed with error `msg`.
    pub fn sym_var_owned<T>(
        self: &Arc<Self>,
        symbol: &CStr,
    ) -> Result<OwnedLibraryVar<T>, LibraryError> {
        let LibraryVar { ptr, .. } = self.sym_var::<T>(symbol)?;
        Ok(OwnedLibraryVar {
            ptr,
            _library: self.clone(),
        })
    }

    /// F must be a function pointer type
    ///
    /// Returns Ok(None) if `symbol`'s value is NULL. Returns `Err(msg)` if dlsym failed with error `msg`.
    pub fn sym_func_owned<F: FnPtr>(
        self: &Arc<Self>,
        symbol: &CStr,
    ) -> Result<OwnedLibraryFunc<F>, LibraryError> {
        let LibraryFunc { ptr, .. } = self.sym_func::<F>(symbol)?;
        Ok(OwnedLibraryFunc {
            ptr,
            _library: self.clone(),
        })
    }

    /// F must be a function pointer type.
    ///
    /// Returns Ok(None) if `symbol`'s value is NULL. Returns `Err(msg)` if dlsym failed with error `msg`.
    pub unsafe fn sym_func_owned_unchecked<F>(
        self: &Arc<Self>,
        symbol: &CStr,
    ) -> Result<OwnedLibraryFunc<F>, LibraryError> {
        let LibraryFunc { ptr, .. } = unsafe { self.sym_func_unchecked::<F>(symbol) }?;
        Ok(OwnedLibraryFunc {
            ptr,
            _library: self.clone(),
        })
    }
}

pub trait AsPtr {
    type Pointer;
    /// This pointer may point to data which is tied to `self`'s lifetime. However, this pointer may be a (safe) `fn` pointer, which is callable without any `unsafe`. Thus this function is unsafe.
    unsafe fn as_ptr(&self) -> Self::Pointer;
}

pub trait Leak {
    type Result;
    fn leak(self) -> Self::Result;
}

/// Asserts that this is the only pointer to its symbol.
// Intentionally does not implement clone or copy.
#[derive(Debug)]
pub struct AssertUnique<T: ?Sized> {
    inner: T,
}

/// Asserts that there are no unique pointers to this symbol.
#[derive(Debug, Clone, Copy)]
pub struct AssertShared<T: ?Sized> {
    inner: T,
}

/// Asserts that this symbol is Send and Sync.
#[derive(Debug, Clone, Copy)]
pub struct AssertSendSync<T: ?Sized> {
    inner: T,
}

unsafe impl<T: ?Sized> Send for AssertSendSync<T> {}
unsafe impl<T: ?Sized> Sync for AssertSendSync<T> {}

impl<T: AsPtr + ?Sized> AsPtr for AssertShared<T> {
    type Pointer = T::Pointer;
    unsafe fn as_ptr(&self) -> Self::Pointer {
        unsafe { self.inner.as_ptr() }
    }
}

impl<T: AsPtr + ?Sized> AsPtr for AssertUnique<T> {
    type Pointer = T::Pointer;
    unsafe fn as_ptr(&self) -> Self::Pointer {
        unsafe { self.inner.as_ptr() }
    }
}

impl<T: AsPtr + ?Sized> AsPtr for AssertSendSync<T> {
    type Pointer = T::Pointer;
    unsafe fn as_ptr(&self) -> Self::Pointer {
        unsafe { self.inner.as_ptr() }
    }
}

impl<T: Leak> Leak for AssertShared<T> {
    type Result = AssertShared<T::Result>;
    fn leak(self) -> Self::Result {
        AssertShared {
            inner: self.inner.leak(),
        }
    }
}

impl<T: Leak> Leak for AssertUnique<T> {
    type Result = AssertUnique<T::Result>;
    fn leak(self) -> Self::Result {
        AssertUnique {
            inner: self.inner.leak(),
        }
    }
}

impl<T: Leak> Leak for AssertSendSync<T> {
    type Result = AssertSendSync<T::Result>;
    fn leak(self) -> Self::Result {
        AssertSendSync {
            inner: self.inner.leak(),
        }
    }
}

impl<T: ?Sized + 'static> Leak for Arc<T> {
    type Result = &'static T;

    fn leak(self) -> Self::Result {
        let raw = Arc::into_raw(self);
        unsafe { &*raw }
    }
}

impl<T: Deref> Deref for AssertSendSync<T> {
    type Target = T::Target;

    fn deref(&self) -> &Self::Target {
        self.inner.deref()
    }
}

impl<T: DerefMut> DerefMut for AssertSendSync<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner.deref_mut()
    }
}

pub trait AssertSendSyncExt: Sized {
    /// Asserts that this variable/function can be concurrently accessed from
    /// different threads, and sent between threads.
    unsafe fn assert_send_sync(self) -> AssertSendSync<Self> {
        AssertSendSync { inner: self }
    }
}

impl<T> AssertSendSyncExt for T {}

#[cfg(feature = "unsize")]
mod unsize {
    use crate::{AssertSendSync, AssertShared, AssertUnique};

    use std::ops::CoerceUnsized;

    impl<T: ?Sized, U: ?Sized + CoerceUnsized<T>> CoerceUnsized<AssertShared<T>> for AssertShared<U> {}
    impl<T: ?Sized, U: ?Sized + CoerceUnsized<T>> CoerceUnsized<AssertUnique<T>> for AssertUnique<U> {}
    impl<T: ?Sized, U: ?Sized + CoerceUnsized<T>> CoerceUnsized<AssertSendSync<T>>
        for AssertSendSync<U>
    {
    }
}

#[cfg(feature = "fn_traits")]
mod fn_traits {
    use crate::AssertSendSync;

    macro_rules! make_fn_impls {
        ($($var:ident: $ty:ident),*) => {
            impl<Func, $($ty ,)* R> FnOnce<($($ty,)*)> for AssertSendSync<Func>
                where Func: FnOnce($($ty,)*) -> R
            {
                type Output = R;
                extern "rust-call" fn call_once(self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                    (self.inner)($($var,)*)
                }
            }
            impl<Func, $($ty ,)* R> FnMut<($($ty,)*)> for AssertSendSync<Func>
                where Func: FnMut($($ty,)*) -> R
            {
                extern "rust-call" fn call_mut(&mut self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                    (self.inner)($($var,)*)
                }
            }
            impl<Func, $($ty ,)* R> Fn<($($ty,)*)> for AssertSendSync<Func>
                where Func: Fn($($ty,)*) -> R
            {
                extern "rust-call" fn call(&self, ($($var,)*): ($($ty,)*)) -> Self::Output {
                    (self.inner)($($var,)*)
                }
            }
        }
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
}

#[cfg(test)]
pub mod tests {
    use std::sync::atomic::{AtomicUsize, Ordering};

    use super::*;

    #[test]
    fn sin() {
        let libm = Library::new(c_str!("libm.so.6"), false).unwrap();
        let sin = libm
            .sym_func::<extern "C" fn(f64) -> f64>(c_str!("sin"))
            .expect("symbol is present and not null");
        let sin = unsafe { sin.assert_callable_shared() };
        #[cfg(feature = "fn_traits")]
        assert_eq!(0.0, sin(0.0));
        #[cfg(not(feature = "fn_traits"))]
        assert_eq!(0.0, sin.as_ptr()(0.0));
    }
    #[test]
    fn snprintf() {
        let libc = Library::new(c_str!("libc.so.6"), false).unwrap();
        let snprintf = libc
            .sym_func::<unsafe extern "C" fn(*mut i8, usize, *const i8, ...) -> i32>(c_str!(
                "snprintf"
            ))
            .expect("symbol is present and not null");
        let mut buf = [0u8; 64];
        let result = unsafe {
            snprintf.as_ptr()(
                buf.as_mut_ptr().cast(),
                buf.len(),
                c_str!("Hello, %s!").as_ptr(),
                c_str!("World").as_ptr(),
            )
        };
        assert_eq!(result, "Hello, World!".len().try_into().unwrap());
        assert!(buf.starts_with(b"Hello, World!\0"));
    }
    #[test]
    fn owned() {
        let libm = Library::new(c_str!("libm.so.6"), false).unwrap();
        let libm = Arc::new(libm);
        let sin = libm
            .sym_func_owned::<extern "C" fn(f64) -> f64>(c_str!("sin"))
            .expect("symbol is present and not null");
        drop(libm);
        let sin = unsafe { sin.assert_shared() };
        #[cfg(feature = "fn_traits")]
        assert_eq!(0.0, sin(0.0));
        #[cfg(not(feature = "fn_traits"))]
        assert_eq!(0.0, sin.as_ptr()(0.0));
    }

    // Requires "-C", "link-args=-Wl,-export-dynamic" in rustflags in config.toml
    #[no_mangle]
    pub static COUNTER: AtomicUsize = AtomicUsize::new(0);

    #[test]
    fn leak() {
        let libm = Library::new(c_str!("libm.so.6"), false).unwrap();
        let libm = Arc::new(libm);
        let sin = libm
            .sym_func_owned::<extern "C" fn(f64) -> f64>(c_str!("sin"))
            .expect("symbol is present and not null");
        drop(libm);
        let sin = unsafe { sin.assert_shared() };
        let sin = sin.leak();
        #[cfg(feature = "fn_traits")]
        assert_eq!(0.0, sin(0.0));
        #[cfg(not(feature = "fn_traits"))]
        assert_eq!(0.0, sin.as_ptr()(0.0));
    }
    #[test]
    fn this_program() {
        let this = Arc::new(Library::this_program().unwrap());
        let counter = this
            .sym_var_owned::<AtomicUsize>(c_str!("COUNTER"))
            .expect("symbol is present and not null");
        let counter = unsafe { counter.assert_shared().assert_send_sync() };
        counter.fetch_add(1, Ordering::Relaxed);
        COUNTER.fetch_add(1, Ordering::Relaxed);
        assert_eq!(counter.load(Ordering::Relaxed), 2);
        assert_eq!(COUNTER.load(Ordering::Relaxed), 2);
        let handle = std::thread::spawn(move || {
            for _ in 0..100000 {
                counter.fetch_add(1, Ordering::Relaxed);
            }
            counter
        });
        for _ in 0..100000 {
            COUNTER.fetch_add(1, Ordering::Relaxed);
        }
        let counter = handle.join().unwrap();
        assert_eq!(counter.load(Ordering::Relaxed), 200002);
        assert_eq!(COUNTER.load(Ordering::Relaxed), 200002);
    }

    // Requires "-C", "link-args=-Wl,-export-dynamic" in rustflags in config.toml
    #[no_mangle]
    pub static mut ARRAY: [usize; 64] = [0; 64];

    #[cfg(feature = "unsize")]
    #[test]
    fn unsize() {
        let this = Library::this_program().unwrap();
        let array = this
            .sym_var::<[usize; 64]>(c_str!("ARRAY"))
            .expect("symbol is present and not null");
        let array = unsafe { array.assert_unique() };
        let mut array: AssertUnique<LibraryVar<[usize]>> = array;
        assert_eq!(array.len(), 64);
        for (i, element) in array.iter_mut().enumerate() {
            *element = i;
        }
        drop(array);
        let array = unsafe { ARRAY };
        for (i, element) in array.into_iter().enumerate() {
            assert_eq!(i, element);
        }
    }
}
