pub unsafe trait FnPtr: Copy {
    fn addr(self) -> *const ();
}

#[cfg(feature = "fn_traits")]
macro_rules! make_fn_trait_impls {
    ([no_variadic] $($ty:ident)*) => {
        unsafe impl<$($ty ,)* R> FnPtr for unsafe extern "C" fn($($ty,)*) -> R {
            fn addr(self) -> *const () {
                self as *const ()
            }
        }

        unsafe impl<$($ty ,)* R> FnPtr for extern "C" fn($($ty,)*) -> R {
            fn addr(self) -> *const () {
                self as *const ()
            }
        }
    };
    ($($ty:ident)*) => {
        unsafe impl<$($ty ,)* R> FnPtr for unsafe extern "C" fn($($ty,)*) -> R {
            fn addr(self) -> *const () {
                self as *const ()
            }
        }

        unsafe impl<$($ty ,)* R> FnPtr for extern "C" fn($($ty,)*) -> R {
            fn addr(self) -> *const () {
                self as *const ()
            }
        }

        unsafe impl<$($ty ,)* R> FnPtr for unsafe extern "C" fn($($ty,)* ...) -> R {
            fn addr(self) -> *const () {
                self as *const ()
            }
        }

        unsafe impl<$($ty ,)* R> FnPtr for extern "C" fn($($ty,)* ...) -> R {
            fn addr(self) -> *const () {
                self as *const ()
            }
        }
        // impl<$($ty ,)* R> FnMut<($($ty,)*)> for AssertShared<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
        //     extern "rust-call" fn call_mut(&mut self, ($($var,)*): ($($ty,)*)) -> Self::Output {
        //         let f = self.as_ptr();
        //         { f($($var,)*) }
        //     }
        // }
        // impl<$($ty ,)* R> Fn<($($ty,)*)> for AssertShared<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
        //     extern "rust-call" fn call(&self, ($($var,)*): ($($ty,)*)) -> Self::Output {
        //         let f = self.as_ptr();
        //         { f($($var,)*) }
        //     }
        // }

        // impl<$($ty ,)* R> FnOnce<($($ty,)*)> for AssertUnique<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
        //     type Output = R;
        //     extern "rust-call" fn call_once(self, ($($var,)*): ($($ty,)*)) -> Self::Output {
        //         let f = self.as_ptr();
        //         { f($($var,)*) }
        //     }
        // }
        // impl<$($ty ,)* R> FnMut<($($ty,)*)> for AssertUnique<OwnedLibraryFunc<extern "C" fn($($ty,)*) -> R>> {
        //     extern "rust-call" fn call_mut(&mut self, ($($var,)*): ($($ty,)*)) -> Self::Output {
        //         let f = self.as_ptr();
        //         { f($($var,)*) }
        //     }
        // }
    };
}
make_fn_trait_impls!([no_variadic]);
make_fn_trait_impls!(A);
make_fn_trait_impls!(A B);
make_fn_trait_impls!(A B C);
make_fn_trait_impls!(A B C D);
make_fn_trait_impls!(A B C D E);
make_fn_trait_impls!(A B C D E F);
make_fn_trait_impls!(A B C D E F G);
make_fn_trait_impls!(A B C D E F G H);
make_fn_trait_impls!(A B C D E F G H I);
make_fn_trait_impls!(A B C D E F G H I J);
