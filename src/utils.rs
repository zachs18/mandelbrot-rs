#[macro_export]
macro_rules! unwrap_or {
    ( $value:expr, $else_value:expr , with_message($e:pat) $( $msg:tt )* ) => {
        // This arm only accepts Result
        match ($value) {
            Ok(x) => x,
            Err($e) => {
                eprintln!($($msg)*);
                $else_value
            },
        }
    };
    ( $value:expr, $else_value:expr , with_error($e:pat)) => {
        // This arm only accepts Result
        match ($value) {
            Ok(x) => x,
            Err($e) => {
                $else_value
            },
        }
    };
    ( $value:expr, $else_value:expr $(, $( with_message $( $msg:tt )* )? )? ) => {
        // Option<T> -> Option<Option<T>> -> Option<T>
        // Result<T> -> Result<Option<T>> -> Option<T>
        match ($value).map(Some).unwrap_or_default() {
            Some(x) => x,
            None => {
                $( $( eprintln!($($msg)*); )? )?
                $else_value
            },
        }
    };
}
pub use unwrap_or;

#[macro_export]
macro_rules! unwrap_or_return {
    ( $value:expr, $retval:expr $(, $( $rest:tt )* )? ) => {
        $crate::util::unwrap_or!($value, return $retval $(, $($rest)* )? )
    }
}
pub use unwrap_or_return;

#[macro_export]
macro_rules! shadow_or {
    ( mut $shadow:ident, $retval:expr $(, $( $rest:tt )* )? ) => {
        let mut $shadow = unwrap_or!($shadow, $retval $(, $( $rest )* )?);
    };
    ( $shadow:ident, $retval:expr $(, $( $rest:tt )* )? ) => {
        let $shadow = unwrap_or!($shadow, $retval $(, $( $rest )* )?);
    };
}
pub use shadow_or;

#[macro_export]
macro_rules! shadow_or_return {
    ( mut $shadow:ident, $retval:expr $(, $( $rest:tt )* )? ) => {
        $crate::util::shadow_or!(mut $shadow, return $retval $(, $($rest)* )? )
    };
    ( $shadow:ident, $retval:expr $(, $( $rest:tt )* )? ) => {
        $crate::util::shadow_or!($shadow, return $retval $(, $($rest)* )? )
    };
}
pub use shadow_or_return;

#[macro_export]
macro_rules! c_str {
    ( $s:literal ) => {{
        lazy_static::lazy_static! {
            static ref S: &'static std::ffi::CStr = {
                let s: &'static str = concat!($s, "\0");
                let s: &'static [u8] = s.as_bytes();
                std::ffi::CStr::from_bytes_with_nul(s).unwrap()
            };
        }
        &**S
    }};
}
pub use c_str;
