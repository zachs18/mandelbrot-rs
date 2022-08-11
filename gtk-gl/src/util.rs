#[macro_export]
macro_rules! shader_source {
    ($($path:literal),*) => {
        {
            const MAX_LEN: usize = gl::types::GLint::MAX as usize;
            const _COUNT: usize = shader_source!(@count [$($path),*]);
            const SOURCES: [&'static [u8]; _COUNT] = [
                $({
                    let source: &'static [u8] = include_bytes!($path);
                    if source.len() > MAX_LEN {
                        panic!("Shader source too long.");
                    }
                    source
                }),*
            ];
            const COUNT: i32 = match _COUNT {
                len@(0..=MAX_LEN) => len as i32,
                _ => panic!("Shader source too long"),
            };
            let string: [*const i8; _COUNT] = SOURCES.map(|source| source.as_ptr() as *const i8);
            let length = SOURCES.map(|source| match source.len() {
                len@(0..=MAX_LEN) => len as i32,
                _ => unreachable!(),
            });
            (COUNT, string, length)
        }
    };
    (@count []) => {
        0
    };
    (@count [$path1:literal $(, $($path:literal),+)?]) => {
        1 + shader_source!(@count [$($($path),+)?])
    };
}

#[macro_export]
macro_rules! c_str {
    ($value:literal) => {
        {
            const S: &'static str = concat!($value, "\0");
            std::ffi::CStr::from_bytes_with_nul(S.as_bytes()).unwrap()
        }
    };
}
