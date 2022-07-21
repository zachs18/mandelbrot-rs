use std::{fs::File, os::unix::prelude::FromRawFd, io::Write, ffi::{CString, CStr}, process::{Command, Stdio}};

use loader::{Library, c_str};

#[cfg(not(any(
    all(target_os = "linux", target_env = "gnu"),
    all(target_os = "linux", target_env = "musl"),
    target_os = "freebsd",
)))]
compile_error!("The libc crate only has the memfd_create syscall under linux-gnu, linux-musl, and freebsd.");

unsafe fn make_fd(name: &CStr, flags: u32) -> Result<i32, std::io::Error> {
    let fd = libc::memfd_create(name.as_ptr(), flags);
    if fd >= 0 {
        Ok(fd)
    } else {
        Err(std::io::Error::last_os_error())
    }
}

#[derive(Debug)]
pub enum CompileError {
    IOError(std::io::Error),
    CompileError(CString),
    DLError(CString),
}

impl From<std::io::Error> for CompileError {
    fn from(err: std::io::Error) -> Self {
        CompileError::IOError(err)
    }
}

impl From<CString> for CompileError {
    fn from(err: CString) -> Self {
        CompileError::DLError(err)
    }
}

pub enum OptimizationLevel {
    NoOptimization,
    One,
    Two,
    Three,
    ForDebugging,
}

impl OptimizationLevel {
    fn arg(&self) -> &'static str {
        match self {
            OptimizationLevel::NoOptimization => "-O0",
            OptimizationLevel::One => "-O1",
            OptimizationLevel::Two => "-O2",
            OptimizationLevel::Three => "-O3",
            OptimizationLevel::ForDebugging => "-Og",
        }
    }
}

pub fn compile(compiler: &str, source: &str, language: &str, optimization: OptimizationLevel, debug_symbols: bool) -> Result<Library, CompileError> {
    let pid = unsafe {
        libc::getpid()
    };
    let source_fd = unsafe {
        make_fd(c_str!("source"), 0)
    }?;
    let code_fd = unsafe {
        make_fd(c_str!("code"), 0)
    }?;

    let source_path = format!("/proc/{pid}/fd/{source_fd}");
    let code_path = format!("/proc/{pid}/fd/{code_fd}");

    let mut source_file = unsafe { File::from_raw_fd(source_fd) };

    source_file.write_all(source.as_bytes())?;
    source_file.flush()?;

    let mut args = vec![
        "-shared", optimization.arg(),
        "-x", language,
        &source_path,
        "-o", &code_path,
    ];
    if debug_symbols {
        args.push("-g");
    }
    
    let handle = Command::new(compiler)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .args(args)
        .spawn()?;

    let mut x = handle.wait_with_output()?;

    if !x.status.success() {
        let mut errmsg = std::mem::take(&mut x.stdout);
        errmsg.append(&mut x.stderr);
        return Err(CompileError::CompileError(CString::new(errmsg).unwrap()));
    }

    let code_path = CString::new(code_path).unwrap();

    let handle = Library::new(&code_path, false)?;
    
    Ok(handle)
}

#[test]
fn main() {
    let handle = compile(
        "gcc",
        "int add(int x, int y) { return x + y; }",
        "c",
        OptimizationLevel::ForDebugging,
        true
    ).unwrap();
    
    let func = handle.sym_func::<extern "C" fn(i32, i32) -> i32>(c_str!("add")).unwrap();
    let func = unsafe { func.assert_callable_shared() };

    assert_eq!(func(3, 4), 7);
}


#[test]
fn float() {
    let handle = compile(
        "gcc",
        "void mandelbrot(
            _Complex double *result,
            const _Complex double *c,
            const _Complex double *z
        ) {
            *result = *z * *z + *c;
        }
        ",
        "c",
        OptimizationLevel::ForDebugging,
        true
    ).unwrap();

    use num_complex::Complex;
    
    let func = handle.sym_func::<
        extern "C" fn(*mut Complex<f64>, *const Complex<f64>, *const Complex<f64>)
    >(c_str!("mandelbrot")).unwrap();
    let func = unsafe { func.assert_callable_shared() };

    let mut parameter = Complex { re: 0.0, im: 0.0 };
    let negative_three_fourths = Complex { re: -0.75, im: 0.0 };

    let mut result = Complex::default();
    func(&mut result, &negative_three_fourths, &parameter);
    assert_eq!(result, negative_three_fourths);
    parameter = result;
    func(&mut result, &negative_three_fourths, &parameter);
    assert_eq!(result, negative_three_fourths * negative_three_fourths + negative_three_fourths);
}
