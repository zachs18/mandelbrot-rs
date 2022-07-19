use std::fmt::Write;

use loader::Library;

use crate::custom_fractal::parser::Expression;

use self::parser::FunctionDefinition;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(self) enum Op {
    Assign,
    Plus,
    Minus,
    Times,
    Divide,
    /// "~", represents complex conjugate
    Conjugate,
}

mod lexer;
mod parser;

#[derive(Debug, Clone, Copy)]
enum FloatType {
    F32,
    F64,
    F128,
}

impl FloatType {
    fn c_name(self) -> &'static str {
        match self {
            FloatType::F32 => "float",
            FloatType::F64 => "double",
            FloatType::F128 => "_Float128",
        }
    }
    fn c_func(self) -> &'static str {
        match self {
            FloatType::F32 => "func32",
            FloatType::F64 => "func64",
            FloatType::F128 => "func128",
        }
    }
}

/// Always put a space before something if it could combine
trait Translate {
    fn translate(&self, f: &mut String) -> std::fmt::Result;
}
impl<'src> Translate for Expression<'src> {
    fn translate(&self, f: &mut String) -> std::fmt::Result {
        match self {
            Expression::Literal(literal) => f.write_fmt(format_args!(" {}", literal)),
            Expression::Variable(var) => f.write_fmt(format_args!(" *{}", var)),
            Expression::UnaryOp(op, expr) => {
                op.translate(f)?;
                expr.translate(f)
            },
            Expression::BinOp(lhs, op, rhs) => {
                lhs.translate(f)?;
                op.translate(f)?;
                rhs.translate(f)
            },
        }
    }
}
impl Translate for Op {
    fn translate(&self, f: &mut String) -> std::fmt::Result {
        f.write_str(match self {
            Op::Assign => " =",
            Op::Plus => " +",
            Op::Minus => " -",
            Op::Times => " *",
            Op::Divide => " /",
            Op::Conjugate => " ~",
        })
    }
}
impl<'src> Translate for (&FunctionDefinition<'src>, FloatType) {
    fn translate(&self, f: &mut String) -> std::fmt::Result {
        f.write_fmt(format_args!(
r#"void {func}(
    _Complex {ctype} *result,
    const _Complex {ctype} *c,
    const _Complex {ctype} *z
) {{
    *result = "#,
            func = self.1.c_func(),
            ctype = self.1.c_name(),
        ))?;
        self.0.body.translate(f)?;
        f.write_str(";}")
    }
}

#[track_caller]
fn debug_and_ignore_impl<E: std::fmt::Debug>(e: E) -> () {
    println!("{}: {e:?}",  std::panic::Location::caller());
}
macro_rules! debug_and_ignore {
    () => {
        |e| debug_and_ignore_impl(e)
    };
}

pub fn compile(src: &str) -> Result<Library, ()> {
    let tokens = lexer::lex(src).map_err(debug_and_ignore!())?;
    let definition = parser::parse_tokens(&tokens).map_err(debug_and_ignore!())?;

    match &*definition.args {
        &["c", "z"] => {},
        &["z", "c"] => {},
        _ => return Err(()),
    };

    let mut source = String::with_capacity(4096);
    (&definition, FloatType::F32).translate(&mut source).map_err(debug_and_ignore!())?;
    (&definition, FloatType::F64).translate(&mut source).map_err(debug_and_ignore!())?;
    (&definition, FloatType::F128).translate(&mut source).map_err(debug_and_ignore!())?;

    compile_in_memory::compile(
        "gcc",
        &source,
        "c",
        compile_in_memory::OptimizationLevel::Two,
        false,
    ).map_err(debug_and_ignore!())
}
