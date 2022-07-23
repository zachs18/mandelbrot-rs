use std::{fmt::Write, rc::Rc};

use either::Either;
use loader::Library;
use nom_locate::LocatedSpan;

use self::{type_checker::{SymbolTable, Symbol, TypeKind, Type, TypedExpression, TypedExpressionKind, TypedFunctionDefinition}};

pub(self) type Span<'src> = LocatedSpan<&'src str>;

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
mod type_checker;

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

#[derive(Debug, Clone, Copy)]
enum Component {
    Real,
    Imag,
}

impl Component {
    fn extract_op(self) -> &'static str {
        match self {
            Component::Real => "__real__",
            Component::Imag => "__imag__",
        }
    }
}

/// Translate into compilable C code.
/// Always put a space before something if it could combine.
/// Always put parentheses around things.
trait Translate {
    fn translate(&self, float_type: FloatType, f: &mut String) -> std::fmt::Result;
}
impl<'src> Translate for TypedExpression<'src> {
    fn translate(&self, float_type: FloatType, f: &mut String) -> std::fmt::Result {
        match &self.kind {
            TypedExpressionKind::Literal(literal) => f.write_fmt(format_args!(" {}", literal)),
            TypedExpressionKind::Variable(var) => f.write_fmt(format_args!(" {}", var)),
            TypedExpressionKind::UnaryOp(op, expr) => {
                f.write_str("(")?;
                op.translate(float_type, f)?;
                expr.translate(float_type, f)?;
                f.write_str(")")
            },
            TypedExpressionKind::BinOp(lhs, op, rhs) => {
                f.write_str("(")?;
                lhs.translate(float_type, f)?;
                op.translate(float_type, f)?;
                rhs.translate(float_type, f)?;
                f.write_str(")")
            },
            TypedExpressionKind::Component(expr, component) => {
                f.write_fmt(format_args!("({}(", component.extract_op()))?;
                expr.translate(float_type, f)?;
                f.write_str("))")
            },
            TypedExpressionKind::FunctionCall(func, args) => {
                f.write_str("(")?;
                f.write_str(&func.c_name(float_type))?;
                f.write_str("(")?;
                let mut args = args.iter();
                if let Some(arg) = args.next() {
                    arg.translate(float_type, f)?;
                    for arg in args {
                        f.write_str(", ")?;
                        arg.translate(float_type, f)?;
                    }
                }
                f.write_str("))")
            },
        }
    }
}
impl Translate for Op {
    fn translate(&self, _: FloatType, f: &mut String) -> std::fmt::Result {
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
impl<'src> Translate for TypedFunctionDefinition<'src> {
    fn translate(&self, float_type: FloatType, f: &mut String) -> std::fmt::Result {

        static TODO_ONCE: std::sync::Once = std::sync::Once::new();
        TODO_ONCE.call_once(|| {
            eprintln!("TODO: don't hard-code function name or argument names, to allow for multiple functions");
            eprintln!("    : Also to allow for multiple functions, probably will need to add prototypes");
            eprintln!("    : e.g. go through each function, add prototype, then go through each function and add impl.");
        });

        f.write_fmt(format_args!(
r#"void {func}(
    _Complex {ctype} *_result,
    const _Complex {ctype} *_c,
    const _Complex {ctype} *_z
) {{
    const _Complex {ctype} c = *_c;
    const _Complex {ctype} z = *_z;
    *_result = "#,
            func = float_type.c_func(),
            ctype = float_type.c_name(),
        ))?;
        self.body.translate(float_type, f)?;
        f.write_str(";}")
    }
}


pub fn compile(src: &str) -> Result<Library, (&'static str, Option<Span>)> {
    let span = LocatedSpan::new(src);

    let tokens = match lexer::lex(span) {
        Ok(tokens) => tokens,
        Err(err) => return Err((err.explanation(), err.span())),
    };

    let definition = match parser::parse_tokens(&tokens) {
        Ok(definition) => definition,
        Err(err) => return Err((err.explanation(), err.span())),
    };

    let real = Type::from(&TypeKind::Real);
    let complex = Type::from(&TypeKind::Complex);
    let rfnr = Type::from(TypeKind::Function { args: vec![real.clone()], returns: real.clone() });
    let rfnc = Type::from(TypeKind::Function { args: vec![complex.clone()], returns: real.clone() });

    fn make_builtin<'src>(name: &'src str, r#type: Type, single: &'src str, double: &'src str, quad: &'src str) -> (&'src str, Symbol<'src>) {
        (
            name,
            Symbol {
                r#type,
                definition: None,
                c_name: Either::Right(Rc::new(Symbol::c_name_explicit(single, double, quad)))
            },
        )
    }

    let global_symbols = SymbolTable { parent: None, symbols: [
        make_builtin("abs", rfnr.clone(), "fabsf", "fabs", "__builtin_fabsq"),
        make_builtin("cabs", rfnc.clone(), "cabsf", "cabs", "cabsq"),
        make_builtin("ln", rfnr.clone(), "logf", "log", "logq"),
    ].into() };

    let definition = match type_checker::TypeCheck::type_check(&definition, &global_symbols) {
        Ok(definition) => definition,
        Err(err) => return Err((err.explanation(), err.span())),
    };

    if definition.args.len() != 2 {
        return Err(("Entry point does not have two argments", None));
    }

    match [&definition.args[0].c_name, &definition.args[1].c_name] {
        [Either::Left("c"), Either::Left("z")] => {},
        [Either::Left("z"), Either::Left("c")] => {},
        _ => return Err(("Entry point does not have arguments 'z' and 'c'", None)),
    };

    static TODO_ONCE: std::sync::Once = std::sync::Once::new();
    TODO_ONCE.call_once(|| {
        eprintln!("TODO: maybe trig using GCC libquadmath");
    });

    let mut source = String::with_capacity(4096);
    source.push_str(
"#include <quadmath.h>
#include <math.h>
static inline _Float128 _cabsq(_Complex _Float128 x) {
    return hypotq(__real__ x, __imag__ x);
}"
    );
    definition.translate(FloatType::F32, &mut source).map_err(|_| ("Failed to translate for single-precision", None))?;
    definition.translate(FloatType::F64, &mut source).map_err(|_| ("Failed to translate for double-precision", None))?;
    definition.translate(FloatType::F128, &mut source).map_err(|_| ("Failed to translate for quad-precision", None))?;

    compile_in_memory::compile(
        "gcc",
        &source,
        "c",
        compile_in_memory::OptimizationLevel::Two,
        false,
    ).map_err(|err| {
        dbg!(err);
        ("Failed to compile", None)
    })
}
