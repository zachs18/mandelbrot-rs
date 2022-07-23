use std::{rc::Rc, ops::Deref, collections::{HashMap}, borrow::Cow};

use either::Either;

use super::{FloatType, Op, Component, parser::{Expression, FunctionDefinition}, Span};

#[derive(Debug, Clone)]
pub struct TypedFunctionDefinition<'src> {
    pub(super) name: Symbol<'src>,
    pub(super) args: Vec<Symbol<'src>>,
    pub(super) body: TypedExpression<'src>,
}

#[derive(Debug, Clone)]
pub(super) struct TypedExpression<'src> {
    pub(super) r#type: Type,
    pub(super) kind: TypedExpressionKind<'src>,
}

#[derive(Debug, Clone)]
pub(super) enum TypedExpressionKind<'src> {
    Literal(Span<'src>),
    Variable(Span<'src>),
    UnaryOp(Op, Box<TypedExpression<'src>>),
    BinOp(Box<TypedExpression<'src>>, Op, Box<TypedExpression<'src>>),
    /// Used for .re and .im
    Component(Box<TypedExpression<'src>>, Component),
    /// Represents function calls and method calls
    FunctionCall(Symbol<'src>, Vec<TypedExpression<'src>>),
}

// type Type = Either<Rc<TypeKind>, &'static TypeKind>;
#[derive(Clone)]
#[repr(transparent)]
pub struct Type(Either<Rc<TypeKind>, &'static TypeKind>);

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        TypeKind::fmt(&self, f)
    }
}

impl From<&'static TypeKind> for Type {
    fn from(r#type: &'static TypeKind) -> Self {
        Type(Either::Right(r#type))
    }
}

impl From<TypeKind> for Type {
    fn from(r#type: TypeKind) -> Self {
        Type(Either::Left(r#type.into()))
    }
}

impl Deref for Type {
    type Target = TypeKind;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
    
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    // Error,
    Real,
    Complex,
    Function {
        args: Vec<Type>,
        returns: Type,
    }
}

impl Ord for TypeKind {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        use std::cmp::Ordering::*;
        match (self, other) {
            // (TypeKind::Error, TypeKind::Error) => Equal,
            // (TypeKind::Error, _) => Less,
            // (_, TypeKind::Error) => Greater,
            (TypeKind::Real, TypeKind::Real) => Equal,
            (TypeKind::Real, _) => Less,
            (_, TypeKind::Real) => Greater,
            (TypeKind::Complex, TypeKind::Complex) => Equal,
            (TypeKind::Complex, _) => Less,
            (_, TypeKind::Complex) => Greater,
            
            (TypeKind::Function { args: l_args, returns: l_returns }, TypeKind::Function { args: r_args, returns: r_returns }) => {
                TypeKind::cmp(&l_returns, &r_returns).then_with(|| {
                    // Modified from <A as SliceOrd>::compare at core/slice/cmp.rs:166
                    let l = usize::min(l_args.len(), r_args.len());

                    let lhs = &l_args[..l];
                    let rhs = &l_args[..l];

                    for i in 0..l {
                        match TypeKind::cmp(&lhs[i], &rhs[i]) {
                            Equal => (),
                            non_eq => return non_eq,
                        }
                    }

                    usize::cmp(&l_args.len(), &r_args.len())
                })
            },
        }
    }
}

impl PartialOrd for TypeKind {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(TypeKind::cmp(self, other))
    }
}

impl Eq for TypeKind {}

impl PartialEq for TypeKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Function { args: l_args, returns: l_returns }, Self::Function { args: r_args, returns: r_returns }) => {
                TypeKind::eq(&l_returns, &r_returns)
                && l_args.len() == r_args.len()
                && l_args.iter().zip(r_args.iter()).all(|(l_arg, r_arg)| TypeKind::eq(l_arg, r_arg))
            },
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Clone)]
pub(super) struct Symbol<'src> {
    pub(super) r#type: Type,
    pub(super) definition: Option<Span<'src>>,
    pub(super) c_name: Either<&'src str, Rc<dyn Fn(FloatType) -> Cow<'src, str> + 'src>>,
}

impl<'src> std::fmt::Debug for Symbol<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c_name: Cow<str> = match &self.c_name {
            &Either::Left(c_name) => c_name.into(),
            Either::Right(func) => format!("({}, {}, {})", func(FloatType::F32), func(FloatType::F64), func(FloatType::F128)).into(),
        };
        f.debug_struct("Symbol").field("r#type", &self.r#type).field("c_name", &c_name).finish()
    }
}

impl<'src> Symbol<'src> {
    pub(super) fn c_name_append_size(ident: &'src str) -> impl Fn(FloatType) -> Cow<'src, str> + 'src {
        let single = format!("{}_32", ident);
        let double = format!("{}_64", ident);
        let quad = format!("{}_128", ident);
        move |float_type: FloatType| {
            match float_type {
                FloatType::F32 => single.clone().into(),
                FloatType::F64 => double.clone().into(),
                FloatType::F128 => quad.clone().into(),
            }
        }
    }
    pub(super) fn c_name_explicit(single: &'src str, double: &'src str, quad: &'src str) -> impl Fn(FloatType) -> Cow<'src, str> + 'src {
        move |float_type: FloatType| {
            match float_type {
                FloatType::F32 => single.into(),
                FloatType::F64 => double.into(),
                FloatType::F128 => quad.into(),
            }
        }
    }

    pub(super) fn c_name(&self, float_type: FloatType) -> Cow<'src, str> {
        match self.c_name {
            Either::Left(name) => name.into(),
            Either::Right(ref func) => func(float_type),
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct SymbolTable<'parent, 'src> {
    pub(super) parent: Option<&'parent SymbolTable<'parent, 'src>>,
    pub(super) symbols: HashMap<&'src str, Symbol<'src>>,
}

impl<'parent, 'src> SymbolTable<'parent, 'src> {
    fn get(&self, symbol: &str) -> Option<&Symbol<'src>> {
        self.symbols.get(symbol).or_else(|| {
            self.parent.map(|table| table.get(symbol)).flatten()
        })
    }
    fn insert(&mut self, symbol: &'src str, value: Symbol<'src>) -> Option<Symbol<'src>> {
        self.symbols.insert(symbol, value)
    }
}


#[derive(Debug)]
pub(super) enum TypeCheckError<'src> {
    UndeclaredFunction(Span<'src>),
    UndeclaredVariable(Span<'src>),
    BinOp(TypedExpression<'src>, Op, TypedExpression<'src>),
    Component(TypedExpression<'src>, Component),
    FunctionCallWrongArgs(Symbol<'src>, Vec<TypedExpression<'src>>),
    NotAFunction(Span<'src>),
    Redefinition(Span<'src>, Option<Span<'src>>),
}

pub(super) trait TypeCheck<'src> {
    type Output;
    fn type_check(&self, symbols: &SymbolTable<'_, 'src>) -> Result<Self::Output, TypeCheckError<'src>>;
}

impl<'src> TypeCheck<'src> for FunctionDefinition<'src> {
    type Output = TypedFunctionDefinition<'src>;

    fn type_check(&self, symbols: &SymbolTable<'_, 'src>) -> Result<Self::Output, TypeCheckError<'src>> {
        let &FunctionDefinition { name, ref args, ref body } = self;

        let mut symbols = SymbolTable { parent: Some(symbols), symbols: HashMap::new() };

        let args = args.iter().map(|&arg| {
            let symbol = Symbol {
                r#type: Type::from(&TypeKind::Complex),
                definition: Some(arg),
                c_name: Either::Left(&arg),
            };
            if let Some(prev_sym) = symbols.insert(&arg, symbol.clone()) {
                Err(TypeCheckError::Redefinition(arg, prev_sym.definition))
            } else {
                Ok(symbol)
            }
        }).collect::<Result<Vec<_>, _>>()?;

        let func_type = TypeKind::Function { args: vec![Type::from(&TypeKind::Complex); args.len()], returns: Type::from(&TypeKind::Complex) };
        let func_type = Type::from(func_type);

        let func_name = Symbol::c_name_append_size(name.fragment());

        let func_sym = Symbol {
            r#type: func_type,
            definition: Some(self.name),
            c_name: Either::Right(Rc::new(func_name)),
        };

        if let Some(prev_sym) = symbols.insert(name.fragment(), func_sym.clone()) {
            return Err(TypeCheckError::Redefinition(name, prev_sym.definition));
        }
        
        let body = body.type_check(&symbols)?;

        Ok(TypedFunctionDefinition {
            name: func_sym,
            args,
            body,
        })
    }
}

impl<'src> TypeCheck<'src> for Expression<'src> {
    type Output = TypedExpression<'src>;

    fn type_check(&self, symbols: &SymbolTable<'_, 'src>) -> Result<Self::Output, TypeCheckError<'src>> {
        match *self {
            Expression::Literal(literal) => Ok(TypedExpression {
                r#type: if literal.ends_with("i") {
                    Type::from(&TypeKind::Complex)
                }else {
                    Type::from(&TypeKind::Real)
                },
                kind: TypedExpressionKind::Literal(literal),
            }),
            Expression::Variable(variable) => Ok(TypedExpression {
                r#type: if let Some(symbol) = symbols.get(&variable) {
                    symbol.r#type.clone()
                } else {
                    // Type::from(&TypeKind::Error)
                    return Err(TypeCheckError::UndeclaredVariable(variable))
                },
                kind: TypedExpressionKind::Variable(variable),
            }),
            Expression::UnaryOp(op, ref expr) => {
                let expr = expr.type_check(symbols)?;
                Ok(TypedExpression {
                    r#type: expr.r#type.clone(),
                    kind: TypedExpressionKind::UnaryOp(op, expr.into()),
                })
            },
            Expression::BinOp(ref lhs, op, ref rhs) => {
                let lhs = lhs.type_check(symbols)?;
                let rhs = rhs.type_check(symbols)?;
                let r#type = match (&*lhs.r#type, op, &*rhs.r#type) {
                    // (TypeKind::Error, _, _) => Type::from(&TypeKind::Error),
                    // (_, _, TypeKind::Error) => Type::from(&TypeKind::Error),
                    (TypeKind::Real, _, TypeKind::Real) => Type::from(&TypeKind::Real),
                    (TypeKind::Real, _, TypeKind::Complex) => Type::from(&TypeKind::Complex),
                    (TypeKind::Complex, _, TypeKind::Real) => Type::from(&TypeKind::Complex),
                    (TypeKind::Complex, _, TypeKind::Complex) => Type::from(&TypeKind::Complex),
                    // _ => Type::from(&TypeKind::Error),
                    _ => return Err(TypeCheckError::BinOp(lhs, op, rhs)),
                };
                Ok(TypedExpression {
                    r#type,
                    kind: TypedExpressionKind::BinOp(lhs.into(), op, rhs.into()),
                })
            },
            Expression::Component(ref expr, component) => {
                let expr = expr.type_check(symbols)?;
                let r#type = match &*expr.r#type {
                    TypeKind::Real | TypeKind::Complex => Type::from(&TypeKind::Real),
                    // _ => Type::from(&TypeKind::Error),
                    _ => return Err(TypeCheckError::Component(expr, component)),
                };
                Ok(TypedExpression {
                    r#type,
                    kind: TypedExpressionKind::Component(expr.into(), component),
                })
            },
            Expression::FunctionCall(func, ref args) => {
                let func_sym = match symbols.get(&func) {
                    Some(func_sym) => func_sym,
                    None => return Err(TypeCheckError::UndeclaredFunction(func)),
                };
                
                let (r#type, args) = match &*func_sym.r#type {
                    TypeKind::Function { args: arg_tys, returns: return_ty } => {
                        let args = args.iter().map(|arg| arg.type_check(symbols)).collect::<Result<Vec<_>, _>>()?;
                        if arg_tys.len() == args.len()
                            && arg_tys.iter().zip(args.iter())
                                .all(|(ty, arg)| TypeKind::eq(&arg.r#type, ty))
                        {
                            (return_ty.clone(), args)
                        } else {
                            // Type::from(&TypeKind::Error)
                            return Err(TypeCheckError::FunctionCallWrongArgs(func_sym.clone(), args));
                        }
                    },
                    _ => return Err(TypeCheckError::NotAFunction(func)),
                };

                Ok(TypedExpression {
                    r#type,
                    kind: TypedExpressionKind::FunctionCall(func_sym.clone(), args),
                })
            },
        }
    }
}
