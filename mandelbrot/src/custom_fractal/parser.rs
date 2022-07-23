use nom::{IResult, multi::{separated_list0, many0, separated_list1}, sequence::{tuple, delimited, preceded, terminated}, Parser, Finish, branch::alt, combinator::{opt, map_res}};

use super::{
    lexer::{Token, TokenKind},
    Op, Component, Span,
};

pub struct FunctionDefinition<'src> {
    pub(super) name: Span<'src>,
    pub(super) args: Vec<Span<'src>>,
    pub(super) body: Expression<'src>,
}

#[derive(Debug, Clone)]
pub(super) enum Expression<'src> {
    Literal(Span<'src>),
    Variable(Span<'src>),
    UnaryOp(Op, Box<Expression<'src>>),
    BinOp(Box<Expression<'src>>, Op, Box<Expression<'src>>),
    /// Used for .re and .im
    Component(Box<Expression<'src>>, Component),
    /// Represents function calls and method calls
    FunctionCall(Span<'src>, Vec<Expression<'src>>),
}

#[derive(Debug)]
pub(super) enum ParseError<'tok, 'src> {
    /// A specific token kind was expected.
    TokenKind(TokenKind, Option<Token<'src>>),
    InvalidField(Span<'src>),
    /// A non-ident was used as a function
    InvalidFunction(Expression<'src>),
    ExtraInput(&'tok [Token<'src>]),
    Other(nom::error::Error<&'tok [Token<'src>]>),
}

impl<'tok, 'src> ParseError<'tok, 'src> {
    pub(super) fn explanation(&self) -> &'static str {
        match self {
            ParseError::TokenKind(tok, _) => match tok {
                TokenKind::Op(op) => match op {
                    Op::Assign => "Expected '='",
                    Op::Plus => "Expected '+'",
                    Op::Minus => "Expected '-'",
                    Op::Times => "Expected '*'",
                    Op::Divide => "Expected '/'",
                    Op::Conjugate => "Expected '~'",
                },
                TokenKind::LParen => "Expected '('",
                TokenKind::RParen => "Expected ')'",
                TokenKind::Comma => "Expected ','",
                TokenKind::Period => "Expected '.'",
                TokenKind::Ident => "Expected identifier",
                TokenKind::Literal => "Expected number",
            },
            ParseError::InvalidField(_) => "Invalid field",
            ParseError::InvalidFunction(_) => "Invalid function",
            ParseError::ExtraInput(_) => "Extra input",
            ParseError::Other(_) => "Parser error",
        }
    }

    pub(super) fn span(&self) -> Option<Span<'src>> {
        match *self {
            ParseError::TokenKind(_, token) => token.map(|token| token.1),
            ParseError::InvalidField(ident) => Some(ident),
            ParseError::InvalidFunction(_) => None, // TODO
            ParseError::ExtraInput(input) => input.first().map(|token| token.1),
            ParseError::Other(ref err) => err.input.first().map(|token| token.1),
        }
    }
}

impl<'tok, 'src> nom::error::ParseError<&'tok [Token<'src>]> for ParseError<'tok, 'src> {
    fn from_error_kind(input: &'tok [Token<'src>], kind: nom::error::ErrorKind) -> Self {
        ParseError::Other(nom::error::Error::from_error_kind(input, kind))
    }

    fn append(_input: &'tok [Token<'src>], _kind: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

impl<'tok, 'src> nom::error::FromExternalError<&'tok [Token<'src>], ParseError<'tok, 'src>> for ParseError<'tok, 'src> {
    fn from_external_error(_: &'tok [Token<'src>], _: nom::error::ErrorKind, e: ParseError<'tok, 'src>) -> Self {
        e
    }
}

fn ident<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Span<'src>, ParseError<'tok, 'src>> {
    token_kind(TokenKind::Ident).map(|tok| tok.1).parse(input)
}

fn literal<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Span<'src>, ParseError<'tok, 'src>> {
    token_kind(TokenKind::Literal).map(|tok| tok.1).parse(input)
}

fn op(
    op: Op,
) -> impl for<'tok, 'src> FnMut(&'tok [Token<'src>]) -> IResult<&'tok [Token<'src>], Op, ParseError<'tok, 'src>>
{
    move |input| token_kind(TokenKind::Op(op)).map(|_| op).parse(input)
}

#[inline]
fn token_kind(
    kind: TokenKind,
) -> impl for<'tok, 'src> FnMut(&'tok [Token<'src>]) -> IResult<&'tok [Token<'src>], Token<'src>, ParseError<'tok, 'src>>
{
    move |input| match input.split_first() {
        Some((token, rest)) if token.0 == kind => Ok((rest, *token)),
        Some((token, _)) => Err(nom::Err::Error(
            ParseError::TokenKind(kind, Some(*token))
        )),
        None => Err(nom::Err::Error(
            ParseError::TokenKind(kind, None)
        )),
    }
}

/// Syntax: f(x, y, z) = x * y + z

fn function_definition<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], FunctionDefinition<'src>, ParseError<'tok, 'src>> {
    tuple((
        ident,
        delimited(
            token_kind(TokenKind::LParen),
            separated_list0(token_kind(TokenKind::Comma), ident),
            token_kind(TokenKind::RParen),
        ),
        op(Op::Assign),
        expression,
    )).map(|(name, args, _, body)| {
        FunctionDefinition { name, args, body }
    }).parse(input)
}

/// Precedence
/// * / (left-to-right, 5 / 3 / 3 = (5 / 3) / 3)
/// + - (left-to-right, 5 - 3 - 3 = (5 - 3) - 3)
///
/// Grammar:
/// expression: add_expression
/// add_expression: add_expression (add_op multiply_expression)?
/// multiply_expression: multiply_expression (multiply_op unary_prefix_expression)?
/// unary_prefix_expression: (unary_prefix_op)* call_expression
/// call_or_field_expression: unit_expression (call_or_field_tail)?
/// call_or_field_tail: call_tail     <-- function call
///                   | '.' call_tail <-- method call
///                   | ident         <-- component access
/// call_tail: '(' (expression (',' expression)? ','?)? ')'
/// unit_expression: '(' expression ')'
///                | variable
///                | literal

fn expression<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Expression<'src>, ParseError<'tok, 'src>> {
    add_expression(input)
}

fn add_expression<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Expression<'src>, ParseError<'tok, 'src>> {
    let add_op = alt((op(Op::Plus), op(Op::Minus)));
    let add_tail = tuple((add_op, multiply_expression));
    tuple((
        multiply_expression,
        many0(add_tail),
    )).map(|(mut lhs, tail)| {
        for (op, rhs) in tail {
            lhs = Expression::BinOp(lhs.into(), op, rhs.into())
        }
        lhs
    }).parse(input)
}

fn multiply_expression<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Expression<'src>, ParseError<'tok, 'src>> {
    let mul_op = alt((op(Op::Times), op(Op::Divide)));
    let mul_tail = tuple((mul_op, unary_prefix_expression));
    tuple((
        unary_prefix_expression,
        many0(mul_tail),
    )).map(|(mut lhs, tail)| {
        for (op, rhs) in tail {
            lhs = Expression::BinOp(lhs.into(), op, rhs.into())
        }
        lhs
    }).parse(input)
}

fn unary_prefix_expression<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Expression<'src>, ParseError<'tok, 'src>> {
    let unary_prefix_op = alt((op(Op::Minus), op(Op::Conjugate)));
    let unary_prefix_tail = call_or_field_expression;
    tuple((
        many0(unary_prefix_op),
        unary_prefix_tail,
    )).map(|(ops, mut tail)| {
        for op in ops {
            tail = Expression::UnaryOp(op, tail.into())
        }
        tail
    }).parse(input)
}

fn call_or_field_expression<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Expression<'src>, ParseError<'tok, 'src>> {
    enum CallOrFieldTail<'src> {
        CallTail(Vec<Expression<'src>>),
        MethodTail(Span<'src>, Vec<Expression<'src>>),
        Component(Component),
    }

    fn call_tail<'tok, 'src>(
        input: &'tok [Token<'src>],
    ) -> IResult<&'tok [Token<'src>], Vec<Expression<'src>>, ParseError<'tok, 'src>> {
        delimited(
            token_kind(TokenKind::LParen),
            opt(
                terminated(
                    separated_list1(token_kind(TokenKind::Comma), expression),
                    opt(token_kind(TokenKind::Comma)),
                )
            ),
            token_kind(TokenKind::RParen),
        ).map(
            Option::unwrap_or_default
        ).parse(input)
    }

    fn component<'tok, 'src>(
        input: &'tok [Token<'src>],
    ) -> IResult<&'tok [Token<'src>], Component, ParseError<'tok, 'src>> {
        map_res(
            preceded(
                token_kind(TokenKind::Period), ident
            ),
            |field| match *field.fragment() {
                "re" | "real" => Ok(Component::Real),
                "im" | "imag" => Ok(Component::Imag),
                _ => Err(ParseError::InvalidField(field)),
            }
        ).parse(input)
    }

    map_res(
        tuple((
            unit_expression,
            many0(alt((
                call_tail.map(CallOrFieldTail::CallTail),
                tuple((
                    preceded(
                        token_kind(TokenKind::Period), ident
                    ),
                    call_tail,
                )).map(|(method, args)| CallOrFieldTail::MethodTail(method, args)),
                component.map(CallOrFieldTail::Component))
            )),
        )),
        |(mut expr, tails)| {
            for tail in tails {
                expr = match tail {
                    CallOrFieldTail::CallTail(args) => {
                        match expr {
                            Expression::Variable(func) => Expression::FunctionCall(func, args),
                            _ => return Err(ParseError::InvalidFunction(expr)),
                        }
                    },
                    CallOrFieldTail::MethodTail(method, mut args) => {
                        args.insert(0, expr);
                        Expression::FunctionCall(method, args)
                    },
                    CallOrFieldTail::Component(component) => Expression::Component(expr.into(), component),
                };
            }
            Ok(expr)
        }
    ).parse(input)
}

fn unit_expression<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Expression<'src>, ParseError<'tok, 'src>> {
    alt((
        delimited(token_kind(TokenKind::LParen), expression, token_kind(TokenKind::RParen)),
        ident.map(|ident| Expression::Variable(ident)),
        literal.map(|literal| Expression::Literal(literal)),
    )).parse(input)
}

pub(super) fn parse_tokens<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> Result<FunctionDefinition<'src>, ParseError<'tok, 'src>> {
    match function_definition(input).finish() {
        Ok((&[], definition)) => Ok(definition),
        Ok((rest, _)) => Err(ParseError::ExtraInput(rest)),
        Err(err) => Err(err),
    }
}
