use nom::{IResult, multi::{separated_list0, many0}, sequence::{tuple, delimited, preceded}, Parser, Finish, branch::alt, combinator::{opt, map_res}};

use super::{
    lexer::{Token, TokenKind},
    Op, Component,
};

pub struct FunctionDefinition<'src> {
    pub(super) name: &'src str,
    pub(super) args: Vec<&'src str>,
    pub(super) body: Expression<'src>,
}

#[derive(Debug, Clone)]
pub(super) enum Expression<'src> {
    Literal(&'src str),
    Variable(&'src str),
    UnaryOp(Op, Box<Expression<'src>>),
    BinOp(Box<Expression<'src>>, Op, Box<Expression<'src>>),
    /// Used for .re and .im
    Component(Box<Expression<'src>>, Component),
}

#[derive(Debug)]
pub(super) enum ParseError<'tok, 'src> {
    Ident,
    Literal,
    Op(Op),
    TokenKind(TokenKind),
    InvalidField(&'src str),
    ExtraInput(&'tok [Token<'src>]),
    Other(nom::error::Error<&'tok [Token<'src>]>),
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
) -> IResult<&'tok [Token<'src>], &'src str, ParseError<'tok, 'src>> {
    match input.split_first() {
        Some(((TokenKind::Ident, ident), rest)) => Ok((rest, ident)),
        _ => Err(nom::Err::Error(
            ParseError::Ident,
        )),
    }
}

fn literal<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], &'src str, ParseError<'tok, 'src>> {
    match input.split_first() {
        Some(((TokenKind::Literal, literal), rest)) => Ok((rest, literal)),
        _ => Err(nom::Err::Error(
            ParseError::Literal,
        )),
    }
}

fn op(
    op: Op,
) -> impl for<'tok, 'src> FnMut(&'tok [Token<'src>]) -> IResult<&'tok [Token<'src>], Op, ParseError<'tok, 'src>>
{
    move |input| match input.split_first() {
        Some(((TokenKind::Op(found_op), _), rest)) if *found_op == op => Ok((rest, op)),
        _ => Err(nom::Err::Error(
            ParseError::Op(op)
        )),
    }
}

fn token_kind(
    kind: TokenKind,
) -> impl for<'tok, 'src> FnMut(&'tok [Token<'src>]) -> IResult<&'tok [Token<'src>], Token<'src>, ParseError<'tok, 'src>>
{
    move |input| match input.split_first() {
        Some((token, rest)) if token.0 == kind => Ok((rest, *token)),
        _ => Err(nom::Err::Error(
            ParseError::TokenKind(kind)
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
/// multiply_expression: multiply_expression (multiply_op unary_expression)?
/// unary_expression: (unary_op)* field_expression
/// field_expression: unit_expression ('.' ident)?
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
    let mul_tail = tuple((mul_op, unary_expression));
    tuple((
        unary_expression,
        many0(mul_tail),
    )).map(|(mut lhs, tail)| {
        for (op, rhs) in tail {
            lhs = Expression::BinOp(lhs.into(), op, rhs.into())
        }
        lhs
    }).parse(input)
}

fn unary_expression<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Expression<'src>, ParseError<'tok, 'src>> {
    let unary_op = alt((op(Op::Minus), op(Op::Conjugate)));
    let unary_tail = field_expression;
    tuple((
        many0(unary_op),
        unary_tail,
    )).map(|(ops, mut tail)| {
        for op in ops {
            tail = Expression::UnaryOp(op, tail.into())
        }
        tail
    }).parse(input)
}

fn field_expression<'tok, 'src>(
    input: &'tok [Token<'src>],
) -> IResult<&'tok [Token<'src>], Expression<'src>, ParseError<'tok, 'src>> {
    map_res(
        tuple((
            unit_expression,
            opt(preceded(
                token_kind(TokenKind::Period), ident
            )),
        )),
        |(expr, field)| {
            match field {
                Some("re") | Some("real") => Ok(Expression::Component(expr.into(), Component::Real)),
                Some("im") | Some("imag") => Ok(Expression::Component(expr.into(), Component::Imag)),
                None => Ok(expr),
                Some(field) => Err(ParseError::InvalidField(field))as Result<Expression, _>,
            }
        },
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
