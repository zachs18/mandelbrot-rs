use nom::{
    branch::alt,
    bytes::complete::{tag, is_a},
    character::complete::{alpha1, alphanumeric1, multispace0},
    combinator::{map, recognize, opt},
    multi::{many0, many0_count, many1_count},
    sequence::{delimited, pair, tuple},
    IResult, Parser,
};

use super::{Op, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum TokenKind {
    Op(Op),
    LParen,
    RParen,
    Comma,
    Period,
    Ident,
    Literal,
}

pub(super) type Token<'src> = (TokenKind, Span<'src>);

macro_rules! token_fn {
    ($fn_name:ident: $value:literal => $token_kind:expr) => {
        fn $fn_name(input: Span) -> IResult<Span, Token> {
            map(tag($value), |span| ($token_kind, span))(input)
        }
    };
}

token_fn!(op_assign: "=" => TokenKind::Op(Op::Assign));
token_fn!(op_plus: "+" => TokenKind::Op(Op::Plus));
token_fn!(op_minus: "-" => TokenKind::Op(Op::Minus));
token_fn!(op_times: "*" => TokenKind::Op(Op::Times));
token_fn!(op_divide: "/" => TokenKind::Op(Op::Divide));
token_fn!(op_conjugate: "~" => TokenKind::Op(Op::Conjugate));

fn op(input: Span) -> IResult<Span, Token> {
    alt((op_assign, op_plus, op_minus, op_times, op_divide, op_conjugate))(input)
}

token_fn!(punct_lparen: "(" => TokenKind::LParen);
token_fn!(punct_rparen: ")" => TokenKind::RParen);
token_fn!(punct_comma: "," => TokenKind::Comma);
token_fn!(punct_period: "." => TokenKind::Period);

fn punct(input: Span) -> IResult<Span, Token> {
    alt((punct_lparen, punct_rparen, punct_comma, punct_period))(input)
}

fn ident(input: Span) -> IResult<Span, Token> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    )).map(
        |span| (TokenKind::Ident, span),
    ).parse(input)
}

fn literal(input: Span) -> IResult<Span, Token> {
    recognize(tuple((
        many1_count(is_a("0123456789")),
        opt(tuple((
            tag("."),
            many1_count(is_a("0123456789")),
            opt(
                tuple((
                    is_a("eE"),
                    opt(is_a("-+")),
                    many1_count(is_a("0123456789")),
                ))
            ),
        ))),
        opt(tag("i")),
    ))).map(|span| {
        (TokenKind::Literal, span)
    }).parse(input)
}

fn token(input: Span) -> IResult<Span, Token> {
    alt((op, punct, ident, literal))(input)
}

fn lex_impl(input: Span) -> IResult<Span, Vec<Token>> {
    many0(delimited(multispace0, token, multispace0))(input)
}

#[derive(Debug)]
pub(super) enum LexError<'src> {
    ExtraInput(Span<'src>),
    Other(nom::Err<nom::error::Error<Span<'src>>>),
}

impl<'src> LexError<'src> {
    pub(super) fn explanation(&self) -> &'static str {
        match self {
            LexError::ExtraInput(_) => "Extra input",
            LexError::Other(_) => "Lexer error",
        }
    }
    
    pub fn span(&self) -> Option<Span<'src>> {
        match *self {
            LexError::ExtraInput(span) => Some(span),
            LexError::Other(nom::Err::Incomplete(_)) => None,
            LexError::Other(nom::Err::Error(ref err)) |
            LexError::Other(nom::Err::Failure(ref err)) => {
                Some(err.input)
            },
        }
    }
}

pub(super) fn lex(input: Span) -> Result<Vec<Token>, LexError> {
    match lex_impl(input) {
        Ok((rest, tokens)) if rest.fragment().len() == 0 => Ok(tokens),
        Ok((rest, _)) => Err(LexError::ExtraInput(rest)),
        Err(err) => Err(LexError::Other(err)),
    }
}
