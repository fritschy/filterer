use std::error::Error;
use std::fmt;
use std::num::ParseIntError;
use std::sync::Arc;

use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::character::complete::{multispace0, satisfy};
use nom::character::{is_alphanumeric, is_digit, is_hex_digit, is_oct_digit};
use nom::combinator::{eof, fail, map, opt, peek, recognize};
use nom::error::{context, ErrorKind};
use nom::number::complete::recognize_float;
use nom::sequence::{delimited, preceded};
use nom::{AsChar, Finish, IResult, Offset};

use crate::sema;

#[derive(Debug, Clone)]
pub(crate) enum Node {
    Unary {
        op: UnaryOp,
        expr: Arc<Node>,
    },
    Binary {
        lhs: Arc<Node>,
        op: BinaryOp,
        rhs: Arc<Node>,
    },
    Identifier(Arc<String>),
    IndexedIdentifier(Arc<String>, usize),
    ArrayIdentifierLen(Arc<String>),
    Constant(isize),
    StringLiteral(Arc<String>),
    Regexp(Arc<regex::Regex>),
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum UnaryOp {
    Not,     // !A
    Neg,     // -A
    Pos,     // +A
    NegBits, // ~A
}

// Do we need this.... now?
#[derive(Debug, Clone, Copy)]
pub(crate) enum BinaryOp {
    Eq, // A == B
    Ne, // A != B
    Gt, // A >  B
    Lt, // A <  B
    Ge, // A >= B
    Le, // A <= B

    Match, // A =~ B

    Band, // A & B
    Bor,  // A & B
    Xor,  // A ^ B

    And, // A && B
    Or,  // A || B
}

type Input<'a> = &'a str;

impl<'a> From<Input<'a>> for BinaryOp {
    fn from(i: Input) -> Self {
        match i {
            "==" => BinaryOp::Eq,
            "!=" => BinaryOp::Ne,
            ">=" => BinaryOp::Ge,
            "<=" => BinaryOp::Le,
            ">" => BinaryOp::Gt,
            "<" => BinaryOp::Lt,
            "=~" => BinaryOp::Match,
            "&" => BinaryOp::Band,
            "|" => BinaryOp::Bor,
            "^" => BinaryOp::Xor,
            "&&" => BinaryOp::And,
            "||" => BinaryOp::Or,
            _ => unreachable!("Unknown operator {}", i),
        }
    }
}

impl<'a> From<Input<'a>> for UnaryOp {
    fn from(i: Input) -> Self {
        match i {
            "!" => UnaryOp::Not,
            "-" => UnaryOp::Neg,
            "+" => UnaryOp::Pos, // just for completeness, might beuseful if expressions are generated
            "~" => UnaryOp::NegBits,
            _ => unreachable!("Unknown operator {}", i),
        }
    }
}

pub(crate) fn parse_num(i: &str) -> Result<isize, ParseIntError> {
    if let Some(i) = i.strip_prefix("0x") {
        isize::from_str_radix(i, 16)
    } else if let Some(i) = i.strip_prefix("0o") {
        isize::from_str_radix(i, 8)
    } else if let Some(i) = i.strip_prefix("0b") {
        isize::from_str_radix(i, 2)
    } else {
        i.parse::<isize>()
    }
}

impl Node {
    fn from_identifier(i: Input) -> Arc<Node> {
        Arc::new(Node::Identifier(Arc::new(i.to_string())))
    }

    fn from_indexed_identifier(ident: Input, index: usize) -> Arc<Node> {
        Arc::new(Node::IndexedIdentifier(Arc::new(ident.to_string()), index))
    }

    fn from_array_identifier_len(ident: Input) -> Arc<Node> {
        Arc::new(Node::ArrayIdentifierLen(Arc::new(ident.to_string())))
    }

    fn from_numeric(i: Input) -> Arc<Node> {
        if let Ok(num) = parse_num(i) {
            Arc::new(Node::Constant(num))
        } else {
            Arc::new(Node::Constant(0))
        }
    }

    fn from_string(i: Input) -> Arc<Node> {
        Arc::new(Node::StringLiteral(Arc::new(i.to_string())))
    }

    #[cfg(feature = "fuzz")]
    fn from_regexp(_: Input, _: bool) -> Arc<Node> {
        if let Ok(re) = regex::Regex::new("") {
            Arc::new(Node::Regexp(Arc::new(re)))
        } else {
            Arc::new(Node::Nil)
        }
    }

    #[cfg(not(feature = "fuzz"))]
    fn from_regexp(i: Input, icase: bool) -> Arc<Node> {
        if let Ok(re) = regex::RegexBuilder::new(i).case_insensitive(icase).build() {
            Arc::new(Node::Regexp(Arc::new(re)))
        } else {
            Arc::new(Node::Nil)
        }
    }

    fn new_binary(lhs: Arc<Node>, op: BinaryOp, rhs: Arc<Node>) -> Arc<Node> {
        Arc::new(Node::Binary { lhs, op, rhs })
    }

    fn new_unary(op: UnaryOp, expr: Arc<Node>) -> Arc<Node> {
        Arc::new(Node::Unary { op, expr })
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    input: String,
    pos: usize,
    msg: String,
}

impl Error for ParseError {}

impl ParseError {
    fn new(input: Input, pos: usize, msg: String) -> Self {
        Self {
            input: input.to_string(),
            pos,
            msg,
        }
    }

    pub fn describe(&self) -> String {
        format!("{}", self)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Parse Error: {}", self.msg)?;
        writeln!(f, "> {}", self.input)?;
        if self.pos != 0 {
            writeln!(f, "> {}^", " ".repeat(self.pos))?;
            writeln!(f, "> {}`--- this is wrong!", " ".repeat(self.pos))
        } else {
            writeln!(f, "> {}", "-".repeat(self.input.len()))
        }
    }
}

pub(crate) fn parse(i: Input) -> Result<Arc<Node>, ParseError> {
    match parse_expr(i) {
        Ok((_, o)) => {
            // FIXME: are transformations supposed to be run before analysis/checks?
            let o = sema::transform(&o);
            if let Err(serr) = sema::check(&o) {
                return Err(ParseError::new(i, 0, format!("Semantic error: {}", serr)));
            }
            Ok(o)
        }

        e => {
            // Do something ...
            let fe = e.finish().err().unwrap();
            let ei = &fe.input;
            let sei = ei as &str;
            let pos = i.as_bytes().offset(sei.as_bytes());
            Err(ParseError::new(i, pos, format!("Error at offset {}", pos)))
        }
    }
}

// parse_expr = { SOI ~ simple_expr ~ ws* ~ EOI }
fn parse_expr(i: Input) -> IResult<Input, Arc<Node>> {
    let (i, e) = simple_expr(0)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = eof(i)?;
    Ok((i, e))
}

// generic_expr = { ws* ~ nextp ~ (ws* ~ opp ~ generic_expr ~ ws*)? }
fn generic_expr<'a>(
    opp: &mut impl FnMut(Input) -> IResult<Input, BinaryOp>,
    nextp: &impl Fn(Input) -> IResult<Input, Arc<Node>>,
    i: &'a str,
) -> IResult<&'a str, Arc<Node>> {
    let (i, _) = multispace0(i)?;
    let (i, ae) = nextp(i)?;
    let (i, on) = opt(move |i| {
        let (i, _) = multispace0(i)?;
        let (i, op) = opp(i)?;
        let (i, se) = generic_expr(opp, nextp, i)?;
        Ok((i, (op, se)))
    })(i)?;

    let (i, _) = multispace0(i)?;

    if let Some((op, n)) = on {
        Ok((i, Node::new_binary(ae, op, n)))
    } else {
        Ok((i, ae))
    }
}

fn simple_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(tag("||"), BinaryOp::from)(i),
            &move |i| and_expr(d)(i),
            i,
        )
    }
}

fn and_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(tag("&&"), BinaryOp::from)(i),
            &move |i| rel_expr(d)(i),
            i,
        )
    }
}

fn rel_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(relop, BinaryOp::from)(i),
            &move |i| bor_expr(d)(i),
            i,
        )
    }
}

fn bor_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(tag("|"), BinaryOp::from)(i),
            &move |i| band_expr(d)(i),
            i,
        )
    }
}

fn band_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(tag("&"), BinaryOp::from)(i),
            &move |i| xor_expr(d)(i),
            i,
        )
    }
}

fn xor_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(tag("^"), BinaryOp::from)(i),
            &move |i| unary_expr(d)(i),
            i,
        )
    }
}

// unary_expr = { ws* ~ ("!" | "-" | "+" | "~")? ~ factor }
fn unary_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        let (i, _) = multispace0(i)?;
        let (i, op) = opt(unary_op)(i)?;
        let (i, ue) = factor(d)(i)?;

        if let Some(op) = op {
            Ok((i, Node::new_unary(op, ue)))
        } else {
            Ok((i, ue))
        }
    }
}

// factor = { ws* ~ (identifier | numeric | string | parens_expr) }
fn factor(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        let (i, _) = multispace0(i)?;
        alt((
            identifier,
            numeric,
            map(string("\""), Node::from_string),
            move |i| {
                let (i, re) = string("/")(i)?;
                let (i, fl) = opt(tag("i"))(i)?;
                Ok((i, Node::from_regexp(re, fl.is_some())))
            },
            parens_expr(d),
        ))(i)
    }
}

pub(crate) fn string<'a>(delimiter: Input<'a>) -> impl FnMut(Input<'a>) -> IResult<Input, Input> {
    delimited(
        tag(delimiter),
        recognize(move |i| {
            let mut i = i;
            while let Ok((r, _)) = alt::<_, _, nom::error::Error<Input>, _>((
                escaped_char(delimiter),
                take_till1(|x| x == '\\' || x as u8 == delimiter.as_bytes()[0]),
            ))(i)
            {
                i = r;
            }
            Ok((i, ""))
        }),
        tag(delimiter),
    )
}

// escaped_char = { "\\" ~ ("\"" | "\\" | "n" | "r" ...) }
fn escaped_char(delimiter: Input) -> impl Fn(Input) -> IResult<Input, Input> + '_ {
    move |i| {
        preceded(
            tag("\\"),
            map(
                alt((tag(delimiter), tag("n"), tag("r"), tag("0"), tag("t"))),
                move |c| {
                    if c == delimiter {
                        return c;
                    }

                    match c {
                        "n" => "\n",
                        "r" => "\r",
                        "0" => "\0",
                        "t" => "\t",
                        _ => unreachable!(),
                    }
                },
            ),
        )(i)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escaped_char_test() {
        let input = "\\/";
        assert!(escaped_char("/")(input) == Ok(("", "/")));

        let input = "\\/a";
        assert!(escaped_char("/")(input) == Ok(("a", "/")));
    }
}

// parens_expr = { ws* ~ "(" ~ simple_expr ~ ")" }
fn parens_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Arc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        let (i, _) = multispace0(i)?;
        delimited(tag("("), simple_expr(d), tag(")"))(i)
    }
}

fn unary_op(i: Input) -> IResult<Input, UnaryOp> {
    let (i, _) = multispace0(i)?;
    map(alt((tag("-"), tag("!"), tag("+"), tag("~"))), UnaryOp::from)(i)
}

fn relop(i: Input) -> IResult<Input, BinaryOp> {
    let (i, _) = multispace0(i)?;
    map(
        alt((
            tag("=="),
            tag("!="),
            tag(">="),
            tag(">"),
            tag("<="),
            tag("<"),
            tag("=~"),
        )),
        BinaryOp::from,
    )(i)
}

fn identifier(i: Input) -> IResult<Input, Arc<Node>> {
    let (i, _) = multispace0(i)?;
    let (i, _) = peek(satisfy(|c| c.is_alpha() || c == '_'))(i)?;
    let (i, ident) = take_till1(|c| !(is_alphanumeric(c as u8) || c == '_'))(i)?;
    let (i, _) = multispace0(i)?;
    if let Ok((i, index)) = delimited(
        tag("["),
        context("parse number", alt((hexnum, octnum, binnum, decnum))),
        tag("]"),
    )(i)
    {
        let num = parse_num(index)
            .map_err(|_| nom::Err::Failure(nom::error::Error::new(i, ErrorKind::Digit)))?;
        return Ok((i, Node::from_indexed_identifier(ident, num as usize)));
    } else if let Ok((i, _)) = dot_len(i) {
        return Ok((i, Node::from_array_identifier_len(ident)));
    }
    Ok((i, Node::from_identifier(ident)))
}

fn dot_len(i: Input) -> IResult<Input, ()> {
    let (i, _) = multispace0(i)?;
    let (i, _) = tag(".")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = context("parse array len", tag("len"))(i)?;
    Ok((i, ()))
}

fn numeric(i: Input) -> IResult<Input, Arc<Node>> {
    let (i, _) = multispace0(i)?;
    let (i, n) = map(
        alt((hexnum, octnum, binnum, recognize_float)),
        Node::from_numeric,
    )(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, n))
}

fn prefixed_num<'a>(
    pfx: Option<&'_ str>,
    is_digit: impl Fn(u8) -> bool,
    i: &'a str,
) -> IResult<&'a str, &'a str> {
    let (i, _) = multispace0(i)?;
    recognize(|i| {
        let (i, _) = if let Some(pfx) = pfx {
            tag(pfx)(i)?
        } else {
            (i, "")
        };
        take_till1(|x| !is_digit(x as u8))(i)
    })(i)
}

fn decnum(i: Input) -> IResult<Input, Input> {
    prefixed_num(None, is_digit, i)
}

fn hexnum(i: Input) -> IResult<Input, Input> {
    prefixed_num(Some("0x"), is_hex_digit, i)
}

fn octnum(i: Input) -> IResult<Input, Input> {
    prefixed_num(Some("0o"), is_oct_digit, i)
}

fn binnum(i: Input) -> IResult<Input, Input> {
    prefixed_num(Some("0b"), |x| x == b'0' || x == b'1', i)
}

fn depth(i: Input, d: usize) -> Result<usize, nom::Err<nom::error::Error<Input>>> {
    if d > 1000 {
        // this should be plenty!
        context("recursion depth", fail::<Input, usize, _>)(i)?;
    }
    Ok(d + 1)
}
