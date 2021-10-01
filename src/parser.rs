use std::fmt;
use std::num::ParseIntError;
use std::rc::Rc;

use nom::{AsChar, Finish, IResult, Offset};
use nom::branch::alt;
use nom::bytes::complete::{tag, take_till1};
use nom::character::{is_alphanumeric, is_digit, is_hex_digit, is_oct_digit};
use nom::character::complete::{multispace0, satisfy};
use nom::combinator::{eof, map, opt, peek, recognize, fail};
use nom::number::complete::recognize_float;
use nom::sequence::{delimited, preceded};

use crate::sema;
use nom::error::{ErrorKind};

#[derive(Debug, Clone)]
pub enum Node {
    Unary {
        op: UnaryOp,
        expr: Rc<Node>,
    },
    Binary {
        lhs: Rc<Node>,
        op: BinaryOp,
        rhs: Rc<Node>,
    },
    Identifier(Rc<String>),
    IndexedIdentifier(Rc<String>, usize),
    ArrayIdentifierLen(Rc<String>),
    Constant(isize),
    StringLiteral(Rc<String>),
    Regexp(Rc<regex::Regex>),
    Nil,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Not, // !A
}

// Do we need this.... now?
#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Eq, // A == B
    Ne, // A != B
    Gt, // A >  B
    Lt, // A <  B
    Ge, // A >= B
    Le, // A <= B

    Match, // A =~ B

    Band, // A &  B

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
            _ => unreachable!("Unknown operator {}", i),
        }
    }
}

pub fn parse_num(i: &str) -> Result<isize, ParseIntError> {
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
    fn from_identifier(i: Input) -> Rc<Node> {
        Rc::new(Node::Identifier(Rc::new(i.to_string())))
    }

    fn from_indexed_identifier(ident: Input, index: usize) -> Rc<Node> {
        Rc::new(Node::IndexedIdentifier(Rc::new(ident.to_string()), index))
    }

    fn from_array_identifier_len(ident: Input) -> Rc<Node> {
        Rc::new(Node::ArrayIdentifierLen(Rc::new(ident.to_string())))
    }

    fn from_numeric(i: Input) -> Rc<Node> {
        if let Ok(num) = parse_num(i) {
            Rc::new(Node::Constant(num))
        } else {
            Rc::new(Node::Constant(0))
        }
    }

    fn from_string(i: Input) -> Rc<Node> {
        Rc::new(Node::StringLiteral(Rc::new(i.to_string())))
    }

    fn from_regexp(i: Input) -> Rc<Node> {
        if let Ok(re) = regex::Regex::new(i) {
            Rc::new(Node::Regexp(Rc::new(re)))
        } else {
            Rc::new(Node::Nil)
        }
    }

    fn new_binary(lhs: Rc<Node>, op: BinaryOp, rhs: Rc<Node>) -> Rc<Node> {
        Rc::new(Node::Binary { lhs, op, rhs })
    }

    fn new_unary(op: UnaryOp, expr: Rc<Node>) -> Rc<Node> {
        Rc::new(Node::Unary { op, expr })
    }
}

#[derive(Debug)]
pub struct ParseError<'a> {
    pub input: Input<'a>,
    pub pos: usize,
    pub msg: String,
}

impl<'a> ParseError<'a> {
    fn new(input: Input<'a>, pos: usize, msg: String) -> Self {
        Self { input, pos, msg }
    }

    pub fn describe(&self) -> String {
        format!("{}", self)
    }
}

impl<'a> fmt::Display for ParseError<'a> {
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

pub fn parse(i: Input) -> Result<Rc<Node>, ParseError> {
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
            let fe = e.finish();
            match &fe {
                Err(e) => {
                    let ei = &e.input;
                    let sei = ei as &str;
                    let pos = i.as_bytes().offset(sei.as_bytes());
                    return Err(ParseError::new(i, pos, format!("Error at offset {}", pos)));
                }
                _ => unreachable!(),
            }
        }
    }
}

// parse_expr = { SOI ~ simple_expr ~ ws* ~ EOI }
fn parse_expr(i: Input) -> IResult<Input, Rc<Node>> {
    let (i, e) = simple_expr(0)(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = eof(i)?;
    Ok((i, e))
}

// generic_expr = { ws* ~ nextp ~ (ws* ~ opp ~ generic_expr ~ ws*)? }
fn generic_expr<'a>(
    opp: &mut impl FnMut(Input) -> IResult<Input, BinaryOp>,
    nextp: &impl Fn(Input) -> IResult<Input, Rc<Node>>,
    i: &'a str,
) -> IResult<&'a str, Rc<Node>> {
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

fn simple_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Rc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(tag("||"), BinaryOp::from)(i),
            &move |i| and_expr(d)(i),
            i,
        )
    }
}

fn and_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Rc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(tag("&&"), BinaryOp::from)(i),
            &move |i| rel_expr(d)(i),
            i,
        )
    }
}

fn rel_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Rc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(relop, BinaryOp::from)(i),
            &move |i| sum_expr(d)(i),
            i,
        )
    }
}

fn sum_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Rc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        generic_expr(
            &mut move |i| map(tag("&"), BinaryOp::from)(i),
            &move |i| unary_expr(d)(i),
            i,
        )
    }
}

// unary_expr = { ws* ~ ("!" | "-")? ~ factor }
fn unary_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Rc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        let (i, _) = multispace0(i)?;
        let (i, op) = opt(map(tag("!"), UnaryOp::from))(i)?;
        let (i, ue) = factor(d)(i)?;

        if let Some(op) = op {
            Ok((i, Node::new_unary(op, ue)))
        } else {
            Ok((i, ue))
        }
    }
}

// factor = { ws* ~ (identifier | numeric | string | parens_expr) }
fn factor(d: usize) -> impl Fn(Input) -> IResult<Input, Rc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        let (i, _) = multispace0(i)?;
        alt((
            identifier,
            numeric,
            string("\"", &Node::from_string),
            string("/", &Node::from_regexp),
            parens_expr(d),
        ))(i)
    }
}

// FIXME: this one sucks particularly HARD
fn string<'a>(
    delimiter: Input<'a>,
    map_to: &'a impl Fn(Input) -> Rc<Node>,
) -> impl Fn(Input) -> IResult<Input, Rc<Node>> + 'a {
    move |i| {
        map(
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
            ),
            map_to,
        )(i)
    }
}

// escaped_char = { "\\" ~ ("\"" | "\\" | "n" | "r" ...) }
fn escaped_char(delimiter: Input) -> impl Fn(Input) -> IResult<Input, Input> + '_ {
    move |i| {
        preceded(
            tag("\\"),
            map(alt((
                tag(delimiter),
                tag("n"),
                tag("r"),
                tag("0"),
                tag("t"),
            )), move |c| {
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
            }),
        )(i)
    }
}

// parens_expr = { ws* ~ "(" ~ simple_expr ~ ")" }
fn parens_expr(d: usize) -> impl Fn(Input) -> IResult<Input, Rc<Node>> {
    move |i| {
        let d = depth(i, d)?;
        let (i, _) = multispace0(i)?;
        delimited(tag("("), simple_expr(d), tag(")"))(i)
    }
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

fn identifier(i: Input) -> IResult<Input, Rc<Node>> {
    let (i, _) = multispace0(i)?;
    let (i, _) = peek(satisfy(|c| c.is_alpha() || c == '_'))(i)?;
    let (i, ident) = take_till1(|c| !(is_alphanumeric(c as u8) || c == '_'))(i)?;
    let (i, _) = multispace0(i)?;
    if let Ok((i, index)) = delimited(tag("["), alt((hexnum, octnum, binnum, decnum)), tag("]"))(i) {
        let num = parse_num(index).map_err(|_| nom::Err::Error(nom::error::Error::new(i, ErrorKind::Digit)))?;
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
    let (i, _) = tag("len")(i)?;
    Ok((i, ()))
}

fn numeric(i: Input) -> IResult<Input, Rc<Node>> {
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
        let (i, _) = if let Some(pfx) = pfx { tag(pfx)(i)? } else { (i, "") };
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
    if d > 200 { // this should be plenty!
        fail::<Input, usize, _>(i)?;
    }
    Ok(d+1)
}
