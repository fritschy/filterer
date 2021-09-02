// Examples:
// ctx =~ "AP.*" || ctx == "MAP"             # match ctx against re, or ctx equals to "MAP"
// app == "REND" && payload =~ ".*error.*"   # app == "REND" AND payload contains "error"
//
//
// IDENTIFIERS can be defined freely by first "declaring" them when configuring
// the filter, i.e. filter.add_define("app", |msg| msg.app) # where msg is something
// that makes sense in the context.
//
// A parsed filter should result in an AST that can be evaluated, don't particularly
// concern yourself with efficiency, this is a first test.
//
// How to implement chaining of logic operators, i.e. EXPR && EXPR?

pub mod nom_parser {
    use std::fmt::Debug;

    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_till1};
    use nom::character::complete::{multispace0, satisfy};
    use nom::character::{is_alphanumeric, is_hex_digit, is_oct_digit};
    use nom::combinator::{eof, map, peek, recognize};
    use nom::number::complete::recognize_float;
    use nom::sequence::{delimited, preceded};
    use nom::{AsChar, IResult};
    use tracing::trace;

    use Node::{Binary, Unary};
    use nom::multi::{fold_many0};
    use crate::nom_parser::Node::List;

    #[derive(Debug, Clone)]
    pub enum Node {
        Unary {
            op: UnaryOp,
            expr: Box<Node>,
        },
        Binary {
            lhs: Box<Node>,
            op: BinaryOp,
            rhs: Box<Node>,
        },
        List {
            lst: Vec<Box<Node>>,
            op: BinaryOp,
        },
        Identifier(String),
        Constant(String),
        StringLiteral(String),
    }

    #[derive(Debug, Clone)]
    pub enum UnaryOp {
        Not, // !A
        Neg, // -A
    }

    // Do we need this.... now?
    #[derive(Debug, Clone)]
    pub enum BinaryOp {
        Eq, // A == B
        Ne, // A != B
        Gt, // A >  B
        Lt, // A <  B
        Ge, // A >= B
        Le, // A <= B

        Match, // A =~ B

        Band, // A &  B
        Bor,  // A |  B

        And, // A && B
        Or,  // A || B
    }

    // A pest.rs grammar,
    // Couple of notes though:
    // * WITESPACE should ne done explicitly
    // * string parsing is missin
    // * octal and binary numerals are missing
    //
    // expr = { SOI ~ simpleExpr ~ EOI }
    // simpleExpr = { andExpr ~ or ~ simpleExpr | andExpr }
    // andExpr = { relExpr ~ and ~ andExpr | relExpr }
    // relExpr = { sumExpr ~ relop ~ relExpr | sumExpr }
    // sumExpr = { unaryExpr ~ sumop ~ sumExpr | unaryExpr }
    // relop = { "==" | "!=" | ">=" | "<=" | ">" | "<" | "=~" }
    // sumop = { "&" }
    // unaryExpr = { unaryOp ~ unaryExpr | factor }
    // unaryOp = { "!" | "-" }
    // factor = { identifier | numeral | parensExpr }
    // parensExpr = { "(" ~ expr ~ ")" }
    // identifier = @{ name }
    // nameHead = { ASCII_ALPHA | "_" }
    // nameTail = { ASCII_ALPHANUMERIC | "_" }
    // name = { nameHead ~ nameTail* | nameHead+ }
    // numeral = @{ ("0x" ~ ASCII_HEX_DIGIT+) | ASCII_DIGIT+ }
    // or = { "||" }
    // and = { "&&" }
    // WHITESPACE = _{ " " | "\t" }

    impl From<&str> for BinaryOp {
        fn from(i: &str) -> Self {
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
                "&&" => BinaryOp::And,
                "||" => BinaryOp::Or,
                _ => panic!("Unknown operator {}", i),
            }
        }
    }

    impl From<&str> for UnaryOp {
        fn from(i: &str) -> Self {
            match i {
                "!" => UnaryOp::Not,
                "-" => UnaryOp::Neg,
                _ => panic!("Unaknown operator {}", i),
            }
        }
    }

    impl Node {
        fn from_identifier(i: &str) -> Box<Node> {
            Box::new(Node::Identifier(i.to_string()))
        }

        fn from_numeric(i: &str) -> Box<Node> {
            Box::new(Node::Constant(i.to_string()))
        }

        fn from_string(i: &str) -> Box<Node> {
            Box::new(Node::StringLiteral(i.to_string()))
        }

        fn from_list(i: Node) -> Box<Node> {
            Box::new(i)
        }

        fn push(&mut self, i: Box<Node>) {
            match self {
                List { ref mut lst, op: _ } => lst.push(i),
                _ => unreachable!(),
            }
        }

        fn is_empty(&self) -> bool {
            match self {
                List { ref lst, op: _ } => lst.is_empty(),
                _ => unreachable!(),
            }
        }

        fn len(&self) -> usize {
            match self {
                List { ref lst, op: _ } => lst.len(),
                _ => 0,
            }
        }

        fn unwrap(&self) -> Box<Node> {
            match self {
                List { ref lst, op: _ } => {
                    if lst.len() == 1 {
                        return lst[0].clone();
                    } else {
                        panic!("Cannot unwrap a list with len > 1");
                    }
                }
                _ => Box::new(self.clone()),
            }
        }
    }

    pub fn parse(i: &str) -> IResult<&str, Box<Node>> {
        trace!("parse: i={}", i);
        let (i, e) = simple_expr(i)?;
        let (i, _) = eof(i)?;
        Ok((i, e))
    }

    fn simple_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("simple_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, ae) = and_expr(i)?;
        let (i, l) = fold_many0(
            move |i| {
                let (i, _) = multispace0(i)?;
                let (i, _) = map(tag("||"), BinaryOp::from)(i)?;
                simple_expr(i)
            },
            || List { op: BinaryOp::from("||"), lst: Vec::new() },
            |mut acc, x| {
                acc.push(x);
                acc
            }
        )(i)?;

        if l.is_empty() {
            Ok((i, ae))
        } else if l.len() == 1 {
            Ok((i, Box::new(Binary { lhs: ae, op: BinaryOp::from("||"), rhs: l.unwrap() })))
        } else {
            Ok((i, Box::new(Binary { lhs: ae, op: BinaryOp::from("||"), rhs: Node::from_list(l) })))
        }
    }

    fn and_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("and_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, re) = rel_expr(i)?;
        let (i, l) = fold_many0(
            move |i| {
                let (i, _) = multispace0(i)?;
                let (i, _) = map(tag("&&"), BinaryOp::from)(i)?;
                and_expr(i)
            },
            || List { op: BinaryOp::from("&&"), lst: Vec::new() },
            |mut acc, x| {
                acc.push(x);
                acc
            }
        )(i)?;

        if l.is_empty() {
            Ok((i, re))
        } else if l.len() == 1 {
            Ok((i, Box::new(Binary { lhs: re, op: BinaryOp::from("&&"), rhs: l.unwrap() })))
        } else {
            Ok((i, Box::new(Binary { lhs: re, op: BinaryOp::from("&&"), rhs: Node::from_list(l) })))
        }
    }

    fn rel_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("rel_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        alt((
            move |i| {
                let (i, se) = sum_expr(i)?;
                let (i, _) = multispace0(i)?;
                let (i, op) = map(relop, BinaryOp::from)(i)?;
                let (i, re) = rel_expr(i)?;
                Ok((i, Box::new(Binary { lhs: se, op, rhs: re, }), ))
            },
            sum_expr,
        ))(i)
    }

    fn sum_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("sum_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, ue) = unary_expr(i)?;
        let (i, l) = fold_many0(
            move |i| {
                let (i, _) = multispace0(i)?;
                let (i, _) = map(tag("&"), BinaryOp::from)(i)?;
                sum_expr(i)
            },
            || List { op: BinaryOp::from("&"), lst: Vec::new() },
            |mut acc, x| {
                acc.push(x);
                acc
            }
        )(i)?;

        if l.is_empty() {
            Ok((i, ue))
        } else if l.len() == 1 {
            Ok((i, Box::new(Binary { lhs: ue, op: BinaryOp::from("&"), rhs: l.unwrap() })))
        } else {
            Ok((i, Box::new(Binary { lhs: ue, op: BinaryOp::from("&"), rhs: Node::from_list(l) })))
        }
    }

    fn unary_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("unary_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        alt((
            move |i| {
                let (i, op) = alt((tag("!"), tag("-")))(i)?;
                let (i, ue) = unary_expr(i)?;
                Ok((i, Box::new(Unary { expr: ue, op: UnaryOp::from(op), }), ))
            },
            factor,
        ))(i)
    }

    fn factor(i: &str) -> IResult<&str, Box<Node>> {
        trace!("factor: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, f) = alt((identifier, numeric, string, parens_expr))(i)?;
        Ok((i, f))
    }

    // FIXME: this one sucks particularly HARD
    fn string(i: &str) -> IResult<&str, Box<Node>> {
        trace!("string: i={}", i);
        let (i, s) = delimited(
            tag("\""),
            recognize(move |i| {
                let mut i = i;
                loop {
                    if let Ok((r, _)) = alt::<_, _, nom::error::Error<&str>, _>((
                        escaped_char,
                        take_till1(|x| x == '\\' || x == '"'),
                    ))(i)
                    {
                        i = r;
                    } else {
                        break;
                    }
                }
                Ok((i, ""))
            }),
            tag("\""),
        )(i)?;
        Ok((i, Node::from_string(s)))
    }

    fn escaped_char(i: &str) -> IResult<&str, &str> {
        trace!("escaped_char: i={}", i);
        preceded(
            tag("\\"),
            alt((
                tag("\""),
                tag("\\"),
                tag("n"),
                tag("r"),
                tag("0"),
                tag("f"),
                tag("b"),
                tag("t"),
                tag("v"),
            )),
        )(i)
    }

    fn parens_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("parens_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        delimited(tag("("), simple_expr, tag(")"))(i)
    }

    fn relop(i: &str) -> IResult<&str, BinaryOp> {
        trace!("relop: i={}", i);
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

    fn identifier(i: &str) -> IResult<&str, Box<Node>> {
        trace!("identifier: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, _) = peek(satisfy(|c| c.is_alpha() || c == '_'))(i)?;
        let (i, ident) = take_till1(|c| !(is_alphanumeric(c as u8) || c == '_'))(i)?;
        let (i, _) = multispace0(i)?;
        Ok((i, Node::from_identifier(ident)))
    }

    fn numeric(i: &str) -> IResult<&str, Box<Node>> {
        trace!("numeric: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, num) = alt((hexnum, octnum, binnum, recognize_float))(i)?;
        Ok((i, Node::from_numeric(num)))
    }

    fn hexnum(i: &str) -> IResult<&str, &str> {
        trace!("hexnum: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, num) = recognize(|i| {
            trace!("recognize_hex: i={}", i);
            let (i, _) = tag("0x")(i)?;
            take_till1(|x| !is_hex_digit(x as u8))(i)
        })(i)?;
        Ok((i, num))
    }

    fn octnum(i: &str) -> IResult<&str, &str> {
        trace!("octnum: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, num) = recognize(|i| {
            trace!("recognize_hex: i={}", i);
            let (i, _) = tag("0o")(i)?;
            take_till1(|x| !is_oct_digit(x as u8))(i)
        })(i)?;
        Ok((i, num))
    }

    fn binnum(i: &str) -> IResult<&str, &str> {
        trace!("binnum: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, num) = recognize(|i| {
            trace!("recognize_hex: i={}", i);
            let (i, _) = tag("0b")(i)?;
            take_till1(|x| x != '0' && x != '1')(i)
        })(i)?;
        Ok((i, num))
    }
}

pub mod pest_parser {
    #[derive(pest_derive::Parser, Debug, Clone)]
    #[grammar = "filter.pest"]
    pub struct Filter;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
