use std::collections::HashMap;
use std::fmt::Debug;
use std::marker::PhantomData;

use nom::branch::alt;
use nom::bytes::complete::take_till1;
use nom::character::complete::{multispace0, satisfy};
use nom::character::{is_alphanumeric, is_hex_digit};
use nom::combinator::{fail, peek, recognize};
use nom::error::ParseError;
use nom::number::complete::recognize_float;
use nom::{bytes::complete::tag, AsChar, IResult};
use tracing::{error as log_error, Level};
use tracing::{info, trace};

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

pub mod ast {
    use nom::combinator::{map, eof};
    use nom::sequence::delimited;

    use crate::ast::Node::{Binary, Constant, Identifier, Unary};

    use super::*;

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
        Identifier(String),
        Constant(String),
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
        Bor, // A |  B

        And, // A && B
        Or, // A || B
    }

    // A pest.rs grammar
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
    //
    // Example: a_1 & 0x10a != 0 || (ts > 1000 && ts < 99999)
    // - simpleExpr
    //   - andExpr > relExpr
    //     - sumExpr
    //       - unaryExpr > factor > identifier: "a_1"
    //       - sumop: "&"
    //       - sumExpr > unaryExpr > factor > numeral: "0x10a"
    //     - relop: "!="
    //     - relExpr > sumExpr > unaryExpr > factor > numeral: "0"
    //   - or: "||"
    //   - simpleExpr > andExpr > relExpr > sumExpr > unaryExpr > factor > parensExpr > expr > simpleExpr > andExpr
    //     - relExpr
    //       - sumExpr > unaryExpr > factor > identifier: "ts"
    //       - relop: ">"
    //       - relExpr > sumExpr > unaryExpr > factor > numeral: "1000"
    //     - and: "&&"
    //     - andExpr > relExpr
    //       - sumExpr > unaryExpr > factor > identifier: "ts"
    //       - relop: "<"
    //       - relExpr > sumExpr > unaryExpr > factor > numeral: "99999"

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
    }

    // What we want:
    // A & 0x100 != 0    ->    Binary{Binary{'A', Band, 0x100}, Ne, 0}

    pub fn parse(i: &str) -> IResult<&str, Box<Node>> {
        trace!("parse: i={}", i);
        let (i, e) = expr(i)?;
        let (i, _) = eof(i)?;
        Ok((i, e))
    }

    fn expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("expr: i={}", i);
        simple_expr(i)
    }

    fn simple_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("simple_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        alt((
            move |i| {
                let (i, ae) = and_expr(i)?;
                let (i, _) = multispace0(i)?;
                let (i, op) = map(tag("||"), BinaryOp::from)(i)?;
                let (i, se) = simple_expr(i)?;
                Ok((i, Box::new(Binary { lhs: ae, op, rhs: se, }), ))
            },
            and_expr,
        ))(i)
    }

    fn and_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("and_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        alt((
            move |i| {
                let (i, re) = rel_expr(i)?;
                let (i, _) = multispace0(i)?;
                let (i, op) = map(tag("&&"), BinaryOp::from)(i)?;
                let (i, ae) = and_expr(i)?;
                Ok((i, Box::new(Binary { lhs: re, op, rhs: ae, }), ))
            },
            rel_expr,
        ))(i)
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
        alt((
            move |i| {
                let (i, ue) = unary_expr(i)?;
                let (i, _) = multispace0(i)?;
                let (i, op) = map(tag("&"), BinaryOp::from)(i)?;
                let (i, se) = sum_expr(i)?;
                Ok((i, Box::new(Binary { lhs: ue, op, rhs: se, }), ))
            },
            unary_expr,
        ))(i)
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
        let (i, f) = alt((identifier, numeric, parens_expr))(i)?;
        Ok((i, f))
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
        Ok((i, Box::new(Identifier(ident.to_string()))))
    }

    fn numeric(i: &str) -> IResult<&str, Box<Node>> {
        trace!("numeric: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, num) = alt((hexnum, recognize_float))(i)?;
        Ok((i, Box::new(Constant(num.to_string()))))
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
