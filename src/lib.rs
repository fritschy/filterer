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
    use nom::combinator::{eof, map, peek, recognize, opt};
    use nom::number::complete::recognize_float;
    use nom::sequence::{delimited, preceded};
    use nom::{AsChar, IResult};
    use tracing::trace;

    use Node::{Binary, Unary};

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
                _ => unreachable!("Unknown operator {}", i),
            }
        }
    }

    impl From<&str> for UnaryOp {
        fn from(i: &str) -> Self {
            match i {
                "!" => UnaryOp::Not,
                "-" => UnaryOp::Neg,
                _ => unreachable!("Unaknown operator {}", i),
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
        let (i, on) = opt(
            move |i| {
                let (i, _) = multispace0(i)?;
                let (i, op) = map(tag("||"), BinaryOp::from)(i)?;
                let (i, se) = simple_expr(i)?;
                Ok((i, (op, se)))
            }
        )(i)?;

        if let Some((op, n)) = on {
            Ok((i, Box::new(Binary { lhs: ae, op, rhs: n })))
        } else {
            Ok((i, ae))
        }
    }

    fn and_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("and_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, re) = rel_expr(i)?;
        let (i, on) = opt(
            move |i| {
                let (i, _) = multispace0(i)?;
                let (i, op) = map(tag("&&"), BinaryOp::from)(i)?;
                let (i, ae) = and_expr(i)?;
                Ok((i, (op, ae)))
            }
        )(i)?;

        if let Some((op, n)) = on {
            Ok((i, Box::new(Binary { lhs: re, op, rhs: n })))
        } else {
            Ok((i, re))
        }
    }

    fn rel_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("rel_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, se) = sum_expr(i)?;
        let (i, on) = opt(
            move |i| {
                let (i, _) = multispace0(i)?;
                let (i, op) = map(relop, BinaryOp::from)(i)?;
                let (i, re) = rel_expr(i)?;
                Ok((i, (op, re)))
            }
        )(i)?;

        if let Some((op, n)) = on {
            Ok((i, Box::new(Binary { lhs: se, op, rhs: n })))
        } else {
            Ok((i, se))
        }
    }

    fn sum_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("sum_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, ue) = unary_expr(i)?;
        let (i, on) = opt(
            move |i| {
                let (i, _) = multispace0(i)?;
                let (i, op) = map(tag("&"), BinaryOp::from)(i)?;
                let (i, se) = sum_expr(i)?;
                Ok((i, (op, se)))
            }
        )(i)?;

        if let Some((op, n)) = on {
            Ok((i, Box::new(Binary { lhs: ue, op, rhs: n })))
        } else {
            Ok((i, ue))
        }
    }

    fn unary_expr(i: &str) -> IResult<&str, Box<Node>> {
        trace!("unary_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, op) = opt(map(alt((tag("!"), tag("-"))), UnaryOp::from))(i)?;
        let (i, ue) = factor(i)?;

        if let Some(op) = op {
            Ok((i, Box::new(Unary { op, expr: ue })))
        } else {
            Ok((i, ue))
        }
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

    fn prefixed_num<'a>(i: &'a str, pfx: &'_ str, is_digit: impl Fn(u8) -> bool) -> IResult<&'a str, &'a str> {
        trace!("prefixed_num: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, num) = recognize(|i| {
            trace!("recognize_num: i={}", i);
            let (i, _) = tag(pfx)(i)?;
            take_till1(|x| !is_digit(x as u8))(i)
        })(i)?;
        Ok((i, num))
    }

    fn hexnum(i: &str) -> IResult<&str, &str> {
        prefixed_num(i, "0x", is_hex_digit)
    }

    fn octnum(i: &str) -> IResult<&str, &str> {
        prefixed_num(i, "0o", is_oct_digit)
    }

    fn binnum(i: &str) -> IResult<&str, &str> {
        prefixed_num(i, "0b", |x| x == b'0' || x == b'1')
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
