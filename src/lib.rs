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
    use nom::branch::alt;
    use nom::bytes::complete::{tag, take_till1};
    use nom::character::complete::{multispace0, satisfy};
    use nom::character::{is_alphanumeric, is_hex_digit, is_oct_digit};
    use nom::combinator::{eof, map, opt, peek, recognize};
    use nom::number::complete::recognize_float;
    use nom::sequence::{delimited, preceded};
    use nom::{AsChar, Finish, IResult, Offset};
    use tracing::trace;

    use std::fmt;
    use std::num::ParseIntError;

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
                _ => unreachable!("Unaknown operator {}", i),
            }
        }
    }

    fn parse_num(i: &str) -> Result<isize, ParseIntError> {
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
        fn from_identifier(i: Input) -> Box<Node> {
            Box::new(Node::Identifier(i.to_string()))
        }

        fn from_numeric(i: Input) -> Box<Node> {
            if let Ok(num) = parse_num(i) {
                Box::new(Node::Constant(format!("{}", num)))
            } else {
                Box::new(Node::Constant("0".into()))
            }
        }

        fn from_string(i: Input) -> Box<Node> {
            Box::new(Node::StringLiteral(i.to_string()))
        }

        fn new_binary(lhs: Box<Node>, op: BinaryOp, rhs: Box<Node>) -> Box<Node> {
            Box::new(Node::Binary {
                lhs,
                op,
                rhs,
            })
        }

        fn new_unary(op: UnaryOp, expr: Box<Node>) -> Box<Node> {
            Box::new(Node::Unary {
                op,
                expr,
            })
        }
    }

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
            writeln!(f, "> {}^", " ".repeat(self.pos))?;
            writeln!(f, "> {}`--- this is wrong!", " ".repeat(self.pos))
        }
    }

    pub fn parse(i: Input) -> Result<Box<Node>, ParseError> {
        match parse_expr(i) {
            Ok((_, o)) => Ok(o),

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
    pub fn parse_expr(i: Input) -> IResult<Input, Box<Node>> {
        trace!("parse_expr: i={}", i);
        let (i, e) = simple_expr(i)?;
        let (i, _) = multispace0(i)?;
        let (i, _) = eof(i)?;
        Ok((i, e))
    }

    // generic_expr = { ws* ~ nextp ~ (ws* ~ opp ~ generic_expr ~ ws*)? }
    fn generic_expr<'a>(
        opp: &mut impl FnMut(Input) -> IResult<Input, BinaryOp>,
        nextp: &impl Fn(Input) -> IResult<Input, Box<Node>>,
        i: &'a str,
    ) -> IResult<&'a str, Box<Node>> {
        let (i, _) = multispace0(i)?;
        let (i, ae) = nextp(i)?;
        let (i, on) = opt(move |i| {
            let (i, _) = multispace0(i)?;
            let (i, op) = opp(i)?;
            let (i, se) = generic_expr(opp, nextp, i)?;
            let (i, _) = multispace0(i)?;
            Ok((i, (op, se)))
        })(i)?;

        if let Some((op, n)) = on {
            Ok((i, Node::new_binary(ae, op, n)))
        } else {
            Ok((i, ae))
        }
    }

    fn simple_expr(i: Input) -> IResult<Input, Box<Node>> {
        trace!("simple_expr: i={}", i);
        generic_expr(
            &mut move |i| map(tag("||"), BinaryOp::from)(i),
            &move |i| and_expr(i),
            i,
        )
    }

    fn and_expr(i: Input) -> IResult<Input, Box<Node>> {
        trace!("and_expr: i={}", i);
        generic_expr(
            &mut move |i| map(tag("&&"), BinaryOp::from)(i),
            &move |i| rel_expr(i),
            i,
        )
    }

    fn rel_expr(i: Input) -> IResult<Input, Box<Node>> {
        trace!("rel_expr: i={}", i);
        generic_expr(
            &mut move |i| map(relop, BinaryOp::from)(i),
            &move |i| sum_expr(i),
            i,
        )
    }

    fn sum_expr(i: Input) -> IResult<Input, Box<Node>> {
        trace!("sum_expr: i={}", i);
        generic_expr(
            &mut move |i| map(tag("&"), BinaryOp::from)(i),
            &move |i| unary_expr(i),
            i,
        )
    }

    // unary_expr = { ws* ~ ("!" | "-")? ~ factor }
    fn unary_expr(i: Input) -> IResult<Input, Box<Node>> {
        trace!("unary_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, op) = opt(map(alt((tag("!"), tag("-"))), UnaryOp::from))(i)?;
        let (i, ue) = factor(i)?;

        if let Some(op) = op {
            Ok((i, Node::new_unary(op, ue)))
        } else {
            Ok((i, ue))
        }
    }

    // factor = { ws* ~ (identifier | numeric | string | parens_expr) }
    fn factor(i: Input) -> IResult<Input, Box<Node>> {
        trace!("factor: i={}", i);
        let (i, _) = multispace0(i)?;
        alt((identifier, numeric, string, parens_expr))(i)
    }

    // FIXME: this one sucks particularly HARD
    fn string(i: Input) -> IResult<Input, Box<Node>> {
        trace!("string: i={}", i);
        map(
            delimited(
                tag("\""),
                recognize(move |i| {
                    let mut i = i;
                    while let Ok((r, _)) = alt::<_, _, nom::error::Error<Input>, _>((
                            escaped_char,
                            take_till1(|x| x == '\\' || x == '"'),
                        ))(i)
                    {
                        i = r;
                    }
                    Ok((i, ""))
                }),
                tag("\""),
            ),
            Node::from_string,
        )(i)
    }

    // escaped_char = { "\\" ~ ("\"" | "\\" | "n" | "r" ...) }
    fn escaped_char(i: Input) -> IResult<Input, Input> {
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

    // parens_expr = { ws* ~ "(" ~ simple_expr ~ ")" }
    fn parens_expr(i: Input) -> IResult<Input, Box<Node>> {
        trace!("parens_expr: i={}", i);
        let (i, _) = multispace0(i)?;
        delimited(tag("("), simple_expr, tag(")"))(i)
    }

    fn relop(i: Input) -> IResult<Input, BinaryOp> {
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

    fn identifier(i: Input) -> IResult<Input, Box<Node>> {
        trace!("identifier: i={}", i);
        let (i, _) = multispace0(i)?;
        let (i, _) = peek(satisfy(|c| c.is_alpha() || c == '_'))(i)?;
        let (i, ident) = take_till1(|c| !(is_alphanumeric(c as u8) || c == '_'))(i)?;
        Ok((i, Node::from_identifier(ident)))
    }

    fn numeric(i: Input) -> IResult<Input, Box<Node>> {
        trace!("numeric: i={}", i);
        let (i, _) = multispace0(i)?;
        map(
            alt((hexnum, octnum, binnum, recognize_float)),
            Node::from_numeric,
        )(i)
    }

    fn prefixed_num<'a>(
        pfx: &'_ str,
        is_digit: impl Fn(u8) -> bool,
        i: &'a str,
    ) -> IResult<&'a str, &'a str> {
        trace!("prefixed_num: i={}", i);
        let (i, _) = multispace0(i)?;
        recognize(|i| {
            trace!("recognize_num: i={}", i);
            let (i, _) = tag(pfx)(i)?;
            take_till1(|x| !is_digit(x as u8))(i)
        })(i)
    }

    fn hexnum(i: Input) -> IResult<Input, Input> {
        prefixed_num("0x", is_hex_digit, i)
    }

    fn octnum(i: Input) -> IResult<Input, Input> {
        prefixed_num("0o", is_oct_digit, i)
    }

    fn binnum(i: Input) -> IResult<Input, Input> {
        prefixed_num("0b", |x| x == b'0' || x == b'1', i)
    }

    pub mod eval {
        // quick and hacked implementation of an expression evaluation

        use std::collections::HashMap;
        use regex::Regex;
        use super::{Node, BinaryOp, UnaryOp};

        pub trait Accessor {
            fn ident<'a>(&'a self, k: &str) -> Result<&'a str, String>;
        }

        pub trait Eval<T> {
            fn eval_filter(&self, e: T, re_cache: &mut HashMap<String, Regex>) -> bool;
        }

        // FIXME: this is quickest and most inefficient way I could possibly imagine!
        impl Eval<&dyn Accessor> for Box<Node> {
            fn eval_filter(&self, e: &dyn Accessor, re_cache: &mut HashMap<String, Regex>) -> bool {
                fn parse_num(i: &str) -> isize {
                    let r = if let Some(i) = i.strip_prefix("0x") {
                        isize::from_str_radix(i, 16)
                    } else if let Some(i) = i.strip_prefix("0o") {
                        isize::from_str_radix(i, 8)
                    } else if let Some(i) = i.strip_prefix("0b") {
                        isize::from_str_radix(i, 2)
                    } else {
                        i.parse::<isize>()
                    };
                    r.unwrap_or(0)
                }

                fn value(node: &Node, e: &dyn Accessor) -> Option<String> {
                    match node {
                        Node::StringLiteral(s) => Some(s.clone()),
                        Node::Constant(s) => Some(s.clone()),
                        Node::Identifier(s) => {
                            e.ident(s).ok().map(|x| {
                                let num = parse_num(x);
                                let snum = num.to_string();
                                // Is this a number?
                                if num != 0 && snum != "0" {
                                    snum
                                } else {
                                    x.to_string()
                                }
                            })
                        },
                        _ => None,
                    }
                }

                fn ret(v: bool) -> String {
                    if v { "1".into() } else { "0".into() }
                }

                fn eval(node: &Node, e: &dyn Accessor, re_cache: &mut HashMap<String, Regex>) -> String {
                    match node {
                        Node::Binary { rhs, op, lhs } => {
                            let l = eval(lhs, e, re_cache);
                            let r = eval(rhs, e, re_cache);

                            match op {
                                BinaryOp::And => {
                                    ret(l != "0" && r != "0")
                                },
                                BinaryOp::Or  => {
                                    ret(l != "0" || r != "0")
                                },

                                BinaryOp::Eq  => {
                                    ret(l == r)
                                },
                                BinaryOp::Ne  => {
                                    ret(l != r)
                                },
                                BinaryOp::Ge  => {
                                    ret(l >= r)
                                },
                                BinaryOp::Gt  => {
                                    ret(l >  r)
                                },
                                BinaryOp::Le  => {
                                    ret(l <= r)
                                },
                                BinaryOp::Lt  => {
                                    ret(l <  r)
                                },
                                BinaryOp::Match => {
                                    let re = re_cache.entry(r.clone());
                                    let re = re.or_insert_with(move || {
                                        if let Ok(re) = regex::Regex::new(&r) {
                                            re
                                        } else {
                                            panic!("Invalid regular expression");
                                        }
                                    });
                                    ret(re.is_match(&l))
                                },

                                BinaryOp::Band => {
                                    let le = parse_num(&l) as usize;
                                    let re = parse_num(&r) as usize;
                                    let res = format!("{}", le & re);
                                    res
                                },
                                BinaryOp::Bor => unreachable!(),
                            }
                        }

                        Node::Unary { op, expr } => {
                            match op {
                                UnaryOp::Not => {
                                    ret(eval(expr, e, re_cache) == "0")
                                }
                                UnaryOp::Neg => {
                                    format!("{}", -parse_num(&eval(expr, e, re_cache)))
                                }
                            }
                        }

                        _ => value(node, e).unwrap_or_else(|| "0".into()),
                    }
                }

                eval(self, e, re_cache) != "0"
            }
        }
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
