use tracing::{error as log_error, Level};

// A filtering grammar... (Note: I am not even sure this CAN work)
//
//
// FILTER = EXPR
// EXPR = '(' EXPR ')'
//      | UNOP EXPR
//      | PRIM
//      | SECONDARY
//      | COMPARISON
//      | LOGIC
//      | IDENTIFIER '(' EXPR ')'
//      | CONST
//      | IDENTIFIER
// UNOP = '!' | '-' | '~'
// PRIM = EXPR '*' EXPR
//      | EXPR '/' EXPR
// SECONDARY = EXPR '+' EXPR
//           | EXPR '-' EXPR
// COMPARISON = EXPR COMPOP EXPR
// COMPOP = '>' | '<' | '==' | '>=' | '<=' | '!='
// LOGIC = EXPR '&&' EXPR
//       | EXPR '||' EXPR
// CONST = NUMBER | STRING
// STRING = any c string literal?
// NUMBER = any c numeric literal?
//
//
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

use nom::{IResult, bytes::complete::{
    tag
}, AsChar};

use std::collections::HashMap;
use std::fmt::Debug;
use nom::character::complete::{multispace0, satisfy};
use nom::combinator::peek;
use nom::bytes::complete::take_till1;
use nom::character::is_alphanumeric;
use std::marker::PhantomData;
use nom::branch::alt;

use tracing::{
    trace,
    info,
};
use nom::number::complete::recognize_float;

pub trait Filter<T: Debug> : Debug {
    fn eval(&self, item: &T) -> bool;
}

#[derive(Debug)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string()
        }
    }
}

impl<T: Debug> Filter<T> for Identifier {
    fn eval(&self, item: &T) -> bool {
        return true;
    }
}

#[derive(Debug)]
pub struct Constant {
    value: String,
}

impl Constant {
    fn new(name: &str) -> Self {
        Self {
            value: name.to_string()
        }
    }
}

impl<T: Debug> Filter<T> for Constant {
    fn eval(&self, item: &T) -> bool {
        return true;
    }
}

#[derive(Debug)]
struct UnaryOp<T: Debug> {
    op: String,
    expr: Box<dyn Filter<T>>,
}

impl<T: Debug> UnaryOp<T> {
    fn new(op: &str, expr: Box<dyn Filter<T>>) -> Self {
        Self {
            op: op.to_string(),
            expr,
        }
    }
}

impl<T: Debug> Filter<T> for UnaryOp<T> {
    fn eval(&self, item: &T) -> bool {
        todo!()
    }
}

#[derive(Debug)]
struct BinaryOp<T: Debug> {
    left: Box<dyn Filter<T>>,
    op: String,
    right: Box<dyn Filter<T>>,
}

impl<T: Debug> BinaryOp<T> {
    fn new(left: Box<dyn Filter<T>>, op: &str, right: Box<dyn Filter<T>>) -> Self {
        Self {
            left,
            op: op.to_string(),
            right,
        }
    }
}

impl<T: Debug> Filter<T> for BinaryOp<T> {
    fn eval(&self, item: &T) -> bool {
        todo!()
    }
}

pub struct FilterContext<T> {
    constants: HashMap<String, String>,
    identifiers: HashMap<String, Box<dyn Fn(&T) -> String>>,
    functions: HashMap<String, Box<dyn Filter<T>>>,  // do we really want/can do this?!
}

pub fn parse_filter<T: 'static + Debug>(i: &str) -> IResult<&str, Box<dyn Filter<T>>> {
    trace!("parse_filter: i={}", i);
    let (i, expr) = parse_chained_expr(i)?;
    Ok((i, expr))
}

fn parse_logic_part<T: 'static + Debug>(i: &str) -> IResult<&str, (&str, Box<dyn Filter<T>>)> {
    trace!("parse_logic_part: i={}", i);
    let (i, _) = multispace0(i)?;
    let (i, op) = alt((tag("&&"), tag("||")))(i)?;
    let (i, right) = parse_chained_expr(i)?;
    Ok((i, (op, right)))
}

fn parse_chained_expr<T: 'static + Debug>(i: &str) ->  IResult<&str, Box<dyn Filter<T>>> {
    trace!("parse_chained_expr: i={}", i);
    let (i, expr) = parse_expr(i)?;
    if let Ok((i, (op, right))) = parse_logic_part(i) {
        Ok((i, Box::new(BinaryOp::<T>::new(expr, op, right)) as Box<dyn Filter<T>>))
    } else {
        Ok((i, expr))
    }
}

fn parse_expr<T: 'static + Debug>(i: &str) -> IResult<&str, Box<dyn Filter<T>>> {
    trace!("parse_expr: i={}", i);
    let (i, _) = multispace0(i)?;
    // Is this a parenthesized expression?
    if let Ok((i, _)) = tag::<&str, &str, nom::error::Error<&str>>("(")(i) {
        let (i, expr) = parse_chained_expr(i)?;
        let (i, _) = multispace0(i)?;
        let (i, _) = tag(")")(i)?;
        return Ok((i, expr));
    }

    if let Ok((i, unop)) = parse_unary_op::<T>(i) {
        return Ok((i, unop));
    }

    if let Ok((i, ident)) = parse_ident::<T>(i) {
        if let Ok((i, (op, prim))) = parse_primary_part::<T>(i) {
            return Ok((i, Box::new(BinaryOp::<T>::new(ident, op, prim))));
        }

        if let Ok((i, (op, prim))) = parse_secondary_part::<T>(i) {
            return Ok((i, Box::new(BinaryOp::<T>::new(ident, op, prim))));
        }

        if let Ok((i, (op, prim))) = parse_comparison_part::<T>(i) {
            return Ok((i, Box::new(BinaryOp::<T>::new(ident, op, prim))));
        }

        return Ok((i, ident));
    }

    todo!()
}

fn parse_primary_part<T: 'static + Debug>(i: &str) -> IResult<&str, (&str, Box<dyn Filter<T>>)> {
    trace!("parse_primary_part: i={}", i);
    let (i, _) = multispace0(i)?;
    let (i, op) = alt((tag("*"), tag("/")))(i)?;
    let (i, right) = parse_expr(i)?;
    Ok((i, (op, right)))
}

fn parse_comparison_part<T: 'static + Debug>(i: &str) -> IResult<&str, (&str, Box<dyn Filter<T>>)> {
    trace!("parse_comparison_part: i={}", i);
    let (i, _) = multispace0(i)?;
    let (i, op) = alt((tag("=="), tag("!="), tag(">="), tag("<="), tag(">"), tag("<")))(i)?;
    let (i, right) = parse_expr(i)?;
    Ok((i, (op, right)))
}

fn parse_secondary_part<T: 'static + Debug>(i: &str) -> IResult<&str, (&str, Box<dyn Filter<T>>)> {
    trace!("parse_secondary_part: i={}", i);
    let (i, _) = multispace0(i)?;
    let (i, op) = alt((tag("+"), tag("-")))(i)?;
    let (i, right) = parse_expr(i)?;
    Ok((i, (op, right)))
}

fn parse_unary_op<T: 'static + Debug>(i: &str) -> IResult<&str, Box<dyn Filter<T>>> {
    trace!("parse_unary_op: i={}", i);
    let (i, _) = multispace0(i)?;
    let (i, op) = alt((tag("!"), tag("-"), tag("~")))(i)?;
    let (i, _) = multispace0(i)?;
    let (i, expr) = parse_expr(i)?;
    Ok((i, Box::new(UnaryOp::<T>::new(op, expr))))
}

fn identifier(i: &str) -> IResult<&str, &str> {
    trace!("identifier: i={}", i);
    let (i, _) = multispace0(i)?;
    let (i, _) = peek(satisfy(|c| c.is_alpha() || c == '_'))(i)?;
    let (i, ident) = take_till1(|c| !(is_alphanumeric(c as u8) || c == '_'))(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, ident))
}

fn parse_ident<T: Debug>(i: &str) -> IResult<&str, Box<dyn Filter<T>>> {
    trace!("parse_ident: i={}", i);
    if let Ok((i, num)) = numeric(i) {
        Ok((i, Box::new(Constant::new(num))))
    } else {
        let (i, ident) = identifier(i)?;
        Ok((i, Box::new(Identifier::new(ident))))
    }
}

fn numeric(i: &str) -> IResult<&str, &str> {
    trace!("numeric: i={}", i);
    let (i, _) = multispace0(i)?;
    let (i, num) = recognize_float(i)?;
    Ok((i, num))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
