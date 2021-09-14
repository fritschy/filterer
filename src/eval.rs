// quick and hacked implementation of an expression evaluation

use crate::parser;
use regex::Regex;
use std::rc::Rc;

pub fn parse_num(i: &str) -> isize {
    parser::parse_num(i).unwrap_or(0)
}

pub trait Accessor {
    fn get_str(&self, k: &str, i: usize) -> Option<Rc<String>>;
    fn get_num(&self, k: &str, i: usize) -> Option<isize>;
}

pub enum Value {
    Int(isize),
    Str(Rc<String>),
    Re(Rc<Regex>),
    Nil,
}

impl From<bool> for Value {
    fn from(b: bool) -> Self {
        if b {
            Self::Int(1)
        } else {
            Self::Int(0)
        }
    }
}

impl From<isize> for Value {
    fn from(b: isize) -> Self {
        Self::Int(b)
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Int(a), Value::Str(b)) => *a == parse_num(b),
            (Value::Str(a), Value::Int(b)) => parse_num(a) == *b,
            (Value::Str(a), Value::Str(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    pub fn as_int(&self) -> isize {
        match self {
            Value::Int(x) => *x,
            Value::Str(x) => parse_num(x.as_str()),
            Value::Nil => 0,
            _ => panic!("as_int() needs to be a number"),
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Value::Int(x) => *x != 0,
            Value::Str(s) => s.as_str() != "0",
            _ => false,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Value::Str(s) => s.as_str(),
            Value::Int(0) | Value::Re(_) => "0",
            Value::Int(_) => "1",
            Value::Nil => "0",
        }
    }

    pub fn re_matches(&self, other: &str) -> bool {
        match self {
            Value::Re(r) => r.is_match(other),
            _ => false,
        }
    }
}
