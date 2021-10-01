use std::cmp::Ordering;
use std::rc::Rc;

use regex::Regex;

use crate::value;

use crate::parser;

pub fn parse_num(i: &str) -> isize {
    parser::parse_num(i).unwrap_or(0)
}

#[derive(Clone)]
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
            (Value::Int(a), Value::Str(b)) => *a == value::parse_num(b),
            (Value::Str(a), Value::Int(b)) => value::parse_num(a) == *b,
            (Value::Str(a), Value::Str(b)) => a == b,
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Int(a), Value::Str(b)) => a.partial_cmp(&value::parse_num(b)),
            (Value::Str(a), Value::Int(b)) => value::parse_num(a).partial_cmp(b),
            (Value::Str(a), Value::Str(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl Value {
    pub fn as_int(&self) -> isize {
        match self {
            Value::Int(x) => *x,
            Value::Str(x) => value::parse_num(x.as_str()),
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