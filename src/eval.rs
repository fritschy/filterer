// quick and hacked implementation of an expression evaluation

use crate::parser;
use regex::Regex;
use std::rc::Rc;

pub fn parse_num(i: &str) -> isize {
    parser::parse_num(i).unwrap_or(0)
}

pub trait Accessor {
    fn get_str(&self, k: &str) -> Option<Rc<String>>;
    fn get_num(&self, k: &str) -> Option<isize>;

    fn is_int(&self, k: &str) -> bool {
        self.get_num(k).is_some()
    }
    fn is_str(&self, k: &str) -> bool {
        self.get_str(k).is_some()
    }
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
        if self.is_re() || other.is_re() {
            return false;
        }

        if self.is_nil() || other.is_nil() {
            return false;
        }

        let b_i = other.is_int();
        let b_s = other.is_str();

        // allow comparison of nums and strings
        if let Value::Int(a) = self {
            if b_i {
                return *a == other.as_int();
            } else if b_s {
                return *a == parse_num(other.as_str());
            }
        } else if let Value::Str(a) = self {
            if b_s {
                return a.as_str() == other.as_str();
            } else if b_i {
                return parse_num(a) == other.as_int();
            }
        }

        false
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

    pub fn as_re(&self) -> &Regex {
        match self {
            Value::Re(r) => r,
            _ => panic!("Not an re"),
        }
    }

    pub fn re_matches(&self, other: &str) -> bool {
        match self {
            Value::Re(r) => r.is_match(other),
            _ => false,
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Value::Int(_))
    }

    pub fn is_str(&self) -> bool {
        matches!(self, Value::Str(_))
    }

    pub fn is_re(&self) -> bool {
        matches!(self, Value::Re(_))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }
}
