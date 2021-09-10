// quick and hacked implementation of an expression evaluation

use crate::nom_parser::{self, BinaryOp, Node, UnaryOp};
use regex::Regex;
use std::rc::Rc;

pub fn parse_num(i: &str) -> isize {
    nom_parser::parse_num(i).unwrap_or(0)
}

pub trait Accessor {
    fn get_str<'a>(&'a self, k: &str) -> Result<Rc<String>, String>;
    fn get_num(&self, k: &str) -> Result<isize, String>;

    fn is_int(&self, k: &str) -> bool {
        matches!(self.get_num(k), Ok(_))
    }
    fn is_str(&self, k: &str) -> bool {
        matches!(self.get_str(k), Ok(_))
    }
}

pub trait Eval<T> {
    fn eval_filter(&self, e: T) -> bool;
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

fn value<'a>(node: &'a Node, e: &'a dyn Accessor) -> Option<Value> {
    match node {
        Node::StringLiteral(s) => Some(Value::Str(s.clone())),
        Node::Constant(s) => Some(Value::Int(*s)),
        Node::Identifier(s) => {
            if let Ok(num) = e.get_num(s) {
                Some(Value::Int(num))
            } else if let Ok(s) = e.get_str(s) {
                Some(Value::Str(s.clone()))
            } else {
                None
            }
        }
        Node::Regexp(re) => Some(Value::Re(re.clone())),
        Node::Nil => Some(Value::Nil),
        _ => unreachable!(),
    }
}

// FIXME: this is quickest and most inefficient way I could possibly imagine!
impl Eval<&dyn Accessor> for Box<Node> {
    fn eval_filter(&self, e: &dyn Accessor) -> bool {
        fn eval<'a>(node: &'a Node, e: &'a dyn Accessor) -> Value {
            match node {
                Node::Binary { rhs, op, lhs } => {
                    let l = eval(lhs, e);

                    match op {
                        BinaryOp::And => (l.as_bool() && eval(rhs, e).as_bool()).into(),
                        BinaryOp::Or => (l.as_bool() || eval(rhs, e).as_bool()).into(),
                        _ => {
                            let r = eval(rhs, e);

                            match op {
                                BinaryOp::Eq => (l == r).into(),
                                BinaryOp::Ne => (l != r).into(),
                                BinaryOp::Ge => (l.as_int() >= r.as_int()).into(),
                                BinaryOp::Gt => (l.as_int() > r.as_int()).into(),
                                BinaryOp::Le => (l.as_int() <= r.as_int()).into(),
                                BinaryOp::Lt => (l.as_int() < r.as_int()).into(),
                                BinaryOp::Match => (r.re_matches(l.as_str())).into(),

                                BinaryOp::Band => Value::Int(l.as_int() & r.as_int()),
                                BinaryOp::Or | BinaryOp::And => unreachable!(),
                            }
                        }
                    }
                }

                Node::Unary { op, expr } => match op {
                    UnaryOp::Not => Value::Int(if !eval(expr, e).as_bool() { 1 } else { 0 }),
                    UnaryOp::Neg => Value::Int(-eval(expr, e).as_int()),
                },

                _ => value(node, e).unwrap_or(Value::Int(0)),
            }
        }

        eval(self, e).as_bool()
    }
}
