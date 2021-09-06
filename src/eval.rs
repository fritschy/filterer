// quick and hacked implementation of an expression evaluation

use crate::nom_parser::{Node, BinaryOp, UnaryOp};
use regex::Regex;

pub fn parse_num(i: &str) -> isize {
    crate::nom_parser::parse_num(i).unwrap_or(0)
}

pub trait Accessor {
    fn get_str<'a>(&'a self, k: &str) -> Result<&'a str, String>;
    fn get_num(&self, k: &str) -> Result<isize, String>;
}

pub trait Eval<T> {
    fn eval_filter(&self, e: T) -> bool;
}

// FIXME: this is quickest and most inefficient way I could possibly imagine!
impl Eval<&dyn Accessor> for Box<Node> {
    fn eval_filter(&self, e: &dyn Accessor) -> bool {
        #[derive(Debug)]
        enum Value<'a> {
            Int(isize),
            Str(&'a str),
            Re(&'a Regex),
        }

        impl<'a> From<bool> for Value<'a> {
            fn from(b: bool) -> Self {
                if b { Self::Int(1) }
                else { Self::Int(0) }
            }
        }

        impl<'a> PartialEq for Value<'a> {
            fn eq(&self, other: &Self) -> bool {
                if self.is_re() || other.is_re() {
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
                        return *a == other.as_str();
                    } else if b_i {
                        return parse_num(a) == other.as_int();
                    }
                }

                false
            }
        }

        impl<'a> Value<'a> {
            fn as_int(&self) -> isize {
                match self {
                    Value::Int(x) => *x,
                    Value::Str(x) => {
                        tracing::warn!("as_int() from string!");
                        parse_num(*x)
                    },
                    _ => panic!("as_int() needs to be a number"),
                }
            }

            fn as_bool(&self) -> bool {
                match self {
                    Value::Int(x) => *x != 0,
                    Value::Str(s) => *s != "0",
                    _ => false,
                }
            }

            fn as_str(&self) -> &str {
                match self {
                    Value::Str(s) => *s,
                    Value::Int(0) | Value::Re(_) => {
                        tracing::warn!("as_str on int or re");
                        "0"
                    }
                    Value::Int(_) => {
                        tracing::warn!("as_str on int");
                        "1"
                    }
                }
            }

            fn as_re(&self) -> &Regex {
                match self {
                    Value::Re(r) => *r,
                    _ => panic!("Not an re"),
                }
            }

            fn is_int(&self) -> bool {
                if let Value::Int(_) = self {
                    true
                } else {
                    false
                }
            }

            fn is_str(&self) -> bool {
                if let Value::Str(_) = self {
                    true
                } else {
                    false
                }
            }

            fn is_re(&self) -> bool {
                if let Value::Re(_) = self {
                    true
                } else {
                    false
                }
            }
        }

        fn value<'a>(node: &'a Node, e: &'a dyn Accessor) -> Option<Value<'a>> {
            match node {
                Node::StringLiteral(s) => Some(Value::Str(s)),
                Node::Constant(s) => Some(Value::Int(*s)),
                Node::Identifier(s) => {
                    if let Ok(num) = e.get_num(s) {
                        Some(Value::Int(num))
                    } else if let Ok(s) = e.get_str(s) {
                        Some(Value::Str(s))
                    } else {
                        None
                    }
                },
                Node::Regexp(re) => Some(Value::Re(re)),
                _ => unreachable!(),
            }
        }

        fn eval<'a>(node: &'a Node, e: &'a dyn Accessor) -> Value<'a> {
            match node {
                Node::Binary { rhs, op, lhs } => {
                    let l = eval(lhs, e);
                    let r = eval(rhs, e);

                    match op {
                        BinaryOp::And => {
                            (l.as_bool() && r.as_bool()).into()
                        },
                        BinaryOp::Or  => {
                            (l.as_bool() || r.as_bool()).into()
                        },

                        BinaryOp::Eq  => {
                            (l == r).into()
                        },
                        BinaryOp::Ne  => {
                            (l != r).into()
                        },
                        BinaryOp::Ge  => {
                            (l.as_int() >= r.as_int()).into()
                        },
                        BinaryOp::Gt  => {
                            (l.as_int() >  r.as_int()).into()
                        },
                        BinaryOp::Le  => {
                            (l.as_int() <= r.as_int()).into()
                        },
                        BinaryOp::Lt  => {
                            (l.as_int() <  r.as_int()).into()
                        },
                        BinaryOp::Match => {
                            (r.as_re().is_match(l.as_str())).into()
                        },

                        BinaryOp::Band => {
                            Value::Int(l.as_int() & r.as_int())
                        },
                        BinaryOp::Bor => unreachable!(),
                    }
                }

                Node::Unary { op, expr } => {
                    match op {
                        UnaryOp::Not => { Value::Int(if !eval(expr, e).as_bool() { 1 } else { 0 }) }
                        UnaryOp::Neg => { Value::Int(-eval(expr, e).as_int()) }
                    }
                }

                _ => value(node, e).unwrap_or(Value::Int(0)),
            }
        }

        eval(self, e).as_bool()
    }
}