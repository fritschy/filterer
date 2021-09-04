// quick and hacked implementation of an expression evaluation

use crate::nom_parser::{Node, BinaryOp, UnaryOp};

pub fn parse_num(i: &str) -> isize {
    crate::nom_parser::parse_num(i).unwrap_or(0)
}

pub trait Accessor {
    fn ident<'a>(&'a self, k: &str) -> Result<&'a str, String>;
}

pub trait Eval<T> {
    fn eval_filter(&self, e: T) -> bool;
}

// FIXME: this is quickest and most inefficient way I could possibly imagine!
impl Eval<&dyn Accessor> for Box<Node> {
    fn eval_filter(&self, e: &dyn Accessor) -> bool {
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

        fn eval(node: &Node, e: &dyn Accessor) -> String {
            match node {
                Node::Binary { rhs, op, lhs } => {
                    let l = eval(lhs, e);
                    let r = eval(rhs, e);

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
                            let l = parse_num(&l);
                            let r = parse_num(&r);
                            ret(l >= r)
                        },
                        BinaryOp::Gt  => {
                            let l = parse_num(&l);
                            let r = parse_num(&r);
                            ret(l >  r)
                        },
                        BinaryOp::Le  => {
                            let l = parse_num(&l);
                            let r = parse_num(&r);
                            ret(l <= r)
                        },
                        BinaryOp::Lt  => {
                            let l = parse_num(&l);
                            let r = parse_num(&r);
                            ret(l <  r)
                        },
                        BinaryOp::Match => {
                            let re = match &**rhs {Node::Regexp(re) => re, _ => panic!("Not a regex"), };
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
                            ret(eval(expr, e) == "0")
                        }
                        UnaryOp::Neg => {
                            (-parse_num(&eval(expr, e))).to_string()
                        }
                    }
                }

                _ => value(node, e).unwrap_or_else(|| "0".into()),
            }
        }

        eval(self, e) != "0"
    }
}