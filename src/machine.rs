use crate::eval::{Accessor, Value};
use crate::nom_parser::{BinaryOp, Node, UnaryOp};

use regex::Regex;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Instr<'a> {
    LoadIdent(&'a str),
    LoadString(&'a str),
    LoadNum(isize),
    LoadRe(&'a Regex),
    And,
    Or,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    BAnd,
    Match,
    Not,
    Neg,
    Ret,
}

impl Display for Instr<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Instr::LoadIdent(x) => format!("load ident {}", x),
                Instr::LoadString(x) => format!("load string \"{}\"", x),
                Instr::LoadNum(x) => format!("load num {}", x),
                Instr::LoadRe(x) => format!("load re /{:?}/", x),
                Instr::And => "and".into(),
                Instr::Or => "or".into(),
                Instr::Eq => "eq".into(),
                Instr::Ne => "ne".into(),
                Instr::Lt => "lt".into(),
                Instr::Le => "le".into(),
                Instr::Gt => "gt".into(),
                Instr::Ge => "ge".into(),
                Instr::BAnd => "band".into(),
                Instr::Match => "match".into(),
                Instr::Not => "not".into(),
                Instr::Neg => "neg".into(),
                Instr::Ret => "ret".into(),
            }
        )
    }
}

impl<'a> From<&BinaryOp> for Instr<'a> {
    fn from(op: &BinaryOp) -> Self {
        match op {
            BinaryOp::And => Instr::And,
            BinaryOp::Or => Instr::Or,
            BinaryOp::Eq => Instr::Eq,
            BinaryOp::Ne => Instr::Ne,
            BinaryOp::Ge => Instr::Ge,
            BinaryOp::Gt => Instr::Gt,
            BinaryOp::Le => Instr::Le,
            BinaryOp::Lt => Instr::Lt,
            BinaryOp::Match => Instr::Match,
            BinaryOp::Band => Instr::BAnd,
        }
    }
}

impl<'a> From<&UnaryOp> for Instr<'a> {
    fn from(op: &UnaryOp) -> Self {
        match op {
            UnaryOp::Not => Instr::Not,
            UnaryOp::Neg => Instr::Neg,
        }
    }
}

impl<'a> From<&'a Node> for Instr<'a> {
    fn from(node: &'a Node) -> Self {
        match node {
            Node::StringLiteral(s) => Instr::LoadString(s),
            Node::Identifier(s) => Instr::LoadIdent(s),
            Node::Constant(i) => Instr::LoadNum(*i),
            Node::Regexp(r) => Instr::LoadRe(r),
            _ => unreachable!(),
        }
    }
}

pub struct Machine<'a> {
    instr: Vec<Instr<'a>>,
    mem: RefCell<Vec<Value<'a>>>,
}

impl Display for Machine<'_> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for i in self.instr.iter() {
            writeln!(f, "{}", i)?;
        }
        Ok(())
    }
}

impl<'a> Machine<'a> {
    pub fn from_node(node: &'a Node) -> Result<Machine<'a>, String> {
        fn compile_<'a>(buf: &mut Vec<Instr<'a>>, node: &'a Node) -> Result<(), String> {
            match node {
                Node::Binary { rhs, op, lhs } => {
                    // This needs to be reversed for eval.
                    compile_(buf, lhs)?;
                    compile_(buf, rhs)?;
                    buf.push(op.into());
                }

                Node::Unary { op, expr } => {
                    compile_(buf, expr)?;
                    buf.push(op.into());
                }

                _ => buf.push(node.into()),
            }

            Ok(())
        }

        let mut buf = Vec::new();
        if compile_(&mut buf, node).is_ok() {
            buf.push(Instr::Ret);
            Ok(Machine {
                instr: buf,
                mem: RefCell::new(Vec::new()),
            })
        } else {
            Err("Could not compile".into())
        }
    }

    pub fn eval(&self, a: &'a dyn Accessor) -> bool {
        // let mut memp = self.mem.borrow_mut();
        // memp.reserve(self.instr.len()); // too much, still...

        // let mut mem = unsafe { Vec::from_raw_parts(memp.as_mut_ptr(), memp.len(), memp.capacity()) };

        let mut mem = self.mem.borrow_mut();
        mem.clear();

        for i in self.instr.iter() {
            match *i {
                Instr::LoadIdent(x) => {
                    if a.is_int(x) {
                        mem.push(Value::Int(a.get_num(x).unwrap()))
                    } else if a.is_str(x) {
                        mem.push(Value::Str(a.get_str(x).unwrap()))
                    } else {
                        mem.push(Value::Int(0))
                    }
                }

                Instr::LoadString(x) => mem.push(Value::Str(x)),
                Instr::LoadNum(x) => mem.push(Value::Int(x)),
                Instr::LoadRe(x) => mem.push(Value::Re(x)),

                Instr::Eq => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l == r).into());
                }
                Instr::Ne => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l != r).into());
                }

                Instr::Gt => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l.as_int() > r.as_int()).into());
                }
                Instr::Ge => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l.as_int() >= r.as_int()).into());
                }
                Instr::Lt => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l.as_int() < r.as_int()).into());
                }
                Instr::Le => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l.as_int() <= r.as_int()).into());
                }

                // Yes, there is no short-circuit here...
                Instr::And => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l.as_bool() && r.as_bool()).into());
                }
                Instr::Or => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l.as_bool() || r.as_bool()).into());
                }

                Instr::BAnd => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((l.as_int() & r.as_int()).into());
                }
                Instr::Match => {
                    let r = mem.pop().unwrap();
                    let l = mem.pop().unwrap();
                    mem.push((r.as_re().is_match(l.as_str())).into());
                }

                Instr::Not => {
                    let e = mem.pop().unwrap();
                    mem.push((!e.as_bool()).into());
                }
                Instr::Neg => {
                    let e = mem.pop().unwrap();
                    mem.push((-e.as_int()).into());
                }

                Instr::Ret => {
                    return mem.pop().unwrap().as_bool();
                }
            }
        }

        false
    }
}
