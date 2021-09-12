use crate::eval::{Accessor, Value};
use crate::parser::{BinaryOp, Node, UnaryOp};

use regex::Regex;
use std::cell::RefCell;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

pub enum Instr {
    LoadIdent(Rc<String>),
    LoadString(Rc<String>),
    LoadNum(isize),
    LoadRe(Rc<Regex>),
    LoadNil,
    And,
    Or,
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
    BAnd,
    Match,
    Not,
}

impl Display for Instr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::LoadIdent(x) => write!(f, "load ident {}", *x),
            Instr::LoadString(x) => write!(f, "load string \"{}\"", *x),
            Instr::LoadNum(x) => write!(f, "load num {}", x),
            Instr::LoadRe(x) => write!(f, "load re /{:?}/", *x),
            Instr::LoadNil => write!(f, "load nil"),
            Instr::And => write!(f, "and"),
            Instr::Or => write!(f, "or"),
            Instr::Eq => write!(f, "eq"),
            Instr::Lt => write!(f, "lt"),
            Instr::Le => write!(f, "le"),
            Instr::Gt => write!(f, "gt"),
            Instr::Ge => write!(f, "ge"),
            Instr::BAnd => write!(f, "band"),
            Instr::Match => write!(f, "match"),
            Instr::Not => write!(f, "not"),
        }
    }
}

impl From<&BinaryOp> for Instr {
    fn from(op: &BinaryOp) -> Self {
        match op {
            BinaryOp::And => Instr::And,
            BinaryOp::Or => Instr::Or,
            BinaryOp::Eq => Instr::Eq,
            BinaryOp::Ge => Instr::Ge,
            BinaryOp::Gt => Instr::Gt,
            BinaryOp::Le => Instr::Le,
            BinaryOp::Lt => Instr::Lt,
            BinaryOp::Match => Instr::Match,
            BinaryOp::Band => Instr::BAnd,
            _ => unreachable!("Unmapped operator"),
        }
    }
}

impl From<&UnaryOp> for Instr {
    fn from(op: &UnaryOp) -> Self {
        match op {
            UnaryOp::Not => Instr::Not,
        }
    }
}

impl From<&Node> for Instr {
    fn from(node: &Node) -> Self {
        match node {
            Node::StringLiteral(s) => Instr::LoadString(s.clone()),
            Node::Identifier(s) => Instr::LoadIdent(s.clone()),
            Node::Constant(i) => Instr::LoadNum(*i),
            Node::Regexp(r) => Instr::LoadRe(r.clone()),
            Node::Nil => Instr::LoadNil,
            _ => unreachable!(),
        }
    }
}

pub struct Machine {
    instr: Vec<Instr>,
    mem: RefCell<Vec<Value>>,
}

impl Display for Machine {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for i in self.instr.iter() {
            writeln!(f, "{}", i)?;
        }
        Ok(())
    }
}

impl Machine {
    pub fn from_node(node: Rc<Node>) -> Result<Machine, String> {
        fn compile_(buf: &mut Vec<Instr>, node: Rc<Node>) -> Result<(), String> {
            match node.as_ref() {
                Node::Binary { rhs, op, lhs } => {
                    // This needs to be reversed for eval.
                    compile_(buf, lhs.clone())?;
                    compile_(buf, rhs.clone())?;
                    if matches!(op, BinaryOp::Ne) {
                        buf.push((&BinaryOp::Eq).into());
                        buf.push((&UnaryOp::Not).into());
                    } else {
                        buf.push(op.into());
                    }
                }

                Node::Unary { op, expr } => {
                    compile_(buf, expr.clone())?;
                    buf.push(op.into());
                }

                _ => buf.push(node.as_ref().into()),
            }

            Ok(())
        }

        let mut buf = Vec::new();
        if compile_(&mut buf, node).is_ok() {
            Ok(Machine {
                instr: buf,
                mem: RefCell::new(Vec::new()),
            })
        } else {
            Err("Could not compile".into())
        }
    }

    pub fn eval(&self, a: Rc<dyn Accessor>) -> bool {
        fn eval_(mach: &Machine, a: Rc<dyn Accessor>) -> Option<bool> {
            let mut mem = mach.mem.borrow_mut();
            mem.clear();

            for i in mach.instr.iter() {
                match i {
                    Instr::LoadIdent(x) => {
                        if a.is_int(x) {
                            mem.push(Value::Int(a.get_num(x)?))
                        } else if a.is_str(x) {
                            mem.push(Value::Str(a.get_str(x)?))
                        } else {
                            mem.push(Value::Nil)
                        }
                    }

                    Instr::LoadString(x) => mem.push(Value::Str(x.clone())),
                    Instr::LoadNum(x) => mem.push(Value::Int(*x)),
                    Instr::LoadRe(x) => mem.push(Value::Re(x.clone())),
                    Instr::LoadNil => mem.push(Value::Nil),

                    Instr::Eq => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l == r).into());
                    }

                    Instr::Gt => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l.as_int() > r.as_int()).into());
                    }
                    Instr::Ge => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l.as_int() >= r.as_int()).into());
                    }
                    Instr::Lt => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l.as_int() < r.as_int()).into());
                    }
                    Instr::Le => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l.as_int() <= r.as_int()).into());
                    }

                    // Yes, there is no short-circuit here...
                    Instr::And => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l.as_bool() && r.as_bool()).into());
                    }
                    Instr::Or => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l.as_bool() || r.as_bool()).into());
                    }

                    Instr::BAnd => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l.as_int() & r.as_int()).into());
                    }
                    Instr::Match => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((r.re_matches(l.as_str())).into());
                    }

                    Instr::Not => {
                        let e = mem.pop()?;
                        mem.push((!e.as_bool()).into());
                    }
                }
            }

            mem.pop().map(|x| x.as_bool())
        }

        match eval_(self, a) {
            Some(r) => r,
            _ => {
                eprintln!("Could not evaluate expression");
                false
            }
        }
    }
}
