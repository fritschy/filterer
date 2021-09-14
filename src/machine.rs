use crate::eval::{Accessor, Value};
use crate::parser::{BinaryOp, Node, UnaryOp};

use regex::Regex;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

pub enum Instr {
    LoadIdent(Rc<String>),
    LoadIndexIdent(Rc<String>, usize),
    LoadArrayIdentLen(Rc<String>),
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

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::LoadIdent(x) => write!(f, "load ident {}", *x),
            Instr::LoadIndexIdent(x, i) => write!(f, "load ident {}[{}]", *x, *i),
            Instr::LoadArrayIdentLen(x) => write!(f, "load ident {}.len", *x),
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

impl From<BinaryOp> for Instr {
    fn from(op: BinaryOp) -> Self {
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

impl From<UnaryOp> for Instr {
    fn from(op: UnaryOp) -> Self {
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
            Node::IndexedIdentifier(s, i) => Instr::LoadIndexIdent(s.clone(), *i),
            Node::ArrayIdentifierLen(s) => Instr::LoadArrayIdentLen(s.clone()),
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

impl fmt::Display for Machine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.instr.iter().try_for_each(|instr| {
            writeln!(f, "{}", instr)
        })
    }
}

impl Machine {
    pub fn from_node(node: &Node) -> Machine {
        fn compile_(buf: &mut Vec<Instr>, node: &Node) {
            match node {
                Node::Binary { rhs, op, lhs } => {
                    // This needs to be reversed for eval.
                    compile_(buf, lhs.as_ref());
                    compile_(buf, rhs.as_ref());
                    if matches!(op, BinaryOp::Ne) {
                        buf.push(BinaryOp::Eq.into());
                        buf.push(UnaryOp::Not.into());
                    } else {
                        buf.push((*op).into());
                    }
                }

                Node::Unary { op, expr } => {
                    compile_(buf, expr.as_ref());
                    buf.push((*op).into());
                }

                _ => buf.push(node.into()),
            }
        }

        let mut buf = Vec::with_capacity(32);
        compile_(&mut buf, node);
        Machine {
            instr: buf,
            mem: RefCell::new(Vec::with_capacity(16)),
        }
    }

    pub fn eval(&self, a: &dyn Accessor) -> bool {
        fn eval_(mach: &Machine, a: &dyn Accessor) -> Option<bool> {
            let mut mem = mach.mem.borrow_mut();
            mem.clear();

            for i in mach.instr.iter() {
                match i {
                    Instr::LoadIdent(x) => {
                        if let Some(i) = a.get_num(x, 0) {
                            mem.push(Value::Int(i))
                        } else if let Some(s) = a.get_str(x, 0) {
                            mem.push(Value::Str(s))
                        } else {
                            mem.push(Value::Nil)
                        }
                    }
                    Instr::LoadIndexIdent(x, i) => {
                        if let Some(i) = a.get_num(x, *i) {
                            mem.push(Value::Int(i))
                        } else if let Some(s) = a.get_str(x, *i) {
                            mem.push(Value::Str(s))
                        } else {
                            mem.push(Value::Nil)
                        }
                    }
                    Instr::LoadArrayIdentLen(x) => {
                        if let Some(l) = a.get_len(x) {
                            mem.push(Value::Int(l));
                        } else {
                            mem.push(Value::Nil);
                        }
                    }

                    Instr::LoadString(x) => mem.push(Value::Str(x.clone())),
                    Instr::LoadNum(x) => mem.push(Value::Int(*x)),
                    Instr::LoadRe(x) => mem.push(Value::Re(x.clone())),
                    Instr::LoadNil => mem.push(Value::Nil),

                    Instr::Not => {
                        let e = mem.pop()?;
                        mem.push((!e.as_bool()).into());
                    }

                    Instr::Eq => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l == r).into());
                    }

                    Instr::Gt => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l > r).into());
                    }
                    Instr::Ge => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l >= r).into());
                    }
                    Instr::Lt => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l < r).into());
                    }
                    Instr::Le => {
                        let r = mem.pop()?;
                        let l = mem.pop()?;
                        mem.push((l <= r).into());
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
                        if matches!(l, Value::Str(_)) {
                            mem.push((r.re_matches(l.as_str())).into());
                        } else {
                            mem.push(false.into());
                        }
                    }
                }
            }

            assert_eq!(mem.len(), 1);

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
