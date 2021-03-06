use std::cell::RefCell;
use std::collections::BTreeMap;
use std::fmt;
use std::rc::Rc;

use regex::Regex;

use crate::parser::{BinaryOp, Node, UnaryOp};
use crate::value::Value;

pub trait KeyAccessor {
    fn get_str(&self, k: usize, i: usize) -> Option<Rc<String>>;
    fn get_num(&self, k: usize, i: usize) -> Option<isize>;
    fn get_len(&self, k: usize) -> Option<isize>;
}

pub trait AccessorQuery {
    fn get_ident(&self, name: &str) -> Option<usize>;
}

enum Instr {
    LoadIdent(usize),
    LoadIndexIdent(usize, usize),
    LoadArrayIdentLen(usize),

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

impl fmt::Display for NamedInstr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Instr::LoadIdent(x) => {
                let s = format!("load ident {}", x);
                debug_assert!(self.1.contains_key(x));
                write!(f, "{:18} # {}", s, self.1[x])
            }
            Instr::LoadIndexIdent(x, i) => {
                let s = format!("load ident {}[{}]", x, i);
                debug_assert!(self.1.contains_key(x));
                write!(f, "{:18} # {}", s, self.1[x])
            }
            Instr::LoadArrayIdentLen(x) => {
                let s = format!("load ident {}.len", x);
                debug_assert!(self.1.contains_key(x));
                write!(f, "{:18} # {}", s, self.1[x])
            }

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
            BinaryOp::Ne => unreachable!("ne operator needs to be mapped to !eq"),
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

impl Instr {
    fn from_terminal_node(node: &Node) -> Self {
        match node {
            Node::StringLiteral(s) => Instr::LoadString(s.clone()),
            Node::Constant(i) => Instr::LoadNum(*i),
            Node::Regexp(r) => Instr::LoadRe(r.clone()),
            Node::Nil => Instr::LoadNil,
            _ => panic!("Unexpected node type"),
        }
    }
}

pub(crate) struct Machine {
    instr: Vec<Instr>,
    mem: RefCell<Vec<Value>>,
    ident_names: BTreeMap<usize, String>,
}

// Solely used for formatting instructions
struct NamedInstr<'a>(&'a Instr, &'a BTreeMap<usize, String>);

impl fmt::Display for Machine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.instr
            .iter()
            .try_for_each(|instr| writeln!(f, "{}", NamedInstr(instr, &self.ident_names)))
    }
}

impl Machine {
    pub(crate) fn from_node_and_accessor(node: &Node, acc: &dyn AccessorQuery) -> Machine {
        fn compile_(
            buf: &mut Vec<Instr>,
            ident_names: &mut BTreeMap<usize, String>,
            acc: &dyn AccessorQuery,
            node: &Node,
        ) {
            match node {
                Node::Binary { rhs, op, lhs } => {
                    // This needs to be reversed for eval.
                    compile_(buf, ident_names, acc, lhs.as_ref());
                    compile_(buf, ident_names, acc, rhs.as_ref());
                    if matches!(op, BinaryOp::Ne) {
                        buf.push(BinaryOp::Eq.into());
                        buf.push(UnaryOp::Not.into());
                    } else {
                        buf.push((*op).into());
                    }
                }

                Node::Unary { op, expr } => {
                    compile_(buf, ident_names, acc, expr.as_ref());
                    buf.push((*op).into());
                }

                Node::Identifier(x) => {
                    if let Some(ik) = acc.get_ident(x) {
                        ident_names.entry(ik).or_insert_with(|| x.to_string());
                        buf.push(Instr::LoadIdent(ik))
                    } else {
                        eprintln!("Could not lookup ident '{}', emitting 'load nil'", x);
                        buf.push(Instr::LoadNil)
                    }
                }

                Node::IndexedIdentifier(x, i) => {
                    if let Some(ik) = acc.get_ident(x) {
                        ident_names.entry(ik).or_insert_with(|| x.to_string());
                        buf.push(Instr::LoadIndexIdent(ik, *i))
                    } else {
                        eprintln!("Could not lookup index ident '{}', emitting 'load nil'", x);
                        buf.push(Instr::LoadNil)
                    }
                }

                Node::ArrayIdentifierLen(x) => {
                    if let Some(ik) = acc.get_ident(x) {
                        ident_names.entry(ik).or_insert_with(|| x.to_string());
                        buf.push(Instr::LoadArrayIdentLen(ik))
                    } else {
                        eprintln!(
                            "Could not lookup array len ident '{}', emitting 'load nil'",
                            x
                        );
                        buf.push(Instr::LoadNil)
                    }
                }

                _ => buf.push(Instr::from_terminal_node(node)),
            }
        }

        let mut buf = Vec::with_capacity(32);
        let mut ident_names = BTreeMap::new();

        compile_(&mut buf, &mut ident_names, acc, node);

        let max_depth = Self::max_depth(&buf);

        Machine {
            instr: buf,
            mem: RefCell::new(vec![Value::Nil; max_depth]),
            ident_names,
        }
    }

    fn max_depth(buf: &[Instr]) -> usize {
        buf.iter()
            .fold((0isize, 0), |(depth, max), i| {
                let depth = depth
                    + match i {
                        // Pop 2, Push 1
                        Instr::And
                        | Instr::Or
                        | Instr::Match
                        | Instr::BAnd
                        | Instr::Ge
                        | Instr::Le
                        | Instr::Gt
                        | Instr::Lt
                        | Instr::Eq => -1,

                        // Pop 1, Push 1
                        Instr::Not => 0,

                        // All loads push 1
                        Instr::LoadIdent(_)
                        | Instr::LoadIndexIdent(_, _)
                        | Instr::LoadArrayIdentLen(_)
                        | Instr::LoadString(_)
                        | Instr::LoadNum(_)
                        | Instr::LoadRe(_)
                        | Instr::LoadNil => 1,
                    };

                (depth, max.max(depth as usize))
            })
            .1
    }

    pub(crate) fn eval<T: KeyAccessor>(&self, a: &T) -> bool {
        fn eval_<T: KeyAccessor>(mach: &Machine, a: &T) -> bool {
            let mut mem = mach.mem.borrow_mut();
            let stack_size = mem.len();
            let mut cur = 0;
            let ptr = mem.as_mut_ptr();

            macro_rules! push {
                ($e:expr) => {
                    {
                        debug_assert!((cur as usize) < stack_size);
                        unsafe {
                            let ptr = ptr.add(cur);
                            cur += 1;
                            ptr.write($e);
                        }
                    }
                };
            }

            macro_rules! pop {
                () => {{
                    debug_assert!(cur != 0);
                    unsafe {
                        cur -= 1;
                        let ptr = ptr.add(cur);
                        let x = ptr.read();
                        ptr.write(Value::Nil);
                        x
                    }
                }};
            }

            for i in mach.instr.iter() {
                match i {
                    Instr::LoadIdent(x) => {
                        if let Some(i) = a.get_num(*x, 0) {
                            push!(Value::Int(i))
                        } else if let Some(s) = a.get_str(*x, 0) {
                            push!(Value::Str(s))
                        } else {
                            push!(Value::Nil)
                        }
                    }
                    Instr::LoadIndexIdent(x, i) => {
                        if let Some(j) = a.get_num(*x, *i) {
                            push!(Value::Int(j))
                        } else if let Some(s) = a.get_str(*x, *i) {
                            push!(Value::Str(s))
                        } else {
                            push!(Value::Nil)
                        }
                    }
                    Instr::LoadArrayIdentLen(x) => {
                        if let Some(l) = a.get_len(*x) {
                            push!(Value::Int(l));
                        } else {
                            push!(Value::Nil);
                        }
                    }

                    Instr::LoadString(x) => push!(Value::Str(x.clone())),
                    Instr::LoadNum(x) => push!(Value::Int(*x)),
                    Instr::LoadRe(x) => push!(Value::Re(x.clone())),
                    Instr::LoadNil => push!(Value::Nil),

                    Instr::Not => {
                        let e = pop!();
                        push!((!e.as_bool()).into());
                    }

                    Instr::Eq => {
                        let r = pop!();
                        let l = pop!();
                        push!((l == r).into());
                    }

                    Instr::Gt => {
                        let r = pop!();
                        let l = pop!();
                        push!((l > r).into());
                    }
                    Instr::Ge => {
                        let r = pop!();
                        let l = pop!();
                        push!((l >= r).into());
                    }
                    Instr::Lt => {
                        let r = pop!();
                        let l = pop!();
                        push!((l < r).into());
                    }
                    Instr::Le => {
                        let r = pop!();
                        let l = pop!();
                        push!((l <= r).into());
                    }

                    // Yes, there is no short-circuit here...
                    Instr::And => {
                        let r = pop!();
                        let l = pop!();
                        push!((l.as_bool() && r.as_bool()).into());
                    }
                    Instr::Or => {
                        let r = pop!();
                        let l = pop!();
                        push!((l.as_bool() || r.as_bool()).into());
                    }

                    Instr::BAnd => {
                        let r = pop!();
                        let l = pop!();
                        push!((l.as_int() & r.as_int()).into());
                    }

                    // We do not want to fuzz the regex crate.
                    #[cfg(feature = "fuzz")]
                    Instr::Match => {
                        let _r = pop!();
                        let _l = pop!();
                        push!(false.into());
                    }

                    #[cfg(not(feature = "fuzz"))]
                    Instr::Match => {
                        let r = pop!();
                        let l = pop!();
                        if matches!(l, Value::Str(_)) {
                            push!((r.re_matches(l.as_str())).into());
                        } else {
                            push!(false.into());
                        }
                    }
                }
            }

            pop!().as_bool()
        }

        eval_(self, a)
    }
}
