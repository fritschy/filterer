mod old {
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
            let blen = buf.len();
            Ok(Machine {
                instr: buf,
                mem: RefCell::new(Vec::with_capacity(blen)),
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
                        if let Some(i) = a.get_num(x) {
                            mem.push(Value::Int(i))
                        } else if let Some(s) = a.get_str(x) {
                            mem.push(Value::Str(s))
                        } else {
                            mem.push(Value::Nil)
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
}

mod ng {
    use crate::eval::{Value, Accessor};
    use crate::parser::{Node, BinaryOp, UnaryOp};
    use std::rc::Rc;
    use regex::Regex;
    use std::fmt;
    use std::cell::RefCell;

    mod ops {
        pub const AND: u16 = 0x00;
        pub const OR: u16 = 0x01;

        pub const EQ: u16 = 0x02;
        pub const NE: u16 = 0x03;

        pub const LT: u16 = 0x04;
        pub const LE: u16 = 0x05;
        pub const GT: u16 = 0x06;
        pub const GE: u16 = 0x07;

        pub const BAND: u16 = 0x08;
        pub const MTCH: u16 = 0x09;

        pub const NOT: u16 = 0x0a;

        pub const LOADNIL: u16 = 0x0b;
        pub const LOADINT: u16 = 0x0c;
        pub const LOADSTR: u16 = 0x0d;
        pub const LOADRE: u16 = 0x0e;
        pub const LOADIDENT: u16 = 0x0f;
    }

    pub struct Machine {
        code: Vec<u16>,
        strings: Vec<Rc<String>>,
        idents: Vec<Rc<String>>,
        regexes: Vec<Rc<Regex>>,
        nums: Vec<isize>,
        stack: RefCell<Vec<Value>>,
    }

    impl fmt::Display for Machine {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            for instr in self.code.iter() {
                let opc = instr & 0xff;
                let arg = instr >> 8;
                let arg = arg as usize;

                write!(f, "{:04x}  ", *instr)?;

                write!(f, "{}", opc_to_str(opc as u8))?;

                match opc {
                    ops::LOADINT => write!(f, " {}", self.nums[arg])?,
                    ops::LOADSTR => write!(f, " \"{}\"", self.strings[arg])?,
                    ops::LOADRE => write!(f, " {:?}", self.regexes[arg])?,
                    ops::LOADIDENT => write!(f, " {}", self.idents[arg])?,
                    _ => (),
                }

                writeln!(f)?;
            }
            Ok(())
        }
    }

    fn opc_to_str(opc: u8) -> &'static str {
        match opc as u16 {
            ops::AND => "and",
            ops::OR => "or",
            ops::EQ => "eq",
            ops::NE => "ne",

            ops::LT => "lt",
            ops::LE => "le",
            ops::GT => "gt",
            ops::GE => "ge",

            ops::BAND => "band",
            ops::MTCH => "match",

            ops::NOT => "not",

            ops::LOADNIL => "load nil",
            ops::LOADINT => "load int",
            ops::LOADSTR => "load str",
            ops::LOADRE => "load re",
            ops::LOADIDENT => "load ident",

            _ => unreachable!(),
        }
    }

    fn binary_op_as_code(op: &BinaryOp) -> u16 {
        match op {
            BinaryOp::And => ops::AND,
            BinaryOp::Or => ops::OR,
            BinaryOp::Eq => ops::EQ,
            BinaryOp::Ne => ops::NE,
            BinaryOp::Lt => ops::LT,
            BinaryOp::Le => ops::LE,
            BinaryOp::Gt => ops::GT,
            BinaryOp::Ge => ops::GE,
            BinaryOp::Band => ops::BAND,
            BinaryOp::Match => ops::MTCH,
        }
    }

    fn unary_op_as_code(op: &UnaryOp) -> u16 {
        match op {
            UnaryOp::Not => ops::NOT,
        }
    }

    impl Default for Machine {
        fn default() -> Self {
            Self {
                code: Vec::with_capacity(32),
                strings: Vec::with_capacity(16),
                idents: Vec::with_capacity(16),
                regexes: Vec::with_capacity(16),
                nums: Vec::with_capacity(16),
                stack: RefCell::new(Vec::with_capacity(16)),
            }
        }
    }

    impl Machine {
        pub fn from_node(node: Rc<Node>) -> Result<Self, String> {
            Self::from_ast(node)
        }

        pub fn from_ast(node: Rc<Node>) -> Result<Self, String> {
            fn compile(mach: &mut Machine, node: &Node) -> Result<(), String> {
                match node {
                    Node::Binary { lhs, op, rhs } => {
                        compile(mach, lhs.as_ref())?;
                        compile(mach, rhs.as_ref())?;
                        mach.code.push(binary_op_as_code(op));
                    }
                    Node::Unary { op, expr } => {
                        compile(mach, expr.as_ref())?;
                        mach.code.push(unary_op_as_code(op));
                    }
                    Node::Constant(num) => {
                        let opc = ops::LOADINT;
                        let index = mach.nums.len();
                        assert!(index < 0x100);
                        mach.nums.push(*num);
                        mach.code.push(opc | (index as u16) << 8);
                    }
                    Node::StringLiteral(s) => {
                        let opc = ops::LOADSTR;
                        let index = mach.strings.len();
                        assert!(index < 0x100);
                        mach.strings.push(s.clone());
                        mach.code.push(opc | (index as u16) << 8);
                    }
                    Node::Regexp(re) => {
                        let opc = ops::LOADRE;
                        let index = mach.regexes.len();
                        assert!(index < 0x100);
                        mach.regexes.push(re.clone());
                        mach.code.push(opc | (index as u16) << 8);
                    }
                    Node::Identifier(ident) => {
                        let opc = ops::LOADIDENT;
                        let index = mach.idents.len();
                        assert!(index < 0x100);
                        mach.idents.push(ident.clone());
                        mach.code.push(opc | (index as u16) << 8);
                    }
                    Node::Nil => {
                        mach.code.push(ops::LOADNIL);
                    }
                }

                Ok(())
            }

            let mut mach = Machine::default();

            compile(&mut mach, node.as_ref())?;

            Ok(mach)
        }

        pub fn run(&self, a: &dyn Accessor) -> Option<Value> {
            let mut stack = self.stack.borrow_mut();
            stack.clear();

            for instr in self.code.iter() {
                let opc = *instr & 0xff;
                let arg = *instr >> 8;
                let arg = arg as usize;

                match opc {
                    ops::AND => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((r.as_bool() && l.as_bool()).into());
                    }
                    ops::OR => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((r.as_bool() || l.as_bool()).into());
                    }
                    ops::EQ => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((l == r).into());
                    }
                    ops::NE => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((l != r).into());
                    }

                    ops::LT => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((l.as_int() < r.as_int()).into());
                    }
                    ops::LE => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((l.as_int() <= r.as_int()).into());
                    }
                    ops::GT => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((l.as_int() > r.as_int()).into());
                    }
                    ops::GE => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((l.as_int() >= r.as_int()).into());
                    }

                    ops::BAND => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((l.as_int() & r.as_int()).into());
                    }
                    ops::MTCH => {
                        let r = stack.pop()?;
                        let l = stack.pop()?;
                        stack.push((r.re_matches(l.as_str())).into());
                    }

                    ops::NOT => {
                        let e = stack.pop()?;
                        stack.push((!e.as_bool()).into());
                    }

                    ops::LOADNIL => stack.push(Value::Nil),
                    ops::LOADINT => stack.push(Value::Int(self.nums[arg].clone())),
                    ops::LOADSTR => stack.push(Value::Str(self.strings[arg].clone())),
                    ops::LOADRE => stack.push(Value::Re(self.regexes[arg].clone())),
                    ops::LOADIDENT => {
                        let x = self.idents[arg].clone();
                        if let Some(i) = a.get_num(x.as_ref()) {
                            stack.push(Value::Int(i))
                        } else if let Some(s) = a.get_str(x.as_ref()) {
                            stack.push(Value::Str(s))
                        } else {
                            stack.push(Value::Nil)
                        }
                    }

                    _ => eprintln!("Invalid opcode 0x{:04x}", instr),   // so ... can we get away with this?!
                }
            }

            assert!(stack.len() == 1);

            stack.pop()
        }

        pub fn eval(&self, a: Rc<dyn Accessor>) -> bool {
            self.run(a.as_ref()).unwrap_or(Value::Nil).as_bool()
        }
    }
}

pub use ng::Machine as Machine;
