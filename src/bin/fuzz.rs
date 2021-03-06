#[macro_use]
extern crate afl;

use std::fmt::{Display, Formatter};
use std::rc::Rc;

use filterer::{AccessorQuery, compile, KeyAccessor};

#[derive(Clone)]
struct Message {
    ts: usize,
    flags: usize,
    ctx: Rc<String>,
    app: Rc<String>,
    level: usize,
    args: Vec<usize>,
}

impl Display for Message {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[T {:<3} F 0x{:04x} C'{}' A'{}' L {} A {:?}]",
            self.ts, self.flags, self.ctx, self.app, self.level, self.args
        )
    }
}

mod keys {
    pub const CTX: usize = 0;
    pub const APP: usize = 1;
    pub const FLAGS: usize = 2;
    pub const TS: usize = 3;
    pub const LEVEL: usize = 4;
    pub const ARGS: usize = 5;
}

impl KeyAccessor for Message {
    fn get_str(&self, k: usize, _i: usize) -> Option<Rc<String>> {
        match k {
            keys::CTX => Some(self.ctx.clone()),
            keys::APP => Some(self.app.clone()),
            _ => None,
        }
    }
    fn get_num(&self, k: usize, i: usize) -> Option<isize> {
        if k == keys::ARGS {
            return if i < self.args.len() {
                Some(self.args[i] as isize)
            } else {
                None
            };
        }

        match k {
            keys::FLAGS => Some(self.flags as isize),
            keys::TS => Some(self.ts as isize),
            keys::LEVEL => Some(self.level as isize),
            _ => None,
        }
    }
    fn get_len(&self, k: usize) -> Option<isize> {
        match k {
            keys::ARGS => Some(self.args.len() as isize),
            _ => None,
        }
    }
}

struct MessageQuery;

impl AccessorQuery for MessageQuery {
    fn get_ident(&self, name: &str) -> Option<usize> {
        match name {
            "ctx" => Some(keys::CTX),
            "app" => Some(keys::APP),
            "flags" => Some(keys::FLAGS),
            "ts" => Some(keys::TS),
            "level" => Some(keys::LEVEL),
            "args" => Some(keys::ARGS),
            _ => None,
        }
    }
}

fn messages() -> Vec<Rc<Message>> {
    vec![
        Rc::new(Message {
            ts: 0,
            flags: 0x300,
            ctx: Rc::new(String::from("render")),
            app: Rc::new(String::from("HMI2")),
            level: 0,
            args: vec![47, 11],
        }),
        Rc::new(Message {
            ts: 100,
            flags: 0x301,
            ctx: Rc::new(String::from("render")),
            app: Rc::new(String::from("HMI1")),
            level: 0,
            args: vec![42, 16, 29, 34],
        }),
        Rc::new(Message {
            ts: 101,
            flags: 0x201,
            ctx: Rc::new(String::from("menu")),
            app: Rc::new(String::from("HMI")),
            level: 3,
            args: vec![0, 1, 2],
        }),
        Rc::new(Message {
            ts: 200,
            flags: 0x300,
            ctx: Rc::new(String::from("map")),
            app: Rc::new(String::from("MAP")),
            level: 1,
            args: Vec::new(),
        }),
        Rc::new(Message {
            ts: 300,
            flags: 0x004,
            ctx: Rc::new(String::from("intersection")),
            app: Rc::new(String::from("SideMAP")),
            level: 1,
            args: vec![0, 8, 15],
        }),
    ]
}

fn doit(l: &str) {
    if let Err(e) = compile(l, &MessageQuery).map(|c| {
        println!("Code:\n{}", c);

        let mut count = 0;
        let msgs = messages();
        for m in msgs.iter() {
            if c.eval(m.as_ref()) {
                count += 1;
            }
        }

        println!("matched {}/{} messages", count, messages().len());
    }) {
        eprintln!("{}", e);
    }
}

fn main() {
    fuzz!(|data: &[u8]| {
        let text = String::from_utf8_lossy(data);
        doit(&text);
    });
}
