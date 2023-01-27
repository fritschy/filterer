use std::fmt::{Display, Formatter};
use std::io;
use std::sync::Arc;
use std::time::Instant;

use rustyline::Editor;
use rustyline::error::ReadlineError;

use filterer::{AccessorQuery, compile, KeyAccessor};

#[derive(Clone)]
struct Message {
    ts: usize,
    flags: usize,
    ctx: Arc<String>,
    app: Arc<String>,
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
    fn get_str(&self, k: usize, _i: usize) -> Option<Arc<String>> {
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

fn messages() -> Vec<Arc<Message>> {
    vec![
        Arc::new(Message {
            ts: 0,
            flags: 0x300,
            ctx: Arc::new(String::from("render")),
            app: Arc::new(String::from("HMI2")),
            level: 0,
            args: vec![47, 11],
        }),
        Arc::new(Message {
            ts: 100,
            flags: 0x301,
            ctx: Arc::new(String::from("render")),
            app: Arc::new(String::from("HMI1")),
            level: 0,
            args: vec![42, 16, 29, 34],
        }),
        Arc::new(Message {
            ts: 101,
            flags: 0x201,
            ctx: Arc::new(String::from("menu")),
            app: Arc::new(String::from("HMI")),
            level: 3,
            args: vec![0, 1, 2],
        }),
        Arc::new(Message {
            ts: 200,
            flags: 0x300,
            ctx: Arc::new(String::from("map")),
            app: Arc::new(String::from("MAP")),
            level: 1,
            args: Vec::new(),
        }),
        Arc::new(Message {
            ts: 300,
            flags: 0x004,
            ctx: Arc::new(String::from("intersection")),
            app: Arc::new(String::from("SideMAP")),
            level: 1,
            args: vec![0, 8, 15],
        }),
    ]
}

fn doit(l: &str, bench: bool) {
    if let Err(e) = compile(l.trim(), &MessageQuery).map(|c| {
        if !bench {
            println!("{}", l);
            println!("Code:\n{}", c);
        }

        let mut count = 0;
        let mut allcount = 0;
        let max = if bench { 1_000_000 } else { 1 };
        let msgs = messages();
        for _i in 0..max {
            for m in msgs.iter() {
                if c.eval(m.as_ref()) {
                    if !bench {
                        println!("{} {}", allcount, m);
                    }
                    count += 1;
                }
                allcount += 1;
            }
        }

        println!("matched {}/{} messages", count, messages().len());
    }) {
        eprintln!("{}", e);
    }
}

fn main() -> io::Result<()> {
    let bench = std::env::args().any(|x| x == "--benchmark" || x == "-b");

    if std::env::args().nth(1).unwrap_or_else(|| "".to_string()) == "-i" {
        // `()` can be used when no completer is required
        let mut rl = Editor::<()>::new();
        if rl.load_history("history.txt").is_err() {}

        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(l) => {
                    rl.add_history_entry(l.as_str());
                    let sw = Instant::now();
                    doit(&l, bench);
                    let t = sw.elapsed();
                    println!("Took {:?}", t);
                }
                Err(ReadlineError::Interrupted) => {
                    break;
                }
                Err(ReadlineError::Eof) => {
                    break;
                }
                Err(err) => {
                    eprintln!("Error: {:?}", err);
                    break;
                }
            }
        }

        rl.save_history("history.txt").unwrap();
    } else {
        const EXPRS: &[&str] = &[
            "flags == 0x300 || ts < 200",
            "flags & 0x100 != 0b0 && ts <= 0o10101",
            "(((((((((((((((1)))))))))))))))",
            "args.len && (args[3] > 0 || (args[2] & 1) || !(args[0] & 1)) && level >= 1 && level < 3 && app =~ !/HMI./",
        ];

        let sw = Instant::now();
        for &expr in EXPRS.iter().take(if bench { 3 } else { EXPRS.len() }) {
            if !bench {
                println!("\n{}", "-".repeat(41));
            }
            doit(expr, bench);
        }
        let t = sw.elapsed();
        println!("Took {:?}", t);
    }

    Ok(())
}
