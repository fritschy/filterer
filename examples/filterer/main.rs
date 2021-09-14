use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::fmt::{Display, Formatter};
use std::io;

use std::rc::Rc;

use filterer::{eval::Accessor, machine::Machine, parser};

#[derive(Clone)]
struct Message {
    ts: usize,
    flags: usize,
    ctx: Rc<String>,
    app: Rc<String>,
    level: usize,
}

mod sw;
use sw::Stopwatch;

impl Display for Message {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[T {:<3} F 0x{:04x} C'{}' A'{}' L {}]",
            self.ts, self.flags, self.ctx, self.app, self.level
        )
    }
}

impl Accessor for Message {
    fn get_str(&self, k: &str) -> Option<Rc<String>> {
        match k {
            "ctx" => Some(self.ctx.clone()),
            "app" => Some(self.app.clone()),
            _ => None,
        }
    }
    fn get_num(&self, k: &str) -> Option<isize> {
        match k {
            "flags" => Some(self.flags as isize),
            "ts" => Some(self.ts as isize),
            "level" => Some(self.level as isize),
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
        }),
        Rc::new(Message {
            ts: 100,
            flags: 0x301,
            ctx: Rc::new(String::from("render")),
            app: Rc::new(String::from("HMI1")),
            level: 0,
        }),
        Rc::new(Message {
            ts: 101,
            flags: 0x201,
            ctx: Rc::new(String::from("menu")),
            app: Rc::new(String::from("HMI")),
            level: 3,
        }),
        Rc::new(Message {
            ts: 200,
            flags: 0x300,
            ctx: Rc::new(String::from("map")),
            app: Rc::new(String::from("MAP")),
            level: 1,
        }),
        Rc::new(Message {
            ts: 300,
            flags: 0x004,
            ctx: Rc::new(String::from("intersection")),
            app: Rc::new(String::from("SideMAP")),
            level: 1,
        }),
    ]
}

fn doit(l: &str, bench: bool) {
    if let Err(e) = parser::parse(l.trim()).map(|x| {
        let c = Machine::from_node(x.as_ref());

        if !bench {
            // println!("Got: {:#?}", x.as_ref());
            println!("Code:\n{}", c);
        }

        let mut count = 0;
        let max = if bench { 1_000_000 } else { 1 };
        let msgs = messages();
        for _i in 0..max {
            for m in msgs.iter() {
                if c.eval(m.as_ref()) {
                    count += 1;
                    if !bench {
                        println!("{}", m);
                    }
                }
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
                    let sw = Stopwatch::new();
                    doit(&l, bench);
                    let t = sw.elapsed();
                    if bench {
                        println!("Took {:?}", t);
                    }
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
        let sw = Stopwatch::new();
        doit("flags == 0x300 || ts < 200", bench);
        if !bench {
            println!("{}", "-".repeat(41));
        }
        doit("flags & 0x100 != 0b0 && ts <= 0o10101", bench);
        if !bench {
            println!("{}", "-".repeat(41));
        }
        doit("(((((((((((((((1)))))))))))))))", bench);
        let t = sw.elapsed();
        if bench {
            println!("Took {:?}", t);
        }
    }

    Ok(())
}
