use filterer::nom_parser;

use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::io;

use filterer::eval::{Accessor, Eval};

#[cfg(feature = "pest_parser")]
pub use filterer::pest_parser;
#[cfg(feature = "pest_parser")]
pub use pest::Parser;
use std::fmt::{Display, Formatter};

struct Message<'a> {
    ts: usize,
    flags: usize,
    ctx: &'a str,
    app: &'a str,
    level: usize,
}

mod sw;
use sw::Stopwatch;

mod machine;

impl<'a> Display for Message<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[T {:<3} F 0x{:04x} C'{}' A'{}' L {}]",
            self.ts, self.flags, self.ctx, self.app, self.level
        )
    }
}

impl<'a> Accessor for Message<'a> {
    fn get_str(&self, k: &str) -> Result<&'a str, String> {
        Ok(match k {
            "ctx" => self.ctx,
            "app" => self.app,
            _ => return Err(format!("No such str {}", k)),
        })
    }
    fn get_num(&self, k: &str) -> Result<isize, String> {
        Ok(match k {
            "flags" => self.flags as isize,
            "ts" => self.ts as isize,
            "level" => self.level as isize,
            _ => return Err(format!("No such num {}", k)),
        })
    }
}

fn messages() -> Vec<Message<'static>> {
    vec![
        Message {
            ts: 0,
            flags: 0x300,
            ctx: "render",
            app: "HMI2",
            level: 0,
        },
        Message {
            ts: 100,
            flags: 0x301,
            ctx: "render",
            app: "HMI1",
            level: 0,
        },
        Message {
            ts: 101,
            flags: 0x201,
            ctx: "menu",
            app: "HMI",
            level: 3,
        },
        Message {
            ts: 200,
            flags: 0x300,
            ctx: "map",
            app: "MAP",
            level: 1,
        },
        Message {
            ts: 300,
            flags: 0x004,
            ctx: "intersection",
            app: "SideMAP",
            level: 1,
        },
    ]
}

fn doit(l: &str, bench: bool) {
    if let Err(e) = nom_parser::parse(l.trim()).map(|x| {
        let c = machine::Machine::from_node(&x).unwrap();

        if !bench {
            // println!("Got: {:#?}", x.as_ref());
            println!("Code:\n{}", c);
        }

        let mut count = 0;
        let max = if bench { 1_000_000 } else { 1 };
        let msgs = messages();
        for _i in 0..max {
            for m in msgs.iter() {
                //println!("CodeEval: {}", c.eval(m));
                //if x.eval_filter(m) {
                if c.eval(m) {
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

    #[cfg(feature = "pest_parser")]
    {
        if let Err(e) = pest_parser::Filter::parse(pest_parser::Rule::expr, l.trim()).map(|x| {
            println!("pest_parser, Got: {}", x);
        }) {
            eprintln!("Got error: {:?}", e);
        }
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
