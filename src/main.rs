use tracing_subscriber::FmtSubscriber;
use tracing::{Level, info, error as log_err};
use filterer::nom_parser;

use std::io;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use filterer::eval::{Eval, Accessor};

#[cfg(feature = "pest")]
pub use filterer::pest_parser;

pub use pest::Parser;

#[derive(Debug, Clone)]
struct Message<'a> {
    ts: usize,
    flags: usize,
    ctx: &'a str,
    app: &'a str,
    level: usize,
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
        Message { ts: 0, flags: 0x300, ctx: "render", app: "HMI2", level: 0 },
        Message { ts: 100, flags: 0x301, ctx: "render", app: "HMI1", level: 0 },
        Message { ts: 101, flags: 0x201, ctx: "menu",   app: "HMI",  level: 3 },
        Message { ts: 200, flags: 0x300, ctx: "map",    app: "MAP",  level: 1 },
        Message { ts: 300, flags: 0x004, ctx: "intersection", app: "SideMAP", level: 1 },
    ]
}

fn doit(l: &str) {
    if let Err(e) = nom_parser::parse(l.trim()).map(|x| {
        info!("Got: {:#?}", x.as_ref());
        let mut count = 0;
        //for i in 0..1_000_000 {
            for m in messages().iter() {
                if x.eval_filter(m) {
                    count += 1;
                    info!("{:?}", m);
                }
            }
        //}

        info!("matched {}/{} messages", count, messages().len());
    }) {
        log_err!("{}", e);
    }

    #[cfg(feature = "pest")]
    {
        if let Err(e) = pest_parser::Filter::parse(pest_parser::Rule::expr, l.trim()).map(|_x| {
            //info!("Got: {}", x);
        }) {
            log_err!("Got error: {:?}", e);
        }
    }
}

fn main() -> io::Result<()> {
    // a builder for `FmtSubscriber`.
    let subscriber = FmtSubscriber::builder()
        // all spans/events with a level higher than TRACE (e.g, debug, info, warn, etc.)
        // will be written to stdout.
        .with_max_level(Level::INFO)
        // completes the builder.
        //.with_thread_names(true)
        .finish();

    tracing::subscriber::set_global_default(subscriber)
        .expect("setting default subscriber failed");

    if std::env::args().nth(1).unwrap_or_else(|| "".to_string()) == "-i" {
        // `()` can be used when no completer is required
        let mut rl = Editor::<()>::new();
        if rl.load_history("history.txt").is_err() {
            println!("No previous history.");
        }

        loop {
            let readline = rl.readline(">> ");
            match readline {
                Ok(l) => {
                    rl.add_history_entry(l.as_str());
                    doit(&l);
                },
                Err(ReadlineError::Interrupted) => {
                    info!("CTRL-C");
                    break
                },
                Err(ReadlineError::Eof) => {
                    info!("CTRL-D");
                    break
                },
                Err(err) => {
                    log_err!("Error: {:?}", err);
                    break
                }
            }
        }
        rl.save_history("history.txt").unwrap();
    } else {
        doit("flags == 0x300 || ts < 200");
        info!("{}", "-".repeat(41));
        doit("flags & 0x100 != 0b0 && ts <= 0o10101");
        info!("{}", "-".repeat(41));
        doit("(((((((((((((((1)))))))))))))))");
    }

    Ok(())
}
