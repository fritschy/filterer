use tracing_subscriber::FmtSubscriber;
use tracing::{Level, info, error as log_err};
use filterer::{nom_parser, pest_parser};
use std::io;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::collections::HashMap;

use pest::Parser;
use filterer::nom_parser::eval::{Eval, Accessor};

#[derive(Debug)]
struct Message {
    ts: String,
    flags: String,
    ctx: String,
    app: String,
    level: String,
}

impl Accessor for Message {
    fn ident<'a>(&'a self, k: &str) -> Result<&'a str, String> {
        Ok(match k {
            "flags"  => &self.flags,
            "ctx"    => &self.ctx,
            "app"    => &self.app,
            "level"  => &self.level,
            "ts"     => &self.ts,
            _ => return Err(format!("Unknown identifier: {}", k)),
        })
    }
}

fn messages() -> Vec<Message> {
    vec![
        Message { ts: "0".into(), flags: "0x300".into(), ctx: "render".into(), app: "HMI2".into(), level: "0".into() },
        Message { ts: "100".into(), flags: "0x301".into(), ctx: "render".into(), app: "HMI1".into(), level: "0".into() },
        Message { ts: "101".into(), flags: "0x201".into(), ctx: "menu".into(),   app: "HMI".into(),  level: "3".into() },
        Message { ts: "200".into(), flags: "0x300".into(), ctx: "map".into(),    app: "MAP".into(),  level: "1".into() },
        Message { ts: "300".into(), flags: "0x004".into(), ctx: "intersection".into(), app: "SideMAP".into(), level: "1".into() },
    ]
}

fn doit(l: &str) {
    if let Err(e) = nom_parser::parse(l.trim()).map(|x| {
        info!("Got: {:#?}", x.as_ref());
        let mut count = 0;
        let mut re_cache = HashMap::new();
        for m in messages().iter() {
            if x.eval_filter(m, &mut re_cache) {
                count += 1;
                info!("{:?}", m);
            }
        }

        info!("matched {}/{} messages", count, messages().len());
    }) {
        log_err!("{}", e);
    }

    if let Err(e) = pest_parser::Filter::parse(pest_parser::Rule::expr, l.trim()).map(|_x| {
        //info!("Got: {}", x);
    }) {
        log_err!("Got error: {:?}", e);
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
        doit("flags == 0x300");
        //doit("flags & 0x100 != 0b0 && ts <= 0o10101");
    }

    Ok(())
}
