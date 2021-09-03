use tracing_subscriber::FmtSubscriber;
use tracing::{Level, info, error as log_err};
use filterer::{nom_parser, pest_parser};
use std::io;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use pest::Parser;

fn doit(l: &str) {
    if let Err(e) = nom_parser::parse(l.trim()).and_then(|x| {
        info!("Got: {:#?}", x.as_ref());
        Ok(())
    }) {
        log_err!("{}", e);
    }

    if let Err(e) = pest_parser::Filter::parse(pest_parser::Rule::expr, l.trim()).and_then(|x| {
        info!("Got: {}", x);
        Ok(())
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

    if std::env::args().nth(1).unwrap_or("".to_string()) == "-i" {
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
        doit("flags & 0x100 != 0b0 && ts <= 0o10101");
    }

    Ok(())
}
