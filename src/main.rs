use tracing_subscriber::FmtSubscriber;
use tracing::{Level, info, error as log_err};
use filterer::{ast};
use std::io;
use std::io::BufRead;
use rustyline::error::ReadlineError;
use rustyline::Editor;

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

    #[derive(Debug)]
    struct Message {
        ctx: String,
        app: String,
    }

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

                    if let Err(e) = ast::parse(l.trim()).and_then(|x| {
                        info!("Got: {}: {:#?}", x.0, x.1.as_ref());
                        Ok(())
                    }) {
                        log_err!("Got error: {:?}", e);
                    }
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
        ast::parse("(flags & 0x100) != 0 && ts <= 10101").and_then(|x| {
            info!("Got: {}: {:#?}", x.0, x.1.as_ref());
            Ok(())
        }).expect("parsed");
    }

    Ok(())
}