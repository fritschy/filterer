use tracing_subscriber::FmtSubscriber;
use tracing::{Level, info, error as log_err};
use filterer::{ast};
use std::io;
use std::io::BufRead;

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
        let stdin = std::io::stdin();
        let stdin = stdin.lock();
        for l in stdin.lines() {
            if l.is_err() {
                break;
            }

            let l = l.unwrap();

            if let Err(e) = ast::parse(l.trim()).and_then(|x| {
                info!("Got: {}: {:#?}", x.0, x.1.as_ref());
                Ok(())
            }) {
                log_err!("Got error: {:?}", e);
            }
        }
    } else {
        ast::parse("(flags & 0x100) != 0 && ts <= 10101").and_then(|x| {
            info!("Got: {}: {:#?}", x.0, x.1.as_ref());
            Ok(())
        }).expect("parsed");
    }

    Ok(())
}