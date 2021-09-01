use tracing_subscriber::FmtSubscriber;
use tracing::{Level, info, error as log_err};
use filterer::parse_filter;
use std::io;

fn main() -> io::Result<()> {
    // a builder for `FmtSubscriber`.
    let subscriber = FmtSubscriber::builder()
        // all spans/events with a level higher than TRACE (e.g, debug, info, warn, etc.)
        // will be written to stdout.
        .with_max_level(Level::TRACE)
        // completes the builder.
        .with_thread_names(true)
        .finish();

    tracing::subscriber::set_global_default(subscriber)
        .expect("setting default subscriber failed");

    #[derive(Debug)]
    struct Message {
        ctx: String,
        app: String,
    }

    parse_filter::<Message>("((!(ctx == app)) && (ts > 1000 && ts <= 9999)) || flags & 0x100 != 0").and_then(|x| {
        info!("Got: {}: {:#?}", x.0, x.1.as_ref());
        Ok(())
    }).expect("parsed");

    parse_filter::<Message>("flags & 0x100 != 0").and_then(|x| {
        info!("Got: {}: {:#?}", x.0, x.1.as_ref());
        Ok(())
    }).expect("parsed");

    Ok(())
}