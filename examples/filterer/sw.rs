use std::time::{Duration, Instant};

pub struct Stopwatch {
    start: Instant,
}

impl Stopwatch {
    pub fn new() -> Self {
        Self {
            start: Instant::now(),
        }
    }

    pub fn elapsed(self) -> Duration {
        Instant::now() - self.start
    }
}
