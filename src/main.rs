use tracing_subscriber::FmtSubscriber;
use tracing::{Level, info, error as log_err};
use filterer::{nom_parser, pest_parser};
use std::io;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use std::collections::HashMap;

use pest::Parser;
use filterer::nom_parser::{Node, BinaryOp, UnaryOp};

fn parse_num(i: &str) -> isize {
    let r = if i.starts_with("0x") {
        isize::from_str_radix(&i[2..], 16)
    } else if i.starts_with("0o") {
        isize::from_str_radix(&i[2..], 8)
    } else if i.starts_with("0b") {
        isize::from_str_radix(&i[2..], 2)
    } else {
        isize::from_str_radix(i, 10)
    };
    r.unwrap_or(0)
}

trait Eval<T> {
    fn filter(&self, e: &T) -> bool;
}

impl Eval<Message> for Box<Node> {
    fn filter(&self, e: &Message) -> bool {
        fn value(node: &Box<Node>, e: &Message) -> Option<String> {
            match node.as_ref() {
                Node::StringLiteral(s) => Some(s.clone()),
                Node::Constant(s) => Some(s.clone()),
                Node::Identifier(s) => {
                    e.ident(&s).ok().map(|x| {
                        // info!("identifier: x={}", x);
                        let num = parse_num(x);
                        let snum = format!("{}", x);
                        if num != 0 && snum != "0" {
                            snum
                        } else {
                            x.into()
                        }
                    })
                },
                _ => None,
            }
        }

        fn ret(v: bool) -> String {
            if v { "1".into() } else { "0".into() }
        }

        fn eval(node: &Box<Node>, e: &Message) -> String {
            match node.as_ref() {
                Node::Binary { rhs, op, lhs } => {
                    let l = eval(lhs, e);
                    let r = eval(rhs, e);

                    // info!("l={}, r={}", &l, &r);

                    match op {
                        BinaryOp::And => {
                            ret(l != "0" && r != "0")
                        },
                        BinaryOp::Or  => {
                            ret(l != "0" || r != "0")
                        },

                        BinaryOp::Eq  => {
                            ret(l == r)
                        },
                        BinaryOp::Ne  => {
                            ret(l != r)
                        },
                        BinaryOp::Ge  => {
                            ret(l >= r)
                        },
                        BinaryOp::Gt  => {
                            ret(l >  r)
                        },
                        BinaryOp::Le  => {
                            ret(l <= r)
                        },
                        BinaryOp::Lt  => {
                            ret(l <  r)
                        },
                        BinaryOp::Match => {
                            if let Ok(re) = regex::Regex::new(&r) {
                                ret(re.is_match(&l))
                            } else {
                                ret(l == r)
                            }
                        },

                        BinaryOp::Band => {
                            let le = parse_num(&l) as usize;
                            let re = parse_num(&r) as usize;
                            let res = format!("{}", le & re);
                            // info!("BAnd, le={}, re={}, res={}", le, re, res);
                            res
                        },
                        BinaryOp::Bor => unreachable!(),
                    }
                }

                Node::Unary { op, expr } => {
                    match op {
                        UnaryOp::Not => {
                            ret(eval(expr, e) == "0")
                        }
                        UnaryOp::Neg => {
                            format!("{}", -parse_num(&eval(expr, e)))
                        }
                    }
                }

                _ => value(node, e).unwrap_or_else(|| "0".into()),
            }
        }

        eval(&self, e) != "0"
    }
}

trait Accessor {
    fn ident<'a>(&'a self, k: &str) -> Result<&'a str, String>;
}

#[derive(Debug)]
struct Message {
    flags: String,
    ctx: String,
    app: String,
    level: String,
}

impl Accessor for Message {
    fn ident<'a>(&'a self, k: &str) -> Result<&'a str, String> {
        Ok(match k {
            "flags" => &self.flags,
            "ctx"  => &self.ctx,
            "app"  => &self.app,
            "level"  => &self.level,
            _ => Err(format!("Unknown identifier: {}", k))?,
        })
    }
}

fn messages() -> Vec<Message> {
    vec![
        Message { flags: "0x300".into(), ctx: "render".into(), app: "HMI2".into(), level: "0".into() },
        Message { flags: "0x301".into(), ctx: "render".into(), app: "HMI1".into(), level: "0".into() },
        Message { flags: "0x201".into(), ctx: "menu".into(),   app: "HMI".into(),  level: "3".into() },
        Message { flags: "0x300".into(), ctx: "map".into(),    app: "MAP".into(),  level: "1".into() },
        Message { flags: "0x004".into(), ctx: "intersection".into(), app: "SideMAP".into(), level: "1".into() },
    ]
}

fn doit(l: &str) {
    if let Err(e) = nom_parser::parse(l.trim()).and_then(|x| {
        info!("Got: {:#?}", x.as_ref());
        for m in messages().iter() {
            if x.filter(m) {
                info!("{:?}", m);
            }
        }

        Ok(())
    }) {
        log_err!("{}", e);
    }

    if let Err(e) = pest_parser::Filter::parse(pest_parser::Rule::expr, l.trim()).and_then(|x| {
        //info!("Got: {}", x);
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
