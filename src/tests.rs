use crate::eval::*;
use crate::nom_parser::parse;
use regex::Regex;

type Data<'a> = &'a str;

const DATA: &[Data<'static>] = &[
    "abc",
    "cde",
    "ahi",
    "10a",
    "0x100",
    "0x200",
    "0o12",
    "0b101",
    "\"\\\"moo\\\"\n\t\\v\r\\f",
];

impl Accessor for Data<'_> {
    fn get_str<'a>(&'a self, k: &str) -> Result<&str, String> {
        if k == "d" {
            Ok(self)
        } else {
            Err("Unknown identifier".to_string())
        }
    }

    fn get_num(&self, _k: &str) -> Result<isize, String> {
        Err("No such number".to_string())
    }
}

// Compare expr filter with iter filter
fn compare(expr: &str, filt: impl Fn(&&&str) -> bool) {
    if let Err(p) = parse(expr).map(|p| {
        assert_eq!(
            DATA.iter()
                .filter(|m| p.eval_filter(*m))
                .map(|m| *m)
                .collect::<Vec<_>>(),
            DATA.iter().filter(filt).map(|m| *m).collect::<Vec<_>>()
        );
    }) {
        panic!("{}", p);
    }
}

fn re(s: &str) -> Regex {
    Regex::new(s).expect("regex")
}

#[test]
fn always_true() {
    compare("1", |_| true);
    compare("42", |_| true);
    compare("!0", |_| true);
    compare("-1 < 0", |_| true);
    compare("0 > -1", |_| true);
    compare("0 == -0", |_| true);
    compare("0xfff & 0x070 == 0x070", |_| true);
    compare("0xf3f & 0x070 == 0x030", |_| true);
    compare("1 || 0", |_| true);
    compare("0 || 1", |_| true);
    compare("d == d", |_| true);
    compare("!\"0\"", |_| true);
    compare("\"1\"", |_| true);
}

#[test]
fn always_false() {
    compare("0", |_| false);
    compare("!1", |_| false);
    compare("-1 > 0", |_| false);
    compare("-1 >= 0", |_| false);
    compare("0 < -1", |_| false);
    compare("0 <= -1", |_| false);
    compare("!16180", |_| false);
    compare("0b0 || -0o0 || does_not_exist", |_| false);
    compare("/1/", |_| false);
    compare("1 && 0", |_| false);
    compare("0 && 1", |_| false);
}

#[test]
fn regexes() {
    compare("d =~ /a/", |x| re("a").is_match(x));
    compare("!(d =~ /a/)", |x| !re("a").is_match(x));
}

#[test]
fn relops() {
    compare("d >= 0x100 && d <= 0x200 && d <= d", |&&x| {
        parse_num(x) >= 0x100 && parse_num(x) <= 0x200 && x != "0"
    });
    compare("d > 0xff && d < 0x201 && d >= d", |&&x| {
        parse_num(x) >= 0xff && parse_num(x) <= 0x201 && x != "0"
    });
}

#[test]
fn comparisons() {
    compare("d == \"ahi\"", |&&x| x == "ahi");
    compare("!(d != \"ahi\")", |&&x| x == "ahi");
    compare("d != \"ahi\"", |&&x| x != "ahi");
    compare("!(d == \"ahi\")", |&&x| x != "ahi");
}

#[test]
fn parse_errors() {
    println!("{}", parse("1+2").unwrap_err().describe());
    println!("{}", parse("d =! \"eins\"").unwrap_err().describe());
    println!("{}", parse("d !& \"eins\"").unwrap_err().describe());
    println!("{}", parse("(").unwrap_err().describe());
    println!("{}", parse(")").unwrap_err().describe());
    println!("{}", parse("\"").unwrap_err().describe());
    println!("{}", parse("flags && flags &").unwrap_err().describe());
    println!(
        "{}",
        parse("flags & 0x700 <= 0x300 || (ts >= 1000 && ts < 100000f)")
            .unwrap_err()
            .describe()
    );
    println!(
        "{}",
        parse("flags & 0x700 <= 0x300 || ts >= 1000 && ts < 100000)")
            .unwrap_err()
            .describe()
    );
}

#[test]
fn semantic_errors() {
    // rhs needs to be regex
    println!("{}", parse("1 && app =~ \"moo\"").unwrap_err().describe());
    println!("{}", parse("app =~ (flags)").unwrap_err().describe());
    println!("{}", parse("app =~ flags").unwrap_err().describe());
    println!("{}", parse("\"moo\" =~ \"moo\"").unwrap_err().describe());

    // lhs needs to be ident or string
    println!(
        "{}",
        parse("(app =~ /map/) =~ /moo/").unwrap_err().describe()
    );
    println!("{}", parse("0x100 =~ /moo/").unwrap_err().describe());

    // regexes where they don't make sense
    println!("{}", parse("app == /moo/").unwrap_err().describe());
    println!("{}", parse("/moo/ && 1").unwrap_err().describe());
    println!("{}", parse("1 || /moo/").unwrap_err().describe());
    println!("{}", parse("! /moo/").unwrap_err().describe());
    println!("{}", parse("/moo/ == /^moo$/").unwrap_err().describe());
}

#[test]
fn mixing_types() {
    compare("d == 0x1010101", |_| false);
    compare("\"0x100\" == 0x100", |_| true);
    compare("\"0x100\" == d", |&&x| x == "0x100");
    compare("d == \"0x200\"", |&&x| x == "0x200");
    compare("d >= \"0x100\"", |&&x| x == "0x200" || x == "0x100");
    compare("\"0x200\" == d", |&&x| x == "0x200");
    compare("\"0x100\" <= d", |&&x| x == "0x200" || x == "0x100");
    compare("0x100 == d", |&&x| x == "0x100");
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Item<'a>(isize, &'a str, isize);

impl<'a> Accessor for Item<'a> {
    fn get_str(&self, k: &str) -> Result<&'a str, String> {
        match k {
            "s" => Ok(self.1),
            _ => Err(format!("No such key: {}", k)),
        }
    }

    fn get_num(&self, k: &str) -> Result<isize, String> {
        match k {
            "i" => Ok(self.0),
            "u" => Ok(self.2),
            _ => Err(format!("No such key: {}", k)),
        }
    }
}

const DATA2: &[Item] = &[
    Item(0, "null", 0x100),
    Item(1, "eins", 0x002),
    Item(2, "zwo", 0x002),
    Item(3, "drei", 0x002),
    Item(4, "vier", 0x003),
    Item(5, "fünnef", 0x104),
    Item(6, "sechs", 0x101),
    Item(7, "sieben", 0x104),
    Item(8, "acht", 0x102),
];

fn compare2(expr: &str, f: impl Fn(&&Item) -> bool) {
    if let Err(p) = parse(expr).map(|p| {
        assert_eq!(
            DATA2.iter()
                .filter(|m| p.eval_filter(*m))
                .map(|m| *m)
                .collect::<Vec<_>>(),
            DATA2.iter().filter(f).map(|m| *m).collect::<Vec<_>>()
        );
    }) {
        panic!("{}", p);
    }
}

#[test]
fn comprehensive_data() {
    compare2("s =~ /[es]/", |&&Item(_, s, _)| re("[es]").is_match(s));
    compare2("s =~ /ü/", |&&Item(_, s, _)| re("ü").is_match(s));
    compare2("u & 0x100 && i < 7", |&&Item(i, _, u)| u & 0x100 != 0 && i < 7);
}
