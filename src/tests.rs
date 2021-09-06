use crate::nom_parser::parse;
use crate::eval::*;

type Data<'a> = &'a str;

const DATA: &[Data<'static>] = &["abc", "cde", "ahi", "10a", "0x100", "0x200", "0o12", "0b101", "\"\\\"moo\\\"\n\t\\v\r\\f"];

impl Accessor for Data<'_> {
    fn get_str<'a>(&'a self, k: &str) -> Result<&str, String> {
        if k == "d" {
            Ok(self)
        } else {
            Err("Unknown identifier".to_string())
        }
    }

    fn get_num(&self, k: &str) -> Result<isize, String> {
        Err("No such number".to_string())
    }
}

// Compare expr filter with iter filter
fn compare(expr: &str, filt: impl Fn(&&&str) -> bool) {
    if let Err(p) = parse(expr).map(|p| {
        assert_eq!(DATA.iter().filter(|m| p.eval_filter(*m)).map(|m| *m).collect::<Vec<&str>>(),
                   DATA.iter().filter(filt).map(|m| *m).collect::<Vec<_>>());
    }) {
        panic!("{}", p);
    }
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
}

#[test]
fn regexes() {
    compare("d =~ /a/", |x| regex::Regex::new("a").unwrap().is_match(x));
    compare("!(d =~ /a/)", |x| !regex::Regex::new("a").unwrap().is_match(x));
    // compare("\"a\" =~ d", |x| regex::Regex::new("a").unwrap().is_match(x));
}

#[test]
fn relops() {
    compare("d >= 0x100 && d <= 0x200 && d", |&&x| parse_num(x) >= 0x100 && parse_num(x) <= 0x200 && x != "0");
    compare("d > 0xff && d < 0x201 && d", |&&x| parse_num(x) >= 0xff && parse_num(x) <= 0x201 && x != "0");
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
    println!("{}", parse("flags & 0x700 <= 0x300 || (ts >= 1000 && ts < 100000f)").unwrap_err().describe());
    println!("{}", parse("flags & 0x700 <= 0x300 || ts >= 1000 && ts < 100000)").unwrap_err().describe());
}

#[test]
fn semantic_errors() {
    // rhs needs to be regex
    println!("{}", parse("1 && app =~ \"moo\"").unwrap_err().describe());
    println!("{}", parse("app =~ (flags)").unwrap_err().describe());
    println!("{}", parse("app =~ flags").unwrap_err().describe());
    println!("{}", parse("\"moo\" =~ \"moo\"").unwrap_err().describe());

    // lhs needs to be ident or string
    println!("{}", parse("(app =~ /map/) =~ /moo/").unwrap_err().describe());
    println!("{}", parse("0x100 =~ /moo/").unwrap_err().describe());
}

#[test]
fn mixing_types() {
    compare("d == 0x1010101", |_| false);
    compare("\"0x100\" == 0x100", |_| false);
}