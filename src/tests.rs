use crate::eval::*;
use crate::machine::Machine;
use crate::parser::parse;
use regex::Regex;
use std::rc::Rc;

type Data = &'static str;

const DATA: &[Data] = &[
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

impl Accessor for Data {
    fn get_str(&self, k: &str, _i: usize) -> Option<Rc<String>> {
        if k == "d" {
            Some(Rc::new(String::from(*self)))
        } else {
            None
        }
    }

    fn get_num(&self, _: &str, _: usize) -> Option<isize> {
        None
    }

    fn get_len(&self, _: &str) -> Option<isize> { None }
}

// Compare expr filter with iter filter
fn compare(expr: &str, filt: impl Fn(&&&str) -> bool) {
    if let Err(p) = parse(expr).map(|p| {
        let expect = DATA.iter().filter(filt).map(|m| *m).collect::<Vec<_>>();

        let machine = Machine::from_node(p.as_ref());
        println!("Code:\n{}", &machine);
        let d = DATA
            .iter()
            .cloned()
            .filter(|x| machine.eval(x))
            .collect::<Vec<_>>();
        assert_eq!(d, expect);
    }) {
        panic!("{}", p);
    }
}

fn re(s: &str) -> Regex {
    Regex::new(s).expect("regex")
}

fn check(expr: &str, exp: bool) {
    struct X;
    impl Accessor for X {
        fn get_str(&self, _: &str, _: usize) -> Option<Rc<String>> {
            Some(Rc::new(String::from("1")))
        }
        fn get_num(&self, _: &str, _: usize) -> Option<isize> {
            Some(1)
        }
        fn get_len(&self, _: &str) -> Option<isize> { None }
    }
    const DATA0: X = X;
    let node = parse(expr).unwrap();
    let machine = Machine::from_node(node.as_ref());
    println!("Code:\n{}", &machine);
    assert!(machine.eval(&DATA0) == exp);
}

#[test]
fn always_true() {
    check("1", true);
    check("42", true);
    check("!0", true);
    check("-1 < 0", true);
    check("0 > -1", true);
    check("0 == -0", true);
    check("0xfff & 0x070 == 0x070", true);
    check("0xf3f & 0x070 == 0x030", true);
    check("1 || 0", true);
    check("0 || 1", true);
    check("d == d", true);
    check("!\"0\"", true);
    check("\"1\"", true);
}

#[test]
fn always_false() {
    check("0", false);
    check("!1", false);
    check("-1 > 0", false);
    check("-1 >= 0", false);
    check("0 < -1", false);
    check("0 <= -1", false);
    check("!16180", false);
    check("/1/", false);
    check("1 && 0", false);
    check("0 && 1", false);
    check("0.1", false);

    compare("does_not_exist", |_| false);
}

#[test]
fn regexes() {
    compare("d =~ /a/", |x| re("a").is_match(x));
    compare("!(d =~ /a/)", |x| !re("a").is_match(x));

    // Invalid regex will be replaced by a not-matching regex
    compare("d =~ /(/", |_| false);
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
struct Item(isize, &'static str, isize);

impl Accessor for Item {
    fn get_str(&self, k: &str, _i: usize) -> Option<Rc<String>> {
        match k {
            "s" => Some(Rc::new(String::from(self.1))),
            _ => None,
        }
    }

    fn get_num(&self, k: &str, _i: usize) -> Option<isize> {
        match k {
            "i" => Some(self.0),
            "u" => Some(self.2),
            _ => None,
        }
    }

    fn get_len(&self, _: &str) -> Option<isize> { None }
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
        let expect = DATA2.iter().filter(f).map(|m| *m).collect::<Vec<_>>();

        let machine = Machine::from_node(p.as_ref());
        println!("Code:\n{}", &machine);
        let d = DATA2
            .iter()
            .filter(|&x| machine.eval(x))
            .map(|m| *m)
            .collect::<Vec<_>>();
        assert_eq!(d, expect);
    }) {
        panic!("{}", p);
    }
}

#[test]
fn comprehensive_data() {
    compare2("s =~ /[es]/", |&&Item(_, s, _)| re("[es]").is_match(s));
    compare2("s =~ !/[es]/", |&&Item(_, s, _)| !re("[es]").is_match(s));
    compare2("s =~ /ü/", |&&Item(_, s, _)| re("ü").is_match(s));
    compare2("u & 0x100 && i < 7", |&&Item(i, _, u)| {
        u & 0x100 != 0 && i < 7
    });
}

#[test]
fn arrays() {
    #[derive(PartialEq, Debug)]
    struct D {
        i: isize,
        a: Vec<isize>,
    }

    impl Accessor for D {
        fn get_str(&self, _: &str, _: usize) -> Option<Rc<String>> {
            None
        }

        fn get_len(&self, k: &str) -> Option<isize> {
            match k {
                "a" => Some(self.a.len() as isize),
                _ => None,
            }
        }

        fn get_num(&self, k: &str, i: usize) -> Option<isize> {
            match (k, i) {
                ("i", _) => Some(self.i),
                ("a", i) => {
                    if i < self.a.len() {
                        Some(self.a[i])
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
    }

    fn compare(expr: &str, f: impl Fn(&&D) -> bool) {
        let data = vec![
            D{i: 12, a: vec![9, 8, 7]},
            D{i: 22, a: vec![0, 1, 2, 3, 4]},
            D{i: 2, a: Vec::new()},
            D{i: 3, a: vec![4711, 42]},
            D{i: 5, a: vec![42]},
        ];

        if let Err(p) = parse(expr).map(|p| {
            let expect = data.iter().filter(f).map(|m| m).collect::<Vec<_>>();

            let machine = Machine::from_node(p.as_ref());
            println!("Code:\n{}", &machine);
            let d = data
                .iter()
                .filter(|x| machine.eval(*x))
                .collect::<Vec<_>>();
            assert_eq!(d, expect);
        }) {
            panic!("{}", p);
        }
    }

    compare("a[0] > 0", |x| x.a.len() > 0 && x.a[0] > 0);
    compare("a[0x0] > 0", |x| x.a.len() > 0 && x.a[0] > 0);
    compare("a[0b0] > 0", |x| x.a.len() > 0 && x.a[0] > 0);
    compare("a[0o3]", |x| x.a.len() > 3 && x.a[3] > 0);
    compare("a", |x| x.a.len() > 0 && x.a[0] != 0);
    compare("i < a[0]", |x| x.a.len() > 0 && x.a[0] > x.i);
    compare("a[0x10000]", |_| false);
    compare("a.len > 1", |x| x.a.len() > 1);
    compare("a.len == 0", |x| x.a.is_empty());

    println!("{}", parse("a[\"non-numeric-index\"] > 0").unwrap_err().describe());
    println!("{}", parse("a[] > 0").unwrap_err().describe());
    println!("{}", parse("a[identifier] > 0").unwrap_err().describe());
    println!("{}", parse("a[a] > 0").unwrap_err().describe());
    println!("{}", parse("a[-10] > 0").unwrap_err().describe());
    println!("{}", parse("a[10].len").unwrap_err().describe());
    println!("{}", parse("a.length").unwrap_err().describe());
    println!("{}", parse("a.len()").unwrap_err().describe());
}
