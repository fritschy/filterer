use std::sync::Arc;

use regex::{Regex, RegexBuilder};

use crate::machine::{AccessorQuery, CompileError, KeyAccessor};
use crate::parser::parse;
use crate::value::{parse_num, Value};
use crate::{compile, ExprEval};

type Data = &'static str;

const DATA: &[Data] = &[
    "abc",
    "ABC",
    "CDE",
    "CDe",
    "cde",
    "ahi",
    "10a",
    "0x100",
    "0x200",
    "0o12",
    "0b101",
    "\"\\\"moo\\\"\n\t\\v\r\\f",
    "/this/is/a/path",
];

impl KeyAccessor for Data {
    fn get_str(&self, _: usize, _i: usize) -> Option<Arc<String>> {
        Some(Arc::new(String::from(*self)))
    }

    fn get_num(&self, _: usize, _: usize) -> Option<isize> {
        None
    }

    fn get_len(&self, _: usize) -> Option<isize> {
        None
    }
}

struct DataQuery;

impl AccessorQuery for DataQuery {
    fn get_ident(&self, name: &str) -> Option<usize> {
        if name == "d" {
            return Some(0);
        }
        None
    }
}

// Compare expr filter with iter filter
fn compare(expr: &str, filt: impl Fn(&&&str) -> bool) {
    if let Err(p) = compile(expr, &DataQuery).map(|machine| {
        let expect = DATA.iter().filter(filt).copied().collect::<Vec<_>>();

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

// Compare expr filter with iter filter
fn compile_result(expr: &str) -> Result<ExprEval, CompileError> {
    compile(expr, &DataQuery)
}

fn re(s: &str) -> Regex {
    cre(s, false)
}

fn cre(s: &str, icase: bool) -> Regex {
    RegexBuilder::new(s)
        .case_insensitive(icase)
        .build()
        .expect("regex")
}

fn check(expr: &str, exp: bool) {
    struct X;
    impl KeyAccessor for X {
        fn get_str(&self, _: usize, _: usize) -> Option<Arc<String>> {
            Some(Arc::new(String::from("1")))
        }
        fn get_num(&self, _: usize, _: usize) -> Option<isize> {
            Some(1)
        }
        fn get_len(&self, _: usize) -> Option<isize> {
            None
        }
    }
    impl AccessorQuery for X {
        fn get_ident(&self, name: &str) -> Option<usize> {
            if name == "d" {
                return Some(0);
            }
            None
        }
    }
    const DATA0: X = X;
    let machine = compile(expr, &X).unwrap();
    assert_eq!(machine.eval(&DATA0), exp);
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
    check("\"\\\"\\r\\n\\t\\0\"", true);
    check("/\\/\\r\\n\\t\\0/", false);
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
    check(&("0||".repeat(64) + "0"), false);
    check(&("0||".repeat(750) + "0"), false);

    assert_eq!(
        compile_result("does_not_exist").unwrap_err(),
        CompileError::UnknownIdentifier(String::from("does_not_exist"))
    );
    assert_eq!(
        compile_result("doesNotExist == doesNotExist").unwrap_err(),
        CompileError::UnknownIdentifier(String::from("doesNotExist"))
    );
}

#[test]
fn one_array_only() {
    fn check(expr: &str, f: impl Fn(&&Data) -> bool) {
        struct X;
        impl KeyAccessor for X {
            fn get_str(&self, n: usize, i: usize) -> Option<Arc<String>> {
                if n == 0 && i < DATA.len() {
                    return Some(Arc::new(DATA[i].to_string()));
                }
                None
            }
            fn get_num(&self, _: usize, _: usize) -> Option<isize> {
                None
            }
            fn get_len(&self, n: usize) -> Option<isize> {
                if n == 0 {
                    return Some(DATA.len() as isize);
                }
                None
            }
        }
        impl AccessorQuery for X {
            fn get_ident(&self, name: &str) -> Option<usize> {
                if name == "d" {
                    return Some(0);
                }
                None
            }
        }

        let machine = compile(expr, &X).unwrap();
        println!("Code:\n{}", &machine);

        let x = DATA.iter().filter(|&x| machine.eval(x)).collect::<Vec<_>>();
        let expect = DATA.iter().filter(f).collect::<Vec<_>>();

        assert_eq!(expect, x);
    }

    check("d[0] == 0x100", |&&x| x == "0x100");
}

#[test]
fn regexes() {
    compare("d =~ /a/", |x| re("a").is_match(x));
    compare("!(d =~ /a/)", |x| !re("a").is_match(x));

    compare("!(d =~ /a/i)", |x| !cre("a", true).is_match(x));
    compare("!(d =~ /e/i)", |x| !cre("e", true).is_match(x));

    compare(" d =~ /a/ ", |x| re("a").is_match(x));

    // negated match
    compare(" d =~ !/a/ ", |x| !re("a").is_match(x));
    compare(" ! ( d =~ /a/ ) ", |x| !re("a").is_match(x));
    compare(" ! ( d =~ !/a/ ) ", |x| re("a").is_match(x));

    // We don't currently handle "!!..."
    compare(" ! (!( d =~ !/a/ )) ", |x| !re("a").is_match(x));

    // match with a /, obviously needed for paths and URLs ...
    compare(" ! ( d =~ /\\// ) ", |x| !re("/").is_match(x));
    compare(" d =~ !/\\// ", |x| !re("/").is_match(x));
    compare(" d =~ /\\// ", |x| re("/").is_match(x));

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
    println!("{:?}", parse("1+2").unwrap_err());
    println!("{:?}", parse("d =! \"eins\"").unwrap_err());

    println!("{}", parse("1+2").unwrap_err().describe());
    println!("{}", parse("d =! \"eins\"").unwrap_err().describe());
    println!("{}", parse("d !& \"eins\"").unwrap_err().describe());
    println!("{}", parse("(").unwrap_err().describe());
    println!("{}", parse(")").unwrap_err().describe());
    println!("{}", parse("\"").unwrap_err().describe());
    println!("{}", parse("flags && flags &").unwrap_err().describe());
    println!("{}", parse("\0").unwrap_err().describe());
    println!(
        "{}",
        parse("(".repeat(500).as_str()).unwrap_err().describe()
    );
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
    compare("\"0x200\" == d", |&&x| x == "0x200");
    compare("0x100 == d", |&&x| x == "0x100");

    // Need to invoke PartialOrd for Value
    compare("d >= \"0x100\"", |&&x| {
        Value::Str(Arc::new(String::from(x))) >= Value::Str(Arc::new(String::from("0x100")))
    });
    compare("\"0x100\" <= d", |&&x| {
        Value::Str(Arc::new(String::from(x))) >= Value::Str(Arc::new(String::from("0x100")))
    });

    // Nil cannot be matched against an Re
    assert_eq!(
        compile_result("doesNotExist =~ /./").unwrap_err(),
        CompileError::UnknownIdentifier(String::from("doesNotExist"))
    );
}

#[derive(Debug, PartialEq, Copy, Clone)]
struct Item(isize, &'static str, isize);

impl KeyAccessor for Item {
    fn get_str(&self, k: usize, _i: usize) -> Option<Arc<String>> {
        match k {
            1 => Some(Arc::new(String::from(self.1))),
            _ => None,
        }
    }

    fn get_num(&self, k: usize, _i: usize) -> Option<isize> {
        match k {
            0 => Some(self.0),
            2 => Some(self.2),
            _ => None,
        }
    }

    fn get_len(&self, _: usize) -> Option<isize> {
        None
    }
}

const DATA2: &[Item] = &[
    Item(0, "null", 0x100),
    Item(0, "null2", 0x000),
    Item(1, "eins", 0x002),
    Item(2, "zwo", 0x002),
    Item(3, "drei", 0x002),
    Item(4, "vier", 0x003),
    Item(5, "fünnef", 0x104),
    Item(6, "sechs", 0x101),
    Item(7, "sieben", 0x104),
    Item(8, "acht", 0x102),
    Item(9, "neun\n\tzehn", 0x700),
];

struct Data2;

impl AccessorQuery for Data2 {
    fn get_ident(&self, name: &str) -> Option<usize> {
        match name {
            "s" => Some(1),
            "u" => Some(2),
            "i" => Some(0),
            _ => None,
        }
    }
}

fn compare2(expr: &str, f: impl Fn(&&Item) -> bool) {
    if let Err(p) = compile(expr, &Data2).map(|machine| {
        let expect = DATA2.iter().filter(f).copied().collect::<Vec<_>>();

        println!("Code:\n{}", &machine);
        let d = DATA2
            .iter()
            .filter(|&x| machine.eval(x)).copied()
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
    compare2("s =~ /[eS]/i", |&&Item(_, s, _)| {
        cre("[eS]", true).is_match(s)
    });
    compare2("s =~ !/[Es]/i", |&&Item(_, s, _)| {
        !cre("[Es]", true).is_match(s)
    });
    // watch out for the infamous ü-separated values!!1
    compare2("s =~ /ü/", |&&Item(_, s, _)| re("ü").is_match(s));
    compare2("u & 0x100 && i < 7", |&&Item(i, _, u)| {
        u & 0x100 != 0 && i < 7
    });
    compare2("u & 0x100 && i < +7", |&&Item(i, _, u)| {
        u & 0x100 != 0 && i < 7
    });
    compare2("u & ~0x100 && i > -7", |&&Item(i, _, u)| {
        u & !0x100 != 0 && i > -7
    });
    compare2("u | 1 && i > -7", |&&Item(i, _, u)| u | 1 != 0 && i > -7);
    compare2("s =~ /\\n/", |&&Item(_, s, _)| re("\n").is_match(s));
    compare2("s =~ /\\t/", |&&Item(_, s, _)| re("\t").is_match(s));
}

#[test]
fn arrays() {
    #[derive(PartialEq, Debug)]
    struct D {
        i: isize,
        a: Vec<isize>,
        s: Vec<String>,
    }

    impl KeyAccessor for D {
        fn get_str(&self, k: usize, i: usize) -> Option<Arc<String>> {
            if k == 2 && i < self.s.len() {
                Some(Arc::new(self.s[i].clone()))
            } else {
                None
            }
        }

        fn get_num(&self, k: usize, i: usize) -> Option<isize> {
            match (k, i) {
                (0, _) => Some(self.i),
                (1, i) => {
                    if i < self.a.len() {
                        Some(self.a[i])
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }

        fn get_len(&self, k: usize) -> Option<isize> {
            match k {
                1 => Some(self.a.len() as isize),
                2 => Some(self.s.len() as isize),
                _ => None,
            }
        }
    }

    struct DataD;

    impl AccessorQuery for DataD {
        fn get_ident(&self, name: &str) -> Option<usize> {
            match name {
                "i" => Some(0),
                "a" => Some(1),
                "s" => Some(2),
                _ => None,
            }
        }
    }

    fn compare(expr: &str, f: impl Fn(&&D) -> bool) {
        let data = vec![
            D {
                i: 12,
                a: vec![9, 8, 7],
                s: vec![],
            },
            D {
                i: 22,
                a: vec![0, 1, 2, 3, 4],
                s: vec!["moo a".to_string(), "test c".to_string()],
            },
            D {
                i: 2,
                a: Vec::new(),
                s: vec!["moo".to_string(), " test ".to_string()],
            },
            D {
                i: 3,
                a: vec![4711, 42],
                s: vec![" moo test a ".to_string()],
            },
            D {
                i: 5,
                a: vec![42],
                s: vec![
                    "a".to_string(),
                    "b".to_string(),
                    "a b".to_string(),
                    "c".to_string(),
                    "a c".to_string(),
                ],
            },
        ];

        if let Err(p) = compile(expr, &DataD).map(|machine| {
            let expect = data.iter().filter(f).collect::<Vec<_>>();

            println!("Code:\n{}", &machine);
            let d = data.iter().filter(|x| machine.eval(*x)).collect::<Vec<_>>();
            assert_eq!(d, expect);
        }) {
            panic!("{}", p);
        }
    }

    compare("a[0] > 0", |x| !x.a.is_empty() && x.a[0] > 0);
    compare("a[0] > +0", |x| !x.a.is_empty() && x.a[0] > 0);
    compare("a[0] > -0", |x| !x.a.is_empty() && x.a[0] > 0);
    compare("a[0x0] > 0", |x| !x.a.is_empty() && x.a[0] > 0);
    compare("a[0b0] > 0", |x| !x.a.is_empty() && x.a[0] > 0);
    compare("a[0o3]", |x| x.a.len() > 3 && x.a[3] > 0);
    compare("a", |x| !x.a.is_empty() && x.a[0] != 0);
    compare("i < a[0]", |x| !x.a.is_empty() && x.a[0] > x.i);
    compare("a[0x10000]", |_| false);
    compare("a[0x10000] == a[0x10000]", |_| false);
    compare("a.len > 1", |x| x.a.len() > 1);
    compare("a.len > -1", |x| x.a.len() as isize > -1);
    compare("a.len > +1", |x| x.a.len() > 1);
    compare("a.len == 0", |x| x.a.is_empty());
    compare("i.len", |_| false);
    compare("s[0] =~ /a/", |x| !x.s.is_empty() && x.s[0].contains('a'));
    compare("s[0] =~ /NOT FOUND/", |_| false);
    compare("s[0] =~ /moo/", |x| {
        !x.s.is_empty() && x.s[0].contains("moo")
    });
    compare("s[0] =~ !/moo/", |x| {
        x.s.is_empty() || (!x.s.is_empty() && !x.s[0].contains("moo"))
    });

    println!(
        "{}",
        parse("a[\"non-numeric-index\"] > 0")
            .unwrap_err()
            .describe()
    );
    println!("{}", parse("a[] > 0").unwrap_err().describe());
    println!("{}", parse("a[identifier] > 0").unwrap_err().describe());
    println!("{}", parse("a[a] > 0").unwrap_err().describe());
    println!("{}", parse("a[-10] > 0").unwrap_err().describe());
    println!("{}", parse("a[10].len").unwrap_err().describe());
    println!("{}", parse("a.length").unwrap_err().describe());
    println!("{}", parse("a.len()").unwrap_err().describe());
    println!(
        "{}",
        parse("a[7777777777777777777777777]")
            .unwrap_err()
            .describe()
    );
}
