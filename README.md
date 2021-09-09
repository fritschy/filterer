# Goal of this project
Implement simple filtering expressions.  Simple means, there is not
much in the way of ambiguity or surprise when writing expressions. The
*filter* in this case means, it is conceived as a way to filter some
arbitrary data.


The glue between expression evaluation and the data is the `trait Accessor`.


This is very much an experiment, there are things that make no sense, are
redundant and/or just plain inefficient...

* Expressions follow a c-like syntax
* There are nearly none mathematical operations
* An arbitrary number of expressions can be chained together using `&&` and 
  `||` operators.
  expressions together
* Strings and integers can be used
* Identifiers that will resolve to *some* data that is to be
  filtered can be used
* Matches against regular expressions
* I am not claiming correctness, however there are some tests
  that give me *some* confidence that it is doing what I think
  it should be doing
* Invalid regexes will quietly be replaced by something that never
  matches
* Empty strings, or strings which can be converted to a 0
  will be evaluated as false

## Examples

* `ctx =~ /e/ && ((app =~ /HMI/ && ts > 0) || flags & 0x100)`
* `1`
* `0`
* `"0" == 0`
* `!(1 == 0)`

For more examples check the tests and main

## Dependencies
* This project uses the awesome [nom](https://github.com/Geal/nom) to
  implement its parser.
* Furthermore I rely on [rustyline](https://github.com/kkawakam/rustyline)
  to provide line editing and history (by really just copying what
  its introduction provides).

## Grammar
I used [pest](https://pest.rs/) to prototype the grammar, so this is what
I followed *mostly*. I opted to make whitespaces explicit, as there are
places they should not just be skipped/accepted:

    simpleExpr = { ws* ~ andExpr ~ ws* ~ (or ~ simpleExpr)? | andExpr }
    andExpr = { ws* ~ relExpr ~ ws* ~ (and ~ andExpr)? | relExpr }
    relExpr = { ws* ~ sumExpr ~ ws* ~ (relop ~ relExpr)? | sumExpr }
    sumExpr = { ws* ~ unaryExpr ~ ws* ~ (sumop ~ sumExpr)? | unaryExpr }
    relop = { "==" | "!=" | ">=" | "<=" | ">" | "<" | "=~" }
    sumop = { "&" }
    unaryExpr = { ws* ~ unaryOp ~ unaryExpr | factor }
    unaryOp = { "!" | "-" }
    factor = { ws* ~ (identifier | numeral | string | parensExpr){1,1} }
    string = { ws* ~ dquote ~ stringChars ~ dquote }
    dquote = { "\"" }
    stringChars = { (quoteChar | ASCII_ALPHANUMERIC | (chars))* }
    chars = { "." | "~" | "*" | "(" | ")" | "{" | "}" | "[" | "]" | " " | "_" | "-" | "+" | "/" | "`" | "´" | "$" | "%" | "§" | "@" | ":" | ";" | "#" | "'" | "=" | "!" | "^" | "°" }
    quoteChar = { "\\" ~ ("\"" | "\\" | "n" | "r" | "t" | "f" | "v" | "0" ) }
    parensExpr = _{ "(" ~ ws* ~ simpleExpr ~ ws* ~ ")" }
    identifier = @{ name }
    nameHead = { ASCII_ALPHA | "_" }
    nameTail = { ASCII_ALPHANUMERIC | "_" }
    name = { nameHead ~ nameTail* | nameHead+ }
    numeral = @{ ("0x" ~ ASCII_HEX_DIGIT+) | ("0o" ~ ASCII_OCT_DIGIT+) | ("0b" ~ ASCII_BIN_DIGIT+) | ASCII_DIGIT+ }
    or = { "||" }
    and = { "&&" }
    ws = { " " | "\t" }

_Note that I since removed all traces of *pest* as I was not using it anymore._
