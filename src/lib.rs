// Examples:
// ctx =~ "AP.*" || ctx == "MAP"             # match ctx against re, or ctx equals to "MAP"
// app == "REND" && payload =~ ".*error.*"   # app == "REND" AND payload contains "error"
//
//
// IDENTIFIERS can be defined freely by first "declaring" them when configuring
// the filter, i.e. filter.add_define("app", |msg| msg.app) # where msg is something
// that makes sense in the context.
//
// A parsed filter should result in an AST that can be evaluated, don't particularly
// concern yourself with efficiency, this is a first test.
//
// How to implement chaining of logic operators, i.e. EXPR && EXPR?

mod sema;
mod value;

mod parser;
mod machine;

#[cfg(test)]
mod tests;

use std::fmt::{Display, Formatter};

// Following is our public interface
pub use crate::parser::ParseError;
pub use crate::machine::{KeyAccessor, AccessorQuery};

pub struct ExprEval {
    vm: machine::Machine,
}

impl ExprEval {
    pub fn eval(&self, a: &dyn KeyAccessor) -> bool {
        self.vm.eval(a)
    }
}

pub fn compile<'a>(text: &'a str, acc: &dyn AccessorQuery) -> Result<ExprEval, ParseError<'a>> {
    let p = parser::parse(text)?;
    let vm = machine::Machine::from_node_and_accessor(&p, acc);
    Ok(ExprEval { vm })
}

impl Display for ExprEval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.vm)
    }
}
