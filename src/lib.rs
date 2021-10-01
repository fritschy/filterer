use std::fmt::{Display, Formatter};

mod sema;
mod value;

mod parser;
mod machine;

#[cfg(test)]
mod tests;

// Following is our public interface
pub use crate::machine::{AccessorQuery, KeyAccessor};
pub use crate::parser::ParseError;

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
