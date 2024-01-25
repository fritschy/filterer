use std::fmt::{Debug, Display, Formatter};

mod sema;
mod value;

mod machine;
mod parser;

#[cfg(test)]
mod tests;

// Following is our public interface
pub use crate::machine::CompileError;
pub use crate::machine::{AccessorQuery, KeyAccessor};
pub use crate::parser::ParseError;

pub struct ExprEval {
    text: String,
    vm: machine::Machine,
}

impl Debug for ExprEval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExprEval")
            .field("text", &self.text)
            .field(
                "idents",
                &self
                    .vm
                    .ident_names
                    .values()
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
}

impl ExprEval {
    pub fn eval<T: KeyAccessor>(&self, a: &T) -> bool {
        self.vm.eval(a)
    }
}

pub fn compile(text: &str, acc: &dyn AccessorQuery) -> Result<ExprEval, CompileError> {
    let p = parser::parse(text)?;
    let vm = machine::Machine::from_node_and_accessor(&p, acc)?;
    Ok(ExprEval {
        text: text.to_string(),
        vm,
    })
}

impl Display for ExprEval {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.vm)
    }
}
