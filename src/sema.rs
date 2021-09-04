use crate::nom_parser::{
    BinaryOp,
    Node,
};

pub struct Error {
    pub diag: String,
}

pub fn check(node: &Node) -> Result<(), Error> {
    fn walk(node: &Node) -> Result<(), Error> {
        match node {
            Node::Binary {lhs, op, rhs} => {
                match op {
                    BinaryOp::Match => {
                        if rhs.name() != "regex" {
                            return Err(Error {diag: "Match operator needs a right regex argument".to_string()});
                        }
                        if lhs.name() != "ident" && lhs.name() != "string" {
                            return Err(Error {diag: "Match operator needs a left string or identifier argument".to_string()});
                        }
                    }
                    _ => {
                        walk(lhs)?;
                        walk(rhs)?;
                    }
                }
            }
            Node::Unary {op: _, expr} => {
                walk(expr)?;
            }
            Node::Constant(_num) => {

            }
            Node::StringLiteral(_s) => {

            }
            Node::Identifier(_ident) => {

            }
            Node::Regexp(_re) => {

            }
        }
        Ok(())
    }

    walk(node)
}