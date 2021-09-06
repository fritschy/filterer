use crate::nom_parser::{BinaryOp, Node, NodeType};

pub fn check(node: &Node) -> Result<(), String> {
    fn walk(node: &Node) -> Result<(), String> {
        match node {
            Node::Binary {lhs, op, rhs} => {
                match op {
                    BinaryOp::Match => {
                        if rhs.get_type() != NodeType::Regexp {
                            return Err("Match operator needs a right regex argument".to_string());
                        }
                        if lhs.get_type() != NodeType::Identifier && lhs.get_type() != NodeType::StringLiteral {
                            return Err("Match operator needs a left string or identifier argument".to_string());
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