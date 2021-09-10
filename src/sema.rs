use crate::parser::{BinaryOp, Node, NodeType, UnaryOp};
use std::rc::Rc;

pub fn check(node: &Node) -> Result<(), String> {
    fn walk(node: &Node) -> Result<(), String> {
        match node {
            Node::Binary { lhs, op, rhs } => match op {
                BinaryOp::Match => {
                    if rhs.get_type() != NodeType::Regexp && rhs.get_type() != NodeType::Nil {
                        return Err("Match operator needs a right regex argument".to_string());
                    }
                    if lhs.get_type() != NodeType::Identifier
                        && lhs.get_type() != NodeType::StringLiteral
                    {
                        return Err(
                            "Match operator needs a left string or identifier argument".to_string()
                        );
                    }
                }
                _ => {
                    if rhs.get_type() == NodeType::Regexp || lhs.get_type() == NodeType::Regexp {
                        return Err("Regex not allowed here".to_string());
                    }
                    walk(lhs)?;
                    walk(rhs)?;
                }
            },
            Node::Unary { op: _, expr } => {
                if expr.get_type() == NodeType::Regexp {
                    return Err("Regex not allowed here".to_string());
                }
                walk(expr)?;
            }
            Node::Constant(_num) => (),
            Node::StringLiteral(_s) => (),
            Node::Identifier(_ident) => (),
            Node::Regexp(_re) => (),
            Node::Nil => (),
        }
        Ok(())
    }

    walk(node)
}

fn transform_match_not_regex(node: Rc<Node>) -> Option<Rc<Node>> {
    if let Node::Binary { lhs, op, rhs } = node.as_ref() {
        if matches!(op, &BinaryOp::Match) &&
            (matches!(lhs.as_ref(), Node::StringLiteral(_)) || matches!(lhs.as_ref(), Node::Identifier(_))) {
            if let Node::Unary { op: UnaryOp::Not, expr} = rhs.as_ref() {
                if matches!(expr.as_ref(), Node::Regexp(_)) {
                    let e = Rc::new(Node::Binary {lhs: lhs.clone(), op: *op, rhs: expr.clone() });
                    return Some(Rc::new(Node::Unary { op: UnaryOp::Not, expr: e }));
                }
            }
        }
    }

    None
}

pub fn transform(node: Rc<Node>) -> Rc<Node> {
    fn walk(node: Rc<Node>) -> Rc<Node> {
        match node.as_ref() {
            Node::Binary {lhs, op, rhs} => {
                if let Some(node) = transform_match_not_regex(node.clone()) {
                    return node;
                }

                Rc::new(Node::Binary {
                    lhs: walk(lhs.clone()),
                    op: *op,
                    rhs: walk(rhs.clone()),
                })
            }

            Node::Unary {op, expr} => {
                Rc::new(Node::Unary {
                    op: *op,
                    expr: walk(expr.clone()),
                })
            }

            _ => node,
        }
    }

    walk(node)
}