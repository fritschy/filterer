use std::sync::Arc;

use crate::parser::{BinaryOp, Node, UnaryOp};

pub(crate) fn check(node: &Node) -> Result<(), String> {
    fn walk(node: &Node) -> Result<(), String> {
        match node {
            Node::Binary { lhs, op, rhs } => match op {
                BinaryOp::Match => {
                    if !matches!(rhs.as_ref(), Node::Regexp(_))
                        && !matches!(rhs.as_ref(), Node::Nil) {
                        return Err("Match operator needs a right regex argument".to_string());
                    }
                    if !matches!(lhs.as_ref(), Node::Identifier(_))
                        && !matches!(lhs.as_ref(), Node::StringLiteral(_))
                        && !matches!(lhs.as_ref(), Node::IndexedIdentifier(_, _))
                    {
                        return Err(
                            "Match operator needs a left string or identifier argument".to_string()
                        );
                    }
                }
                _ => {
                    if matches!(rhs.as_ref(), Node::Regexp(_))
                        || matches!(lhs.as_ref(), Node::Regexp(_)) {
                        return Err("Regex not allowed here".to_string());
                    }
                    walk(lhs)?;
                    walk(rhs)?;
                }
            },
            Node::Unary { op: _, expr } => {
                if matches!(expr.as_ref(), Node::Regexp(_)) {
                    return Err("Regex not allowed here".to_string());
                }
                walk(expr)?;
            }
            _ => (), // Nothing we can do here...
        }
        Ok(())
    }

    walk(node)
}

fn transform_match_not_regex(node: &Arc<Node>) -> Option<Arc<Node>> {
    if let Node::Binary { lhs, op, rhs } = node.as_ref() {
        if matches!(op, &BinaryOp::Match) &&
            (matches!(lhs.as_ref(), Node::StringLiteral(_)) ||
             matches!(lhs.as_ref(), Node::Identifier(_)) ||
             matches!(lhs.as_ref(), Node::IndexedIdentifier(_, _))) {
            if let Node::Unary { op: UnaryOp::Not, expr} = rhs.as_ref() {
                if matches!(expr.as_ref(), Node::Regexp(_)) {
                    let e = Arc::new(Node::Binary {lhs: lhs.clone(), op: *op, rhs: expr.clone() });
                    return Some(Arc::new(Node::Unary { op: UnaryOp::Not, expr: e }));
                }
            }
        }
    }

    None
}

pub(crate) fn transform(node: &Arc<Node>) -> Arc<Node> {
    fn walk(node: &Arc<Node>) -> Arc<Node> {
        match node.as_ref() {
            Node::Binary {lhs, op, rhs} => {
                if let Some(node) = transform_match_not_regex(node) {
                    return node;
                }

                Arc::new(Node::Binary {
                    lhs: walk(lhs),
                    op: *op,
                    rhs: walk(rhs),
                })
            }

            Node::Unary {op, expr} => {
                Arc::new(Node::Unary {
                    op: *op,
                    expr: walk(expr),
                })
            }

            _ => node.clone(),
        }
    }

    walk(node)
}
