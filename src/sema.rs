use crate::nom_parser::{BinaryOp, Node, NodeType, UnaryOp};

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

fn transform_match_not_regex(node: Box<Node>) -> Option<Box<Node>> {
    if let Node::Binary { lhs, op, rhs } = node.as_ref() {
        if matches!(op, &BinaryOp::Match) &&
            (matches!(lhs.as_ref(), Node::StringLiteral(_)) || matches!(lhs.as_ref(), Node::Identifier(_))) {
            if let Node::Unary { op: UnaryOp::Not, expr} = rhs.as_ref() {
                if matches!(expr.as_ref(), Node::Regexp(_)) {
                    let e = Box::new(Node::Binary {lhs: lhs.clone(), op: op.clone(), rhs: expr.clone() });
                    return Some(Box::new(Node::Unary { op: UnaryOp::Not, expr: e }));
                }
            }
        }
    }

    None
}

pub fn transform(node: Box<Node>) -> Box<Node> {
    fn walk(node: Box<Node>) -> Box<Node> {
        match node.as_ref() {
            Node::Binary {lhs, op, rhs} => {
                if let Some(node) = transform_match_not_regex(node.clone()) {
                    dbg!(&node);
                    return node;
                }

                Box::new(Node::Binary {
                    lhs: walk(lhs.clone()),
                    op: op.clone(),
                    rhs: walk(rhs.clone()),
                })
            }

            Node::Unary {op, expr} => {
                Box::new(Node::Unary {
                    op: op.clone(),
                    expr: walk(expr.clone()),
                })
            }

            x => Box::new(x.clone()),
        }
    }

    walk(node)
}