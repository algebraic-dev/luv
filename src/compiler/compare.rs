use crate::{compiler::syntax::SyntaxKind, span::Span};
use core::fmt;
use std::collections::HashMap;

use super::concrete::SyntaxNode;

#[derive(Debug)]
pub enum Change<'a> {
    Added(&'a SyntaxNode),
    Removed(&'a SyntaxNode),
}

impl<'a> Change<'a> {
    pub fn split(self, removed: &mut Vec<SyntaxNode>, added: &mut Vec<SyntaxNode>) {
        match self {
            Change::Added(add) => added.push(add.clone()),
            Change::Removed(rem) => removed.push(rem.clone()),
        }
    }
}

fn filter_top_level_children<'a>(node: &SyntaxNode) -> impl Iterator<Item = &SyntaxNode> {
    node.nodes()
        .filter(|child| !matches!(child.kind(), SyntaxKind::Whitespace | SyntaxKind::Comment))
}

fn is_affected_by_changed(span: &Span, changed_spans: &[Span]) -> bool {
    changed_spans
        .iter()
        .any(|changed| span.after(changed) || span.intersect(changed))
}

fn adjust_span(span: &Span, changed_spans: &[Span]) -> Span {
    let mut adjusted_span = span.clone();
    for changed in changed_spans {
        if adjusted_span.after(changed) {
            let diff = changed.end.subtract(&changed.start);
            let diff_start = adjusted_span.start.subtract(&diff);
            let diff_end = adjusted_span.end.subtract(&diff);
            adjusted_span = Span {
                start: diff_start,
                end: diff_end,
            };
        }
    }
    adjusted_span
}

impl<'a> fmt::Display for Change<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Change::Added(added) => {
                writeln!(f, "Added")?;
                writeln!(f, "{}", added)
            }
            Change::Removed(rem) => {
                writeln!(f, "Removed")?;
                writeln!(f, "{}", rem)
            }
        }
    }
}

pub fn compare_top_level_nodes<'a>(
    a: &'a SyntaxNode,
    b: &'a SyntaxNode,
    changes: &mut Vec<Change<'a>>,
    changed: &[Span],
) {
    let a_nodes = filter_top_level_children(a).collect::<Vec<_>>();
    let b_nodes = filter_top_level_children(b).collect::<Vec<_>>();

    let mut a_map = HashMap::new();
    let mut b_map = HashMap::new();

    for a_node in a_nodes {
        if is_affected_by_changed(&a_node.span, changed) {
            let adjusted_span = adjust_span(&a_node.span, changed);
            a_map.insert((a_node.hash, adjusted_span.clone()), a_node);
        }
    }

    for b_node in b_nodes {
        if is_affected_by_changed(&b_node.span, changed) {
            let adjusted_span = adjust_span(&b_node.span, changed);
            b_map.insert((b_node.hash, adjusted_span.clone()), b_node);
        }
    }

    for (b_node, value) in &b_map {
        if !a_map.contains_key(b_node) {
            changes.push(Change::Added(value));
        }
    }

    for (a_node, value) in &a_map {
        if !b_map.contains_key(a_node) {
            changes.push(Change::Removed(value));
        }
    }
}

pub fn compare<'a>(a: &'a SyntaxNode, b: &'a SyntaxNode, changed: &[Span]) -> Vec<Change<'a>> {
    let mut changes = Vec::new();
    compare_top_level_nodes(a, b, &mut changes, changed);
    changes
}

#[cfg(test)]
mod test {
    use crate::{
        compiler::{compare::compare, lexer::Lexer, parser::Parser},
        span::{Point, Span},
    };

    #[test]
    fn lexer_test() {
        let input = r#"(a 4 '(5 2 1) 3c)"#;
        let input2 = r#"(a 4 '(5 2 1) 3c)"#;

        let (syntax1, _errors) = Parser::new(Lexer::new(input)).parse();
        let (syntax2, _errors) = Parser::new(Lexer::new(input2)).parse();

        let changes = compare(
            &syntax1,
            &syntax2,
            &[Span::new(Point::new(0, 6), Point::new(0, 6))],
        );

        for change in changes {
            println!("{}", change);
        }
    }
}
