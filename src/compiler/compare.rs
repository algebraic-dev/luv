use std::collections::HashMap;
use crate::{compiler::syntax::SyntaxKind, span::Span};

use super::concrete::SyntaxNode;

#[derive(Debug)]
pub enum Change<'a> {
    Added(&'a SyntaxNode),
    Removed(&'a SyntaxNode),
}

fn filter_top_level_children<'a>(node: &SyntaxNode) -> impl Iterator<Item = &SyntaxNode> {
    node.nodes()
        .filter(|child| !matches!(child.kind(), SyntaxKind::Whitespace | SyntaxKind::Comment))
}

fn is_affected_by_changed(span: &Span, changed_spans: &[Span]) -> bool {
    changed_spans.iter().any(|changed| {
        span.after(changed) || span.intersect(changed)
    })
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

pub fn compare_top_level_nodes<'a>(a: &'a SyntaxNode, b: &'a SyntaxNode, changes: &mut Vec<Change<'a>>, changed: &[Span]) {
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