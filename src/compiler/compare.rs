use core::fmt;
use std::collections::{HashMap, HashSet};

use crate::{compiler::syntax::SyntaxKind, span::{Point, Span}};

use super::concrete::SyntaxNode;

#[derive(Debug)]
pub enum Change<'a> {
    Added(&'a SyntaxNode),
    Removed(&'a SyntaxNode),
    Changed(&'a SyntaxNode, &'a SyntaxNode),
}

fn filter_top_level_children<'a>(node: &SyntaxNode) -> impl Iterator<Item = &SyntaxNode> {
    node.nodes()
        .filter(|child| !matches!(child.kind(), SyntaxKind::Whitespace | SyntaxKind::Comment))
}

fn is_affected_by_changed(span: &Span, changed: &Span) -> bool {
    span.after(changed) || span.intersect(changed)
}

fn adjust_span(span: &Span, changed: &Span) -> Span {
    if span.after(changed) {
        let diff_start = span.start.subtract(&changed.end);
        let diff_end = span.end.subtract(&changed.end);
        
        Span {
            start: diff_start,
            end: diff_end,
        }
    } else {
        span.clone()
    }
}

pub fn compare_top_level_nodes<'a>(a: &'a SyntaxNode, b: &'a SyntaxNode, changes: &mut Vec<Change<'a>>, changed: &Span) {
    let a_nodes = filter_top_level_children(a).collect::<Vec<_>>();
    let b_nodes = filter_top_level_children(b).collect::<Vec<_>>();

    let mut a_map = HashMap::new();
    let mut b_map = HashMap::new();

    for a_node in a_nodes {
        if is_affected_by_changed(&a_node.span, changed) {
            let adjusted_span = adjust_span(&a_node.span, changed);
            a_map.insert((a_node.hash, adjusted_span), a_node);
        }
    }

    for b_node in b_nodes {
        if is_affected_by_changed(&b_node.span, changed) {
            let adjusted_span = adjust_span(&b_node.span, changed);
            b_map.insert((b_node.hash, adjusted_span), b_node);
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


const RESET: &str = "\x1b[0m";
const GREEN: &str = "\x1b[32m"; // Color for Added
const RED: &str = "\x1b[31m";   // Color for Removed
const YELLOW: &str = "\x1b[33m"; // Color for Changed

impl<'a> fmt::Display for Change<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Change::Added(node) => {
                writeln!(f, "{}Added:", GREEN)?;
                write!(f, "{}", node)?;
            }
            Change::Removed(node) => {
                writeln!(f, "{}Removed:", RED)?;
                write!(f, "{}", node)?;
            }
            Change::Changed(before, after) => {
                writeln!(f, "{}Changed:{}\n", YELLOW, RESET)?;
                writeln!(f, "  Before:")?;
                write!(f, "{}", before)?;
                writeln!(f, "  After:")?;
                write!(f, "{}", after)?;
            }
        }
        write!(f, "{}", RESET)
    }
}