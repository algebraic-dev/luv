use std::collections::HashSet;

use crate::{r#abstract::*, span::Point};

#[derive(Hash, PartialEq, Eq, Debug)]
pub enum Found<'a> {
    Id(&'a Text),
    Expr,
}

impl Program {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        for tl in &self.vec {
            if tl.span.contains_point(point) {
                return tl.find(point, found);
            }
        }

        None
    }
}

impl TopLevel {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        match &self.data {
            TopLevelKind::Defn(defn) => defn.find(point, found),
            TopLevelKind::Def(def) => def.find(point, found),
            TopLevelKind::Eval(eval) => eval.find(point, found),
            _ => None,
        }
    }
}

impl Def {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        self.value.find(point, found)
    }
}

impl Defn {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        for tl in &self.body {
            if tl.span.contains_point(point) {
                return tl.find(point, found);
            }
        }

        None
    }
}

impl Eval {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        for tl in &self.expr {
            if tl.span.contains_point(point) {
                return tl.find(point, found);
            }
        }

        None
    }
}

impl Expr {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        let res = match &self.data {
            ExprKind::Let(value) => value.find(point, found),
            ExprKind::If(value) => value.find(point, found),
            ExprKind::Call(value) => value.find(point, found),
            ExprKind::Lambda(value) => value.find(point, found),
            ExprKind::Identifier(value) => {
                if value.span.contains_point(point) {
                    found.insert(Found::Id(&value));
                    Some(())
                } else {
                    None
                }
            }
            _ => None,
        };

        found.insert(Found::Expr);

        res
    }
}

impl LetExpr {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        self.body.find(point, found)
    }
}

impl IfExpr {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        if self.condition.span.contains_point(point) {
            return self.condition.find(point, found);
        }
        if self.true_branch.span.contains_point(point) {
            return self.true_branch.find(point, found);
        }

        if self.false_branch.span.contains_point(point) {
            return self.false_branch.find(point, found);
        }

        None
    }
}

impl CallExpr {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        if self.callee.span.contains_point(point) {
            found.insert(Found::Id(&self.callee));
            return Some(());
        }

        for tl in &self.arguments {
            if tl.span.contains_point(point) {
                return tl.find(point, found);
            }
        }

        None
    }
}

impl LambdaExpr {
    pub fn find<'a>(&'a self, point: &Point, found: &mut HashSet<Found<'a>>) -> Option<()> {
        for tl in &self.body {
            if tl.span.contains_point(point) {
                return tl.find(point, found);
            }
        }

        None
    }
}
