use std::collections::HashMap;

use crate::{
    errors::Error,
    hierarchy::HierarchyBuilder,
    r#abstract::*,
    scope::Scope,
    span::{Span, Spanned},
    syntax::{SyntaxKind, SyntaxNode},
};

#[derive(Default)]
pub struct Data {
    pub errors: Vec<Error>,
}

impl Data {
    fn expected<'a>(&mut self, kind: SyntaxKind, list: &'a SyntaxNode) {
        self.error(
            list.span.clone(),
            format!("expected a {kind} got {}", list.kind),
        );
    }

    fn error(&mut self, span: Span, msg: String) {
        self.errors.push(Error::new(msg, span))
    }

    fn list<'a>(&mut self, list: &'a SyntaxNode) -> Option<impl Iterator<Item = &'a SyntaxNode>> {
        match list.kind {
            SyntaxKind::List => Some(list.nodes()),
            _ => {
                self.expected(SyntaxKind::List, list);
                None
            }
        }
    }

    fn identifier<'a>(&mut self, node: &'a SyntaxNode) -> Option<Text> {
        let span = node.span.clone();

        if let SyntaxKind::Identifier = node.kind {
            let token = node.first_token().unwrap();
            return Some(Text {
                data: token.text.clone(),
                span: token.span.clone(),
            });
        }

        self.error(span, "expected an identifier.".to_owned());
        None
    }

    fn next_identifier<'a>(
        &mut self,
        span: Span,
        iter: &mut impl Iterator<Item = &'a SyntaxNode>,
    ) -> Option<Text> {
        if let Some(next) = iter.next() {
            return self.identifier(next);
        }
        self.error(span, "expected an identifier.".to_owned());
        None
    }

    fn next<'a, T: Into<String>>(
        &mut self,
        iter: &mut impl Iterator<Item = &'a SyntaxNode>,
        span: Span,
        message: T,
    ) -> Option<&'a SyntaxNode> {
        if let Some(node) = iter.next() {
            Some(node)
        } else {
            self.error(
                span.clone(),
                format!("expected {} got nothing.", message.into()),
            );
            None
        }
    }

    pub fn parse_program<'a>(
        &mut self,
        list: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Vec<TopLevel> {
        let mut vec = Vec::new();

        for arg in list {
            vec.push(self.top_level_parse(arg));
        }

        vec
    }

    pub fn top_level_parse<'a>(&mut self, list: &'a SyntaxNode) -> TopLevel {
        let span = list.span.clone();

        if let Some(mut children) = self.list(list) {
            if let Some(identifier) = self.next_identifier(span.clone(), &mut children) {
                match identifier.data.as_str() {
                    "defn" => return self.parse_defn(span.clone(), children),
                    "def" => return self.parse_def(span.clone(), children),
                    "require" => return self.parse_require(span.clone(), children),
                    "eval" => return self.parse_eval(span.clone(), children),
                    _ => self.error(span.clone(), "invalid top level syntax".to_owned()),
                }
            }
        }

        Spanned::new(TopLevelKind::Error, span)
    }

    pub fn parse_params<'a>(
        &mut self,
        span: Span,
        iter: &mut impl Iterator<Item = &'a SyntaxNode>,
    ) -> Option<Vec<Text>> {
        let mut vec = Vec::new();

        let next = self.next(iter, span, "expeted a parameter list.")?;
        let next = self.list(next)?;

        for child in next {
            if let Some(id) = self.identifier(child) {
                vec.push(id)
            }
        }

        Some(vec)
    }

    pub fn drain<'a>(&mut self, mut iter: impl Iterator<Item = &'a SyntaxNode>) {
        while let Some(res) = iter.next() {
            self.error(res.span.clone(), "useless argument. remove it.".to_owned())
        }
    }

    pub fn parse_defn<'a>(
        &mut self,
        span: Span,
        mut iter: impl Iterator<Item = &'a SyntaxNode>,
    ) -> TopLevel {
        let name = self.next_identifier(span.clone(), &mut iter);
        let params = self.parse_params(span.clone(), &mut iter);

        let mut body = Vec::new();

        while let Some(next) = iter.next() {
            let expr = self.parse_expr(next);
            body.push(expr);
        }

        let defn = if let Some((name, params)) = name.zip(params) {
            TopLevelKind::Defn(Defn { name, params, body })
        } else {
            TopLevelKind::Error
        };

        TopLevel::new(defn, span)
    }

    pub fn parse_def<'a>(
        &mut self,
        span: Span,
        mut iter: impl Iterator<Item = &'a SyntaxNode>,
    ) -> TopLevel {
        let name = self.next_identifier(span.clone(), &mut iter);
        let body = if let Some(node) = self.next(&mut iter, span.clone(), "expected value") {
            Some(self.parse_expr(node))
        } else {
            None
        };
        let body = if let Some((name, value)) = name.zip(body) {
            TopLevelKind::Def(Def {
                name,
                value: Box::new(value),
            })
        } else {
            TopLevelKind::Error
        };

        self.drain(iter);

        TopLevel::new(body, span)
    }

    pub fn parse_require<'a>(
        &mut self,
        span: Span,
        mut iter: impl Iterator<Item = &'a SyntaxNode>,
    ) -> TopLevel {
        let name = self.next(&mut iter, span.clone(), "expected a path.");

        let name = name.and_then(|node| match node.kind {
            SyntaxKind::String => {
                let token = node.first_token().unwrap();
                Some(Text::new(token.text.clone(), token.span.clone()))
            }
            _ => {
                self.error(node.span.clone(), "expected a path as a string.".to_owned());
                None
            }
        });

        let mut options = Vec::new();

        while let Some(next) = iter.next() {
            if let Some(mut option) = self.list(next) {
                let key = self.next_identifier(span.clone(), &mut option);
                let value = self.next_identifier(span.clone(), &mut option);
                if let Some((key, value)) = key.zip(value) {
                    options.push((key, value));
                } else {
                    self.error(span.clone(), "invalid require option".to_owned());
                }
            } else {
                self.error(span.clone(), "expected a list for options".to_owned());
            }
        }

        let require = if let Some(name) = name {
            TopLevelKind::Require(Require { name, options })
        } else {
            TopLevelKind::Error
        };

        self.drain(iter);

        TopLevel::new(require, span)
    }

    pub fn parse_eval<'a>(
        &mut self,
        span: Span,
        mut iter: impl Iterator<Item = &'a SyntaxNode>,
    ) -> TopLevel {
        let mut exprs = Vec::new();

        while let Some(next) = iter.next() {
            exprs.push(self.parse_expr(next));
        }

        TopLevel::new(TopLevelKind::Eval(Eval { expr: exprs }), span)
    }

    pub fn parse_expr<'a>(&mut self, node: &'a SyntaxNode) -> Expr {
        let span = node.span.clone();

        match node.kind {
            SyntaxKind::Identifier => self.parse_identifier_expr(node, span),
            SyntaxKind::Number => self.parse_literal_expr(node, span),
            SyntaxKind::String => self.parse_literal_expr(node, span),
            SyntaxKind::Atom => self.parse_literal_expr(node, span),
            SyntaxKind::List => self.parse_list_expr(node, span),
            _ => {
                self.error(span.clone(), "unexpected syntax node".to_owned());
                Expr::new(ExprKind::Error, span)
            }
        }
    }

    fn parse_list_expr<'a>(&mut self, node: &'a SyntaxNode, span: Span) -> Expr {
        if let Some(mut children) = self.list(node) {
            if let Some(first) = children.next() {
                if let Some(keyword) = self.identifier(first) {
                    match keyword.data.as_str() {
                        "let" => return self.parse_let_expr(span, children),
                        "if" => return self.parse_if_expr(span, children),
                        "fn" => return self.parse_lambda_expr(span, children),
                        _ => return self.parse_call_expr(keyword, span, children),
                    }
                }
            }
        }
        Expr::new(ExprKind::Error, span)
    }

    fn parse_identifier_expr<'a>(&mut self, node: &'a SyntaxNode, span: Span) -> Expr {
        let id = self.identifier(node).unwrap();
        Expr::new(ExprKind::Identifier(id.clone()), span)
    }

    fn parse_literal_expr<'a>(&mut self, node: &'a SyntaxNode, span: Span) -> Expr {
        let token = node.first_token().unwrap();
        let literal_kind = match token.kind {
            SyntaxKind::String => LiteralKind::String,
            SyntaxKind::Number => LiteralKind::Number,
            SyntaxKind::Atom => LiteralKind::Atom,
            _ => unreachable!("unexpected literal kind"),
        };
        let literal = Spanned::new(literal_kind, token.span.clone());
        Expr::new(ExprKind::Literal(literal), span)
    }

    fn parse_let_expr<'a>(
        &mut self,
        span: Span,
        mut children: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Expr {
        let name = self.next_identifier(span.clone(), &mut children);
        let body = self
            .next(&mut children, span.clone(), "expected body")
            .map(|node| self.parse_expr(node));

        self.drain(children);

        if let Some(name) = name {
            if let Some(body) = body {
                let let_expr = LetExpr {
                    name,
                    body: Box::new(body),
                };

                Expr::new(ExprKind::Let(let_expr), span)
            } else {
                Expr::new(ExprKind::Error, span)
            }
        } else {
            Expr::new(ExprKind::Error, span)
        }
    }

    fn parse_if_expr<'a>(
        &mut self,
        span: Span,
        mut children: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Expr {
        let condition = self
            .next(&mut children, span.clone(), "expected condition")
            .map(|node| self.parse_expr(node));
        let true_branch = self
            .next(&mut children, span.clone(), "expected true branch")
            .map(|node| self.parse_expr(node));
        let false_branch = self
            .next(&mut children, span.clone(), "expected false branch")
            .map(|node| self.parse_expr(node));

        self.drain(children);

        if let Some(((condition, true_branch), false_branch)) =
            condition.zip(true_branch).zip(false_branch)
        {
            let if_expr = IfExpr {
                condition: Box::new(condition),
                true_branch: Box::new(true_branch),
                false_branch: Box::new(false_branch),
            };
            Expr::new(ExprKind::If(if_expr), span)
        } else {
            Expr::new(ExprKind::Error, span)
        }
    }

    fn parse_call_expr<'a>(
        &mut self,
        callee: Text,
        span: Span,
        mut args: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Expr {
        let mut arguments = Vec::new();

        while let Some(arg) = args.next() {
            arguments.push(self.parse_expr(arg));
        }

        let call_expr = CallExpr {
            callee: Box::new(Expr::new(ExprKind::Identifier(callee.clone()), callee.span)),
            arguments,
        };

        Expr::new(ExprKind::Call(call_expr), span)
    }

    fn parse_lambda_expr<'a>(
        &mut self,
        span: Span,
        mut children: impl Iterator<Item = &'a SyntaxNode>,
    ) -> Expr {
        let parameters = self.parse_params(span.clone(), &mut children);

        let mut body = Vec::new();
        while let Some(next) = children.next() {
            body.push(self.parse_expr(next));
        }

        if let Some(parameters) = parameters {
            let lambda_expr = LambdaExpr {
                parameters: parameters.into_iter().map(|p| p.data).collect(),
                body,
            };
            Expr::new(ExprKind::Lambda(lambda_expr), span)
        } else {
            Expr::new(ExprKind::Error, span)
        }
    }
}

pub fn to_abstract(syntax: &SyntaxNode) -> (Data, Program) {
    let mut data = Data::default();
    let vec = data.parse_program(syntax.nodes());
    (
        data,
        Program {
            vec,
            span: syntax.span.clone(),
        },
    )
}

pub struct ScopeTracker {
    pub scopes: HierarchyBuilder<Scope>,
    pub unbound: HashMap<String, Vec<Text>>,
    pub defined: HashMap<String, Vec<Text>>,
}

impl ScopeTracker {
    pub fn new(span: Span) -> Self {
        Self {
            scopes: HierarchyBuilder::new(span),
            unbound: Default::default(),
            defined: Default::default(),
        }
    }

    fn open(&mut self, span: Span) {
        self.scopes.open(span)
    }

    fn close(&mut self) {
        self.scopes.close()
    }

    fn add(&mut self, name: String, span: Span) {
        let scope = self.scopes.get();
        scope.add(name, span)
    }

    fn find(&mut self, span: Span, name: String) -> Option<Span> {
        let scopes = self.scopes.accumulate(span.clone());

        for site in scopes {
            if let Some(span) = site.find(&name, &span) {
                return Some(span);
            }
        }

        if let Some(span) = self.defined.get(&name) {
            return Some(span.first().unwrap().span.clone())
        }

        None
    }

    fn check_expr(&mut self, expr: &Expr) {
        match &expr.data {
            ExprKind::Let(let_expr) => {
                self.check_expr(&let_expr.body);
                self.add(let_expr.name.data.clone(), expr.span.clone());
            }
            ExprKind::If(if_expr) => {
                self.check_expr(&if_expr.condition);
                self.open(if_expr.true_branch.span.clone());
                self.check_expr(&if_expr.true_branch);
                self.close();
                self.open(if_expr.false_branch.span.clone());
                self.check_expr(&if_expr.false_branch);
                self.close();
            }
            ExprKind::Call(call_expr) => {
                self.check_expr(&call_expr.callee);
                for arg in &call_expr.arguments {
                    self.check_expr(arg);
                }
            }
            ExprKind::Lambda(lambda_expr) => {
                self.open(expr.span.clone());
                for param in &lambda_expr.parameters {
                    self.add(param.clone(), expr.span.clone());
                }
                for body_expr in &lambda_expr.body {
                    self.check_expr(body_expr);
                }
                self.close();
            }
            ExprKind::Identifier(identifier) => {
                if self
                    .find(identifier.span.clone(), identifier.data.clone())
                    .is_none()
                {
                    self.unbound
                        .entry(identifier.data.clone())
                        .or_default()
                        .push(identifier.clone());
                }
            }
            _ => {}
        }
    }

    pub fn check_program(&mut self, program: &Program) {
        for top_level in &program.vec {
            self.check_top_level(top_level);
        }
    }

    fn check_top_level(&mut self, top_level: &TopLevel) {
        match &top_level.data {
            TopLevelKind::Defn(defn) => {
                self.open(top_level.span.clone());
                for param in &defn.params {
                    self.add(param.data.clone(), param.span.clone());
                }
                for body_expr in &defn.body {
                    self.check_expr(body_expr);
                }
                self.close();
            }
            TopLevelKind::Def(def) => {
                self.open(top_level.span.clone());
                self.check_expr(&def.value);
                self.close();
            }
            TopLevelKind::Eval(eval) => {
                for expr in &eval.expr {
                    self.check_expr(expr);
                }
            }
            _ => {}
        }
    }

    fn register_toplevel(&mut self, top_level: &TopLevel) {
        match &top_level.data {
            TopLevelKind::Defn(defn) => {
                self.defined
                    .entry(defn.name.data.clone())
                    .or_default()
                    .push(defn.name.clone());
            }
            TopLevelKind::Def(def) => {
                self.defined
                    .entry(def.name.data.clone())
                    .or_default()
                    .push(def.name.clone());
            }
            TopLevelKind::Require(req) => {
                for (_, rename) in &req.options {
                    self.defined
                        .entry(rename.data.clone())
                        .or_default()
                        .push(rename.clone());
                }
            }
            _ => {}
        }
    }

    pub fn register_program(&mut self, program: &Program) {
        for tl in &program.vec {
            self.register_toplevel(tl)
        }
    }
}

pub fn get_scopes(program: &Program) -> ScopeTracker {
    let mut tracker = ScopeTracker::new(program.span.clone());
    tracker.register_program(program);
    tracker.check_program(program);
    tracker
}
