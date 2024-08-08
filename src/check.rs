use std::collections::{HashMap, HashSet};

use crate::{
    compiler::{concrete::SyntaxNode, r#abstract::*},
    environment::Manager,
    span::{Span, Spanned},
};

pub type Local = im_rc::HashMap<String, Span>;

pub struct Info<'a> {
    pub name: Option<&'a str>,
}

///! The checker for the AST. It checks information useful for compilation.

pub struct Context<'a> {
    env: &'a mut Manager,
    errs: &'a mut Vec<Spanned<String>>,
    references: HashMap<Span, Span>,
    current: Vec<Span>,
    pub scopes: HashMap<Span, HashMap<String, Span>>,
}

impl<'a> Context<'a> {
    pub fn new(env: &'a mut Manager, errs: &'a mut Vec<Spanned<String>>) -> Self {
        Self {
            env,
            errs,
            references: HashMap::new(),
            scopes: HashMap::new(),
            current: vec![]
        }
    }

    fn push_on_scope(&mut self, name: String, span: Span) {
        if let Some(scope) = self.current.get(0) {
            let vec = self.scopes.entry(scope.clone()).or_default();
            vec.insert(name, span);
        }
    }

    fn scope(&mut self, span: Span) {
        self.current.push(span);
    }

    fn pop_scope(&mut self) {
        self.current.pop();
    }

    fn push_error<T>(&mut self, err: Result<T, Spanned<String>>) -> Option<T> {
        match err {
            Err(err) => {
                self.errs.push(err);
                None
            }
            Ok(ok) => Some(ok),
        }
    }

    fn get<T>(&mut self, name: Result<T, Spanned<String>>) -> Option<T> {
        let name = self.push_error(name);
        name
    }

    fn check_ref(&mut self, local: im_rc::HashMap<String, Span>, name: String, span: Span) {
        if let Some(refe) = local.get(&name) {
            self.references.insert(span, refe.clone());
        } else if let Some((refe, _)) = self.env.definitions.get(&name) {
            self.references.insert(span, refe.clone());
        } else {
            self.errs.push(Spanned::new(
                format!("cannot find definition `{name}`"),
                span.clone(),
            ))
        }
    }

    fn check_useless(&mut self, mut arg: List) {
        while !arg.is_empty() {
            let next = arg.next("useless argument");
            self.push_error(next);
        }
    }

    fn check_if(&mut self, local: Local, mut expr: If) -> Option<()> {
        let cond = expr.cond();

        if let Some(cond) = self.push_error(cond) {
            self.check_expr(local.clone(), cond);
        }

        let then = expr.then();

        if let Some(then) = self.push_error(then) {
            self.check_expr(local.clone(), then);
        }

        let else_ = expr.else_();

        if let Some(else_) = self.push_error(else_) {
            self.check_expr(local, else_);
        }

        Some(())
    }

    fn check_fn(&mut self, mut local: Local, mut expr: Fn) -> Option<()> {
        let span = expr.span();
        let params = expr.params();
        let params = self.push_error(params);

        self.scope(span);

        if let Some(mut params) = params {
            let names = params.names();
            for name in names {
                if let Some(name) = self.push_error(name) {
                    let span = name.span();
                    if let Some(name) = self.push_error(name.text()) {
                        local.insert(name.to_string(), span.clone());
                        self.push_on_scope(name.to_string(), span);
                    }
                }
            }
        }

        while let Ok(stmt) = expr.0.next("") {
            if let Some(stmt) = self.push_error(Stmt::from_node(stmt)) {
                if let Some(n_local) = self.check_stmt(local.clone(), stmt) {
                    local = n_local
                }
            }
        }

        self.pop_scope();

        None
    }

    fn check_app(&mut self, local: Local, mut expr: App) -> Option<()> {
        let id = expr.name();

        if let Some(identifier) = self.push_error(id) {
            let span = identifier.span();
            if let Some(name) = self.push_error(identifier.text()) {
                self.check_ref(local.clone(), name.to_string(), span)
            }
        }

        loop {
            let arg = expr.argument();
            let arg = self.push_error(arg);
            if let Some(arg) = arg {
                if let Some(arg) = arg {
                    self.check_expr(local.clone(), arg);
                } else {
                    break;
                }
            }
        }

        Some(())
    }

    fn check_quote(&mut self, _local: Local, _expr: Quote) -> Option<()> {
        Some(())
    }

    fn check_identifier(&mut self, local: Local, expr: Identifier) -> Option<()> {
        let span = expr.span();
        let name = expr.text();
        let name = self.get(name).map(str::to_string)?;

        self.check_ref(local, name, span);

        Some(())
    }

    fn check_number(&mut self, _local: Local, _expr: Number) -> Option<()> {
        Some(())
    }

    fn check_str(&mut self, _local: Local, _expr: Str) -> Option<()> {
        Some(())
    }

    fn check_block(&mut self, mut local: Local, mut expr: Block) -> Option<()> {
        let span = expr.span();
        self.scope(span);

        for stmt in expr.stmt() {
            if let Some(res) = self.push_error(stmt) {
                if let Some(new_local) = self.check_stmt(local.clone(), res) {
                    local = new_local
                }
            }
        }

        self.pop_scope();

        Some(())
    }

    fn check_expr(&mut self, local: Local, expr: Expr) -> Option<()> {
        match expr {
            Expr::If(if_expr) => self.check_if(local.clone(), if_expr),
            Expr::Fn(fn_expr) => self.check_fn(local.clone(), fn_expr),
            Expr::App(app_expr) => self.check_app(local.clone(), app_expr),
            Expr::Quote(quote_expr) => self.check_quote(local.clone(), quote_expr),
            Expr::Identifier(identifier_expr) => {
                self.check_identifier(local.clone(), identifier_expr)
            }
            Expr::Number(mumber_expr) => self.check_number(local.clone(), mumber_expr),
            Expr::Str(str_expr) => self.check_str(local.clone(), str_expr),
            Expr::Block(block) => self.check_block(local.clone(), block),
        }
    }

    fn check_let(&mut self, mut local: Local, mut expr: Let) -> Option<Local> {
        let id = expr.name();

        if let Some(identifier) = self.push_error(id) {
            let span = identifier.span();
            if let Some(name) = self.push_error(identifier.text()) {
                local.insert(name.to_string(), span.clone());
                self.push_on_scope(name.to_string(), span);
            }
        }

        let value = expr.value();
        let value = self.push_error(value);

        if let Some(value) = value {
            self.check_expr(local.clone(), value);
        }

        Some(local)
    }

    fn check_stmt(&mut self, local: Local, stmt: Stmt) -> Option<Local> {
        match stmt {
            Stmt::Let(let_stmt) => self.check_let(local.clone(), let_stmt),
            Stmt::Expr(expr) => {
                self.check_expr(local.clone(), expr);
                None
            }
        }
    }

    fn check_def(&mut self, local: Local, mut def: Def) -> Option<Spanned<String>> {
        let span = def.span();
        let name = def.name();
        let name = self.get_name_ref(name);

        let value = def.body();
        let value = self.push_error(value);

        self.scope(span);

        if let Some(value) = value {
            self.check_stmt(local.clone(), value);
        }

        self.check_useless(def.0);

        self.pop_scope();

        name
    }

    fn check_defn(&mut self, mut local: Local, mut defn: Defn) -> Option<Spanned<String>> {
        let span = defn.span();

        let name = defn.name();
        let name = self.get_name_ref(name);

        let params = defn.parameters();
        let params = self.push_error(params);

        self.scope(span);

        if let Some(mut params) = params {
            let names = params.names();
            for name in names {
                if let Some(name) = self.push_error(name) {
                    let span = name.span();
                    if let Some(name) = self.push_error(name.text()) {
                        local.insert(name.to_string(), span.clone());
                        self.push_on_scope(name.to_string(), span);
                    }
                }
            }
        }

        while let Ok(stmt) = defn.0.next("") {
            if let Some(stmt) = self.push_error(Stmt::from_node(stmt)) {
                if let Some(n_local) = self.check_stmt(local.clone(), stmt) {
                    local = n_local
                }
            }
        }

        self.pop_scope();

        name
    }

    fn check_eval(&mut self, mut local: Local, mut eval: Eval) -> Option<Spanned<String>> {
        let span = eval.span();

        self.scope(span);

        while let Ok(stmt) = eval.0.next("") {
            if let Some(stmt) = self.push_error(Stmt::from_node(stmt)) {
                if let Some(n_local) = self.check_stmt(local.clone(), stmt) {
                    local = n_local
                }
            }
        }

        self.pop_scope();

        None
    }

    fn check_set_option(&mut self, _local: Local, mut _set_option: SetOption) -> Option<Spanned<String>> {
        None
    }

    fn check_require(&mut self, _local: Local, mut _require: Require) -> Option<Spanned<String>> {
        None
    }

    fn check_top_level(&mut self, top_level: TopLevel) -> Option<Spanned<String>> {
        match top_level {
            TopLevel::Def(node) => self.check_def(Default::default(), node),
            TopLevel::Defn(node) => self.check_defn(Default::default(), node),
            TopLevel::Eval(node) => self.check_eval(Default::default(), node),
            TopLevel::SetOption(node) => self.check_set_option(Default::default(), node),
            TopLevel::Require(node) => self.check_require(Default::default(), node),
        }
    }

    pub fn check(&mut self, top_level: SyntaxNode) -> Option<Spanned<String>> {
        match TopLevel::from_node(top_level) {
            Ok(top_level) => self.check_top_level(top_level),
            Err(err) => {
                self.errs.push(err);
                None
            }
        }
    }

    fn get_name(&mut self, top_level: TopLevel) -> Option<Spanned<String>> {
        match top_level {
            TopLevel::Def(mut node) => {
                let name = node.name();
                self.get_name_ref(name)
            }
            TopLevel::Defn(mut node) => {
                let name = node.name();
                self.get_name_ref(name)
            }
            _ => None,
        }
    }

    fn get_name_ref(
        &mut self,
        name: Result<Identifier, Spanned<String>>,
    ) -> Option<Spanned<String>> {
        let name = self.get(name)?;
        let span = name.span();
        let name = self.push_error(name.text())?;
        Some(Spanned::new(name.to_string(), span))
    }

    pub fn get_ref(&mut self, top_level: SyntaxNode) -> Option<Spanned<String>> {
        match TopLevel::from_node(top_level) {
            Ok(top_level) => self.get_name(top_level),
            Err(err) => {
                self.errs.push(err);
                None
            }
        }
    }
}
