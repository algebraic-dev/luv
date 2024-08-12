//! This module is used to analyze an AST (Abstract Syntax Tree) into a collection of data.  It
//! provides structures and implementations to manage scopes and context while visiting  the nodes
//! of the AST, allowing for the organization of variable information and spans.


use crate::{
    r#abstract::{NodeList, Params, TopLevel}, errors::Error, hierarchy::HierarchyBuilder, scope::Scope, span::{Span, Spanned}, syntax::SyntaxNode, visitor::Visitor
};

macro_rules! visit_opt {
    ($self:ident, $func:expr, $params:ident, $action:expr) => {
        if let Some($params) = $self.register_error($func.$params()) {
            $action;
        }
    };
}

macro_rules! while_some {
    ($self:ident, $block:expr, $body:ident, $action:expr) => {
        while let Some($body) = $self.register_error($block.$body()).flatten() {
            $action;
        }
    };
}

/// Represents the context during AST analysis.
#[derive(Default)]
pub struct Analysis {
    /// A builder for managing the hierarchical structure of scopes.
    pub scopes: HierarchyBuilder<Scope>,

    /// Accumulator of errors.
    pub errors: Vec<Error>,

    /// Unbounded names.
    pub unbound: Vec<Spanned<String>>,
}

impl Analysis {
    /// Creates a new context
    pub fn new(span: Span) -> Self {
        Self {
            scopes: HierarchyBuilder::new(span),
            errors: Vec::default(),
            unbound: Vec::new(),
        }
    }

    pub fn register_error<T>(&mut self, data: Result<T, Error>) -> Option<T> {
        match data {
            Ok(res) => Some(res),
            Err(err) => {
                self.errors.push(err);
                None
            }
        }
    }

    pub fn drain(&mut self, mut data: NodeList) -> Option<()> {
        while let Some(syn) = data.bump() {
            self.errors
                .push(Error::new("useless argument, remove it.", syn.span.clone()))
        }

        Some(())
    }
}

impl<'a> Visitor<'a> for Analysis {
    fn visit_params(&mut self, mut s: Params<'a>) -> Option<()> {
        while let Ok(Some(name)) = s.name() {
            let text = name.text().unwrap();
            let scope = self.scopes.get();
            scope.add(text, name.span());
        }

        Some(())
    }

    fn visit_defn(&mut self, mut defn: crate::r#abstract::Defn<'a>) -> Option<()> {
        self.register_error(defn.name());
        self.scopes.open(defn.span());
        visit_opt!(self, defn, parameters, self.visit_params(parameters));
        while_some!(self, defn, body, body.visit(self));
        self.scopes.close();
        self.drain(defn.0)
    }

    fn visit_let(&mut self, mut let_stmt: crate::r#abstract::Let<'a>) -> Option<()> {
        visit_opt!(self, let_stmt, name, {
            let text = name.text().ok()?;
            let scope = self.scopes.get();
            scope.add(text, name.span())
        });
        visit_opt!(self, let_stmt, value, value.visit(self));
        self.drain(let_stmt.0)
    }

    fn visit_block(&mut self, mut block: crate::r#abstract::Block<'a>) -> Option<()> {
        self.scopes.open(block.span());
        while_some!(self, block, body, body.visit(self));
        self.scopes.close();
        self.drain(block.0)
    }

    fn visit_if(&mut self, mut if_expr: crate::r#abstract::If<'a>) -> Option<()> {
        visit_opt!(self, if_expr, cond, cond.visit(self));
        visit_opt!(self, if_expr, then, {
            self.scopes.open(then.span());
            then.visit(self);
            self.scopes.close();
        });
        visit_opt!(self, if_expr, else_, else_.visit(self));
        self.drain(if_expr.0)
    }

    fn visit_fn(&mut self, mut func: crate::r#abstract::Fn<'a>) -> Option<()> {
        self.scopes.open(func.span());
        visit_opt!(self, func, params, self.visit_params(params));
        while_some!(self, func, body, body.visit(self));
        self.scopes.close();
        self.drain(func.0)
    }

    fn visit_def(&mut self, mut def: crate::r#abstract::Def<'a>) -> Option<()> {
        self.register_error(def.name());
        self.scopes.open(def.span());
        visit_opt!(self, def, value, value.visit(self));
        self.scopes.close();
        self.drain(def.0)
    }

    fn visit_eval(&mut self, mut eval: crate::r#abstract::Eval<'a>) -> Option<()> {
        self.scopes.open(eval.span());
        while_some!(self, eval, body, body.visit(self));
        self.scopes.close();
        self.drain(eval.0)
    }

    fn visit_app(&mut self, mut app: crate::r#abstract::App<'a>) -> Option<()> {
        //visit_opt!(self, app, name, { Some(()) });
        while_some!(self, app, argument, argument.visit(self));
        Some(())
    }

    fn visit_set_option(&mut self, _set_option: crate::r#abstract::SetOption<'a>) -> Option<()> {
        Some(())
    }

    fn visit_require(&mut self, _require: crate::r#abstract::Require<'a>) -> Option<()> {
        Some(())
    }

    fn visit_quote(&mut self, _quote: crate::r#abstract::Quote<'a>) -> Option<()> {
        Some(())
    }

    fn visit_identifier(&mut self, id: crate::r#abstract::Identifier<'a>) -> Option<()> {
        if let Some(text) = self.register_error(id.text()) {
            if self.scopes.last().find(text, &id.span()).is_none() {
                self.unbound.push(Spanned::new(text.to_owned(), id.span()))
            }
        }

        Some(())
    }

    fn visit_number(&mut self, _num: crate::r#abstract::Number<'a>) -> Option<()> {
        Some(())
    }

    fn visit_str(&mut self, _s: crate::r#abstract::Str<'a>) -> Option<()> {
        Some(())
    }
}

pub fn analyze<'a>(syn: &'a SyntaxNode) -> Analysis {
    let mut analysis = Analysis::default();

    if let Some(top) = analysis.register_error(TopLevel::from_node(syn)) {
        top.visit(&mut analysis);
    }

    analysis
}