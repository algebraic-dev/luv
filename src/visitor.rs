//! Visitor trait.

use crate::r#abstract::*;

/// Trait for visiting different syntax nodes.
pub trait Visitor<'a>
where
    Self: Sized,
{
    fn visit_let(&mut self, let_expr: Let<'a>) -> Option<()>;

    fn visit_fn(&mut self, func: Fn<'a>) -> Option<()>;

    fn visit_def(&mut self, def: Def<'a>) -> Option<()>;

    fn visit_defn(&mut self, defn: Defn<'a>) -> Option<()>;

    fn visit_eval(&mut self, eval: Eval<'a>) -> Option<()>;

    fn visit_set_option(&mut self, set_option: SetOption<'a>) -> Option<()>;

    fn visit_require(&mut self, require: Require<'a>) -> Option<()>;

    fn visit_if(&mut self, if_expr: If<'a>) -> Option<()>;

    fn visit_block(&mut self, block: Block<'a>) -> Option<()>;

    fn visit_app(&mut self, app: App<'a>) -> Option<()>;

    fn visit_quote(&mut self, quote: Quote<'a>) -> Option<()>;

    fn visit_identifier(&mut self, id: Identifier<'a>) -> Option<()>;

    fn visit_number(&mut self, num: Number<'a>) -> Option<()>;

    fn visit_str(&mut self, s: Str<'a>) -> Option<()>;

    fn visit_params(&mut self, s: Params<'a>) -> Option<()>;
}

/// Default walk implementations for each variant.
impl<'a> Let<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        if let Ok(value) = self.name() {
            visitor.visit_identifier(value);
        }

        if let Ok(value) = self.value() {
            value.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> Fn<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        if let Ok(mut params) = self.params() {
            if let Ok(Some(_name)) = params.name() {
                // Could walk parameters if needed
            }
        }
        if let Ok(Some(body)) = self.body() {
            body.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> Def<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        if let Ok(params) = self.name() {
            visitor.visit_identifier(params);
        }

        if let Ok(value) = self.value() {
            value.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> Defn<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        if let Ok(params) = self.name() {
            visitor.visit_identifier(params);
        }

        if let Ok(params) = self.parameters() {
            visitor.visit_params(params);
        }

        while let Ok(Some(body)) = self.body() {
            body.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> Params<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        while let Ok(Some(name)) = self.name() {
            visitor.visit_identifier(name)?;
        }
        Some(())
    }
}

impl<'a> Eval<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        while let Ok(Some(expr)) = self.body() {
            expr.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> SetOption<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        if let Ok(expr) = self.expr() {
            expr.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> Require<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        if let Ok(mut name) = self.name() {
            name.walk(visitor)?;
        }

        Some(())
    }
}

impl<'a> If<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        if let Ok(cond) = self.cond() {
            cond.visit(visitor)?;
        }
        if let Ok(then_expr) = self.then() {
            then_expr.visit(visitor)?;
        }
        if let Ok(else_expr) = self.else_() {
            else_expr.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> Block<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        while let Ok(Some(body)) = self.body() {
            body.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> App<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        while let Ok(Some(argument)) = self.argument() {
            argument.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> Quote<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        let _ = visitor;
        Some(())
    }
}

impl<'a> Identifier<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        let _ = visitor;
        Some(())
    }
}

impl<'a> Number<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        let _ = visitor;
        Some(())
    }
}

impl<'a> Str<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        let _ = visitor;
        Some(())
    }
}

/// Redirects to the appropriate visit method based on the node type.
impl<'a> TopLevel<'a> {
    pub fn visit<V: Visitor<'a>>(self, visitor: &mut V) -> Option<()> {
        match self {
            TopLevel::Def(def) => visitor.visit_def(def.clone()),
            TopLevel::Defn(defn) => visitor.visit_defn(defn.clone()),
            TopLevel::Eval(eval) => visitor.visit_eval(eval.clone()),
            TopLevel::SetOption(set_option) => visitor.visit_set_option(set_option.clone()),
            TopLevel::Require(require) => visitor.visit_require(require.clone()),
        }
    }
}

/// Redirects to the appropriate visit method based on the node type.
impl<'a> Expr<'a> {
    pub fn visit<V: Visitor<'a>>(self, visitor: &mut V) -> Option<()> {
        match self {
            Expr::If(if_expr) => visitor.visit_if(if_expr.clone()),
            Expr::Fn(func) => visitor.visit_fn(func.clone()),
            Expr::App(app) => visitor.visit_app(app.clone()),
            Expr::Block(block) => visitor.visit_block(block.clone()),
            Expr::Quote(quote) => visitor.visit_quote(quote.clone()),
            Expr::Identifier(id) => visitor.visit_identifier(id.clone()),
            Expr::Number(num) => visitor.visit_number(num.clone()),
            Expr::Str(s) => visitor.visit_str(s.clone()),
            Expr::Let(s) => visitor.visit_let(s.clone()),
        }
    }
}
