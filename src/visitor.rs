//! Visitor trait.

use crate::r#abstract::*;

/// Trait for visiting different syntax nodes.
pub trait Visitor<'a>
where
    Self: Sized,
{
    fn visit_let(&mut self, mut let_stmt: Let<'a>) -> Option<()> {
        let_stmt.walk(self)
    }

    fn visit_fn(&mut self, mut func: Fn<'a>) -> Option<()> {
        func.walk(self)
    }

    fn visit_def(&mut self, mut def: Def<'a>) -> Option<()> {
        def.walk(self)
    }

    fn visit_defn(&mut self, mut defn: Defn<'a>) -> Option<()> {
        defn.walk(self)
    }

    fn visit_eval(&mut self, mut eval: Eval<'a>) -> Option<()> {
        eval.walk(self)
    }

    fn visit_set_option(&mut self, mut set_option: SetOption<'a>) -> Option<()> {
        set_option.walk(self)
    }

    fn visit_require(&mut self, mut require: Require<'a>) -> Option<()> {
        require.walk(self)
    }

    fn visit_if(&mut self, mut if_expr: If<'a>) -> Option<()> {
        if_expr.walk(self)
    }

    fn visit_block(&mut self, mut block: Block<'a>) -> Option<()> {
        block.walk(self)
    }

    fn visit_app(&mut self, mut app: App<'a>) -> Option<()> {
        app.walk(self)
    }

    fn visit_quote(&mut self, mut quote: Quote<'a>) -> Option<()> {
        quote.walk(self)
    }

    fn visit_identifier(&mut self, mut id: Identifier<'a>) -> Option<()> {
        id.walk(self)
    }

    fn visit_number(&mut self, mut num: Number<'a>) -> Option<()> {
        num.walk(self)
    }

    fn visit_str(&mut self, mut s: Str<'a>) -> Option<()> {
        s.walk(self)
    }

    fn visit_params(&mut self, mut s: Params<'a>) -> Option<()> {
        s.walk(self)
    }
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
        if let Ok(stmt) = self.stmt() {
            stmt.visit(visitor)?;
        }

        Some(())
    }
}

impl<'a> SetOption<'a> {
    pub fn walk<V: Visitor<'a>>(&mut self, visitor: &mut V) -> Option<()> {
        if let Ok(stmt) = self.stmt() {
            stmt.visit(visitor)?;
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
        }
    }
}

/// Redirects to the appropriate visit method based on the node type.
impl<'a> Stmt<'a> {
    pub fn visit<V: Visitor<'a>>(self, visitor: &mut V) -> Option<()> {
        match self {
            Stmt::Let(let_stmt) => visitor.visit_let(let_stmt.clone()),
            Stmt::Expr(expr) => expr.visit(visitor),
        }
    }
}
