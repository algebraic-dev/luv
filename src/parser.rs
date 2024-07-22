use crate::{
    lexer::Lexer,
    span::{Span, Spanned},
    syntax::{
        concrete,
        token::{Token, TokenKind},
    },
};

pub struct Parser<'input> {
    lexer: Lexer<'input>,
    curr: Token,
    next: Token,
    last_pos: Span,
}

impl<'input> Parser<'input> {
    pub fn new(mut lexer: Lexer<'input>) -> Self {
        Self {
            curr: lexer.next_token(),
            next: lexer.next_token(),
            last_pos: Span::ghost(),
            lexer,
        }
    }
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(value: &'a str) -> Self {
        let mut lexer = Lexer::new(value);
        Self {
            curr: lexer.next_token(),
            next: lexer.next_token(),
            last_pos: Span::ghost(),
            lexer,
        }
    }
}

pub type Result<T> = std::result::Result<T, String>;

impl<'input> Parser<'input> {
    fn eat(&mut self) -> Token {
        let new_curr = std::mem::replace(&mut self.next, self.lexer.next_token());
        std::mem::replace(&mut self.curr, new_curr)
    }

    fn peek(&self) -> TokenKind {
        self.curr.kind
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        if self.curr.kind == kind {
            Ok(self.eat())
        } else {
            todo!("Error message")
        }
    }

    fn is(&self, kind: TokenKind) -> bool {
        self.curr.kind == kind
    }

    fn span(&self) -> Span {
        self.curr.info.lexeme.span.clone()
    }

    fn spanned<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<Spanned<T>> {
        let start = self.span();
        let value = f(self)?;
        let end = self.last_pos.clone();

        Ok(Spanned::new(value, start.mix(end)))
    }

    pub fn expr(&mut self) -> Result<concrete::Expr> {
        match self.peek() {
            TokenKind::Identifier => self.spanned(|s| s.atom().map(concrete::ExprNode::Atom)),
            TokenKind::Number => self.spanned(|s| s.number().map(concrete::ExprNode::Number)),
            TokenKind::String => self.spanned(|s| s.string().map(concrete::ExprNode::String)),
            TokenKind::LParens => self.spanned(|s| s.list().map(concrete::ExprNode::List)),
            TokenKind::RParens => todo!("Unexpected"),
            TokenKind::LBracket => self.spanned(|s| s.vec().map(concrete::ExprNode::Vec)),
            TokenKind::RBracket => todo!("Unexpected"),
            TokenKind::SimpleQuote => todo!(),
            TokenKind::Error => todo!("Error"),
            TokenKind::Eof => todo!("Eof"),
        }
    }

    fn atom(&mut self) -> Result<concrete::AtomNode> {
        let token = self.expect(TokenKind::Identifier)?;
        Ok(concrete::AtomNode { token })
    }

    fn number(&mut self) -> Result<concrete::NumberNode> {
        let token = self.expect(TokenKind::Number)?;
        Ok(concrete::NumberNode { token })
    }

    fn string(&mut self) -> Result<concrete::StringNode> {
        let token = self.expect(TokenKind::String)?;
        Ok(concrete::StringNode { token })
    }

    fn list(&mut self) -> Result<concrete::ListNode> {
        let lparens = self.expect(TokenKind::LParens)?;
        let mut items = vec![];
        while !self.is(TokenKind::RParens) {
            items.push(self.expr()?);
        }
        let rparens = self.expect(TokenKind::RParens)?;
        Ok(concrete::ListNode {
            lparens,
            items,
            rparens,
        })
    }

    fn vec(&mut self) -> Result<concrete::VecNode> {
        let lbracket = self.expect(TokenKind::LBracket)?;
        let mut items = vec![];
        while !self.is(TokenKind::RBracket) {
            items.push(self.expr()?);
        }
        let rbracket = self.expect(TokenKind::RBracket)?;
        Ok(concrete::VecNode {
            lbracket,
            items,
            rbracket,
        })
    }
}

#[cfg(test)]
mod test {
    use super::Parser;

    #[test]
    fn parse_list() {
        let mut parser = Parser::from("(  test [ 369] )");
        let expr = parser.expr();
        println!("{expr:?}");
    }
}
