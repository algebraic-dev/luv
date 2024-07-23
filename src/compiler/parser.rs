//! A module for parsing tokens into an concrete syntax tree.

use crate::{
    compiler::lexer::Lexer,
    span::{Span, Spanned},
    compiler::syntax::{
        concrete,
        token::{Token, TokenKind},
    },
};

/// A parser for converting tokens into an CST.
pub struct Parser<'input> {
    lexer: Lexer<'input>,
    curr: Token,
    next: Token,
    last_pos: Span,
}

impl<'input> Parser<'input> {
    /// Creates a new `Parser` for the given lexer.
    pub fn new(mut lexer: Lexer<'input>) -> Self {
        let curr = lexer.next_token();
        let next = lexer.next_token();
        Self {
            lexer,
            curr,
            next,
            last_pos: Span::ghost(),
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

pub type Result<T> = std::result::Result<T, (String, Span)>;

impl<'input> Parser<'input> {
    /// Advances the parser to the next token.
    fn eat(&mut self) -> Token {
        let new_curr = std::mem::replace(&mut self.next, self.lexer.next_token());
        std::mem::replace(&mut self.curr, new_curr)
    }

    /// Returns the kind of the current token.
    fn peek(&self) -> TokenKind {
        self.curr.kind
    }

    /// Expects the current token to be of a specific kind and advances if it matches.
    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        if self.curr.kind == kind {
            Ok(self.eat())
        } else {
            Err((format!("Expected {:?}, found {:?}", kind, self.curr.kind), self.curr.info.lexeme.span.clone()))
        }
    }

    /// Checks if the current token is of a specific kind.
    fn is(&self, kind: TokenKind) -> bool {
        self.curr.kind == kind
    }

    /// Returns the span of the current token.
    fn span(&self) -> Span {
        self.curr.info.lexeme.span.clone()
    }

    /// Creates a spanned object from the result of a parsing function.
    fn spanned<T>(&mut self, f: impl FnOnce(&mut Self) -> Result<T>) -> Result<Spanned<T>> {
        let start = self.span();
        let value = f(self)?;
        let end = self.last_pos.clone();

        Ok(Spanned::new(value, start.mix(end)))
    }

    /// Parses an expression.
    pub fn expr(&mut self) -> Result<concrete::Expr> {
        match self.peek() {
            TokenKind::Identifier => self.spanned(|s| s.atom().map(concrete::ExprNode::Atom)),
            TokenKind::Number => self.spanned(|s| s.number().map(concrete::ExprNode::Number)),
            TokenKind::String => self.spanned(|s| s.string().map(concrete::ExprNode::String)),
            TokenKind::LParens => self.spanned(|s| s.list().map(concrete::ExprNode::List)),
            TokenKind::RParens => Err(("Unexpected right parenthesis".into(), self.next.info.lexeme.span.clone())),
            TokenKind::LBracket => self.spanned(|s| s.vec().map(concrete::ExprNode::Vec)),
            TokenKind::RBracket => Err(("Unexpected right bracket".into(), self.next.info.lexeme.span.clone())),
            TokenKind::SimpleQuote => Err(("Unexpected simple quote".into(), self.next.info.lexeme.span.clone())),
            TokenKind::Error => Err(("Unexpected error token".into(), self.next.info.lexeme.span.clone())),
            TokenKind::Eof => Err(("Unexpected end of file".into(), self.next.info.lexeme.span.clone())),
        }
    }

    /// Parses an atom node.
    fn atom(&mut self) -> Result<concrete::AtomNode> {
        let token = self.expect(TokenKind::Identifier)?;
        Ok(concrete::AtomNode { token })
    }

    /// Parses a number node.
    fn number(&mut self) -> Result<concrete::NumberNode> {
        let token = self.expect(TokenKind::Number)?;
        Ok(concrete::NumberNode { token })
    }

    /// Parses a string node.
    fn string(&mut self) -> Result<concrete::StringNode> {
        let token = self.expect(TokenKind::String)?;
        Ok(concrete::StringNode { token })
    }

    /// Parses a list node.
    fn list(&mut self) -> Result<concrete::ListNode> {
        let lparens = self.expect(TokenKind::LParens)?;
        let mut items = vec![];
        
        while !self.is(TokenKind::RBracket) && !self.is(TokenKind::RParens) && !self.is(TokenKind::Eof) {
            items.push(self.expr()?);
        }

        let Ok(rparens) = self.expect(TokenKind::RParens) else {
            return Err(("Unclosed parenthesis".to_string(), lparens.info.lexeme.span.clone()))
        };
        
        Ok(concrete::ListNode {
            lparens,
            items,
            rparens,
        })
    }

    /// Parses a vector node.
    fn vec(&mut self) -> Result<concrete::VecNode> {
        let lbracket = self.expect(TokenKind::LBracket)?;
        let mut items = vec![];

        while !self.is(TokenKind::RBracket) && !self.is(TokenKind::RParens) && !self.is(TokenKind::Eof) {
            items.push(self.expr()?);
        }
        
        let Ok(rbracket) = self.expect(TokenKind::RBracket) else {
            return Err(("Unclosed bracket".to_string(), lbracket.info.lexeme.span.clone()))
        };
        
        Ok(concrete::VecNode {
            lbracket,
            items,
            rbracket,
        })
    }

    pub fn parse(&mut self) -> Result<Vec<concrete::ListNode>> {
        let mut code = Vec::new();

        while self.curr.kind != TokenKind::Eof {
            code.push(self.list()?)
        }

        Ok(code)
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;

    #[test]
    fn parse_list() {
        let mut parser = Parser::from("(test [369])");
        let expr = parser.expr();
        println!("{:?}", expr);
    }
}