use std::{iter::Peekable, str::Chars};

use crate::{
    span::{Span, Spanned},
    syntax::token::{Comment, Token, TokenInfo, TokenKind},
};

fn is_atom(c: &char) -> bool {
    c.is_ascii_alphanumeric() || "!$%&*+-./:<=>?^_".contains(*c)
}

pub struct Lexer<'input> {
    peekable: Peekable<Chars<'input>>,
    input: &'input str,
    start: usize,
    index: usize,
}

pub enum Either<A, B> {
    Left(A),
    Right(B),
}

type Lexeme = String;

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            peekable: input.chars().peekable(),
            input,
            start: 0,
            index: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let char = self.peekable.next()?;
        self.index += char.len_utf8();

        Some(char)
    }

    fn advance_while(&mut self, pred: fn(&char) -> bool) {
        while let Some(char) = self.peekable.peek() {
            if pred(char) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn save(&mut self) {
        self.start = self.index;
    }

    fn spanned<T>(&self, data: T) -> Spanned<T> {
        Spanned::new(data, Span::new(self.start, self.index))
    }

    fn whitespace(&mut self) -> Spanned<String> {
        self.save();

        self.advance_while(|c| c.is_ascii_whitespace());
        let whitespace = self.input[self.start..self.index].to_owned();
        self.spanned(whitespace)
    }

    fn comment(&mut self) -> Either<Comment, Spanned<String>> {
        let whitespace = self.whitespace();

        self.save();

        if let Some(';') = self.peekable.peek() {
            self.advance_while(|c| *c != '\n');
            let comment = self.input[self.start..self.index].to_owned();
            let comment = self.spanned(comment);
            Either::Left(Comment {
                whitespace,
                comment,
            })
        } else {
            Either::Right(whitespace)
        }
    }

    fn comments(&mut self) -> (Vec<Comment>, Spanned<String>) {
        let mut comments = vec![];

        loop {
            match self.comment() {
                Either::Left(comment) => comments.push(comment),
                Either::Right(whitespace) => break (comments, whitespace),
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        let (comments, whitespace) = self.comments();
        self.save();

        let (kind, lexeme) = self.token();
        let lexeme = self.spanned(lexeme);
        Token {
            kind,
            info: TokenInfo {
                comments,
                whitespace,
                lexeme,
            },
        }
    }

    fn token(&mut self) -> (TokenKind, Lexeme) {
        let kind = if let Some(char) = self.advance() {
            match char {
                '(' => TokenKind::LParens,
                ')' => TokenKind::RParens,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                '\'' => TokenKind::SimpleQuote,
                '"' => return self.string(),
                c if c.is_ascii_alphabetic() => {
                    self.advance_while(is_atom);
                    TokenKind::Identifier
                }
                c if c.is_ascii_digit() => {
                    self.advance_while(|c| c.is_ascii_digit());
                    TokenKind::Number
                }
                _ => todo!(),
            }
        } else {
            TokenKind::Eof
        };

        let lexeme = self.input[self.start..self.index].to_owned();
        (kind, lexeme)
    }

    fn string(&mut self) -> (TokenKind, Lexeme) {
        let mut s = String::new();
        while let Some(char) = self.peekable.peek() {
            match char {
                '"' => break,
                _ => s.push(self.advance().unwrap()),
            }
        }
        if let Some('"') = self.peekable.peek() {
            self.advance();
            (TokenKind::String, s)
        } else {
            (TokenKind::Error, s)
        }
    }
}

#[cfg(test)]
mod test {
    use super::Lexer;

    #[test]
    fn read_token_with_comment() {
        let input = r#"

; here
; comment

fly

"#;
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        println!("{token:?}");
    }
}
