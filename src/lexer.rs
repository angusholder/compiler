use chars::PeekableCharIndices;
use result::{Span, CompileResult};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    KIf,
    KElse,
    KLet,
    KTrue,
    KFalse,
    KPrint,
    KWhile,

    LBrace,
    RBrace,
    LParen,
    RParen,
    LBracket,
    RBracket,

    Add,
    Sub,
    Multiply,
    Remainder,
    Divide,
    Dot,

    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    NotEq,

    Assign,

    Semicolon,
    Comma,

    Ident(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    iter: PeekableCharIndices<'a>,
    src: &'a str,
    peeked: Option<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            iter: PeekableCharIndices::new(src),
            src,
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> CompileResult<Option<&Token>> {
        if self.peeked.is_none() {
            self.peeked = self.next()?;
        }
        Ok(self.peeked.as_ref())
    }

    pub fn unget(&mut self, token: Token) {
        assert!(self.peeked.is_none());
        self.peeked = Some(token);
    }

    pub fn next(&mut self) -> CompileResult<Option<Token>> {
        if let Some(tok) = self.peeked.take() {
            return Ok(Some(tok));
        }

        while let Some((idx, c)) = self.iter.peek() {
            if c.is_whitespace() {
                self.iter.next();
            } else if idx + 1 < self.src.len() && &self.src[idx..idx+2] == "//" {
                self.iter.next();
                self.iter.next();

                while let Some((_, c)) = self.iter.next() {
                    if c == '\n' {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        let (index, ch) = match self.iter.next() {
            Some(next) => next,
            None => return Ok(None),
        };

        let mut span = Span::new(index, self.iter.offset());

        use self::TokenKind::*;
        let kind = match ch {
            '[' => LBracket,
            ']' => RBracket,
            '{' => LBrace,
            '}' => RBrace,
            '(' => LParen,
            ')' => RParen,

            '+' => Add,
            '-' => Sub,
            '*' => Multiply,
            '%' => Remainder,
            '/' => Divide,

            ';' => Semicolon,
            ',' => Comma,

            '=' => {
                if self.iter.matches('=') {
                    Eq
                } else {
                    Assign
                }
            }
            '<' => {
                if self.iter.matches('=') {
                    LtEq
                } else {
                    Lt
                }
            }
            '>' => {
                if self.iter.matches('=') {
                    GtEq
                } else {
                    Gt
                }
            }

            'a'...'z' | 'A'...'Z' | '_' => {
                while let Some((pos, ch)) = self.iter.peek() {
                    if ch.is_alphanumeric() || ch == '_' {
                        self.iter.next();
                    } else {
                        break;
                    }
                }
                span.end = self.iter.offset() as u32;

                match span.as_str(self.src) {
                    "if" => KIf,
                    "else" => KElse,
                    "let" => KLet,
                    "true" => KTrue,
                    "false" => KFalse,
                    "print" => KPrint,
                    "while" => KWhile,
                    ident => Ident(ident.to_string()),
                }
            }

            ch => return err!(span, "Unknown character {:?}", ch),
        };

        Ok(Some(Token { kind, span }))
    }

    pub fn expect(&mut self, expected: TokenKind) -> CompileResult<()> {
        // If this fails it is a hard parse error, which is why we don't care about
        // consuming the next token with .next()
        match self.next()? {
            Some(token) => {
                if token.kind == expected {
                    Ok(())
                } else {
                    err!(token.span, "got {:?}, expected {:?}", token.kind, expected)
                }
            }
            None => {
                let end = self.iter.offset();
                let span = Span::new(end, end);
                err!(span, "got end of file, expected {:?}", expected)
            }
        }
    }

    pub fn expect_ident(&mut self) -> CompileResult<String> {
        match self.next()? {
            Some(Token { kind: TokenKind::Ident(ident), .. }) => Ok(ident),
            Some(token) => {
                err!(token.span, "got {:?}, expected an identifier", token.kind)
            }
            None => {
                let end = self.iter.offset();
                let span = Span::new(end, end);
                err!(span, "got end of file, expected an identifier")
            }
        }
    }

    pub fn matches(&mut self, expected: TokenKind) -> bool {
        if let Ok(Some(token)) = self.next() {
            if token.kind == expected {
                true
            } else {
                // Stupid borrow check issues forced me to do this dumb solution.
                self.peeked = Some(token);
                false
            }
        } else {
            false
        }
    }
}

#[cfg(test)]
mod tests {
    use lexer::{ Lexer, TokenKind, Token };
    use result::CompileResult;

    fn expect_kind(l: &mut Lexer, expected: TokenKind) {
        if let Ok(Some(Token { kind, span })) = l.next() {
            if kind != expected {
                panic!("Expected {:?} == {:?} (span: {:?})", kind, expected, span);
            }
            assert_eq!(kind, expected);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_whitespace() {
        let mut l = Lexer::new(" \n\t\r");
        assert_eq!(l.peek(), Ok(None));
        assert_eq!(l.next(), Ok(None));
    }

    #[test]
    fn test_identifiers() {
        let ref mut l = Lexer::new("if else let true false print while foo");
        expect_kind(l, TokenKind::KIf);
        expect_kind(l, TokenKind::KElse);
        expect_kind(l, TokenKind::KLet);
        expect_kind(l, TokenKind::KTrue);
        expect_kind(l, TokenKind::KFalse);
        expect_kind(l, TokenKind::KPrint);
        expect_kind(l, TokenKind::KWhile);
        expect_kind(l, TokenKind::Ident("foo".into()));
        assert_eq!(l.next(), Ok(None));
    }

    #[test]
    fn test_operators() {
        let ref mut l = Lexer::new("*//\n+[]hello");
        expect_kind(l, TokenKind::Multiply);
        expect_kind(l, TokenKind::Add);
        expect_kind(l, TokenKind::LBracket);
        expect_kind(l, TokenKind::RBracket);
        expect_kind(l, TokenKind::Ident("hello".into()));
        assert_eq!(l.next(), Ok(None));
    }
}