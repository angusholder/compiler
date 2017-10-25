use result::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // Keywords
    KIf,
    KElse,
    KLet,
    KTrue,
    KFalse,
    KPrint,
    KWhile,

    LBrace, // {
    RBrace, // }
    LParen, // (
    RParen, // )
    LBracket, // [
    RBracket, // ]

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
    Colon,
    Comma,

    Ident(String),
    Int(i32),
    Float(f32),
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}