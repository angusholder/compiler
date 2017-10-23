use std::fmt;

use entity::{ PrimaryMap, EntityMap };
use result::Span;

pub struct Ast<'a> {
    pub root_block: StmtRef,
    pub expressions: PrimaryMap<ExprRef, Expr>,
    pub expr_locs: EntityMap<ExprRef, Span>,
    pub statements: PrimaryMap<StmtRef, Stmt>,
    pub stmt_locs: EntityMap<StmtRef, Span>,
    pub src: &'a str,
}



#[derive(Debug)]
pub enum Expr {
    BinOp {
        kind: BinOpKind,
        left: ExprRef,
        right: ExprRef,
    },
    UnaryOp {
        kind: UnaryOpKind,
        child: ExprRef,
    },
    Field {
        left: ExprRef,
        ident: String,
    },
    FuncCall {
        left: ExprRef,
        args: Box<[ExprRef]>,
    },
    Assignment {
        left: ExprRef,
        right: ExprRef,
    },
    Lit(Lit),
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Ident(String),
    Bool(bool),
    Int(i32),
    Float(f32),
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Lit::Bool(true) => f.write_str("true"),
            Lit::Bool(false) => f.write_str("false"),
            Lit::Ident(ref ident) => f.write_str(ident),
            Lit::Int(n) => write!(f, "{}", n),
            Lit::Float(n) => write!(f, "{}", n),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    NotEq,

    Index, // a[b]
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BinOpKind::*;
        let s = match *self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Rem => "%",

            Lt => "<",
            LtEq => "<=",
            Gt => ">",
            GtEq => ">=",
            Eq => "==",
            NotEq => "!=",

            Index => "[]",
        };
        f.write_str(s)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOpKind {
    Neg,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            UnaryOpKind::Neg => "-",
        };
        f.write_str(s)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct ExprRef(u32);
impl_entity!(ExprRef);



pub enum Stmt {
    Let {
        name: String,
        ty: Option<String>,
        expr: Option<ExprRef>,
    },
    If {
        cond: ExprRef,
        then: StmtRef,
        els: Option<StmtRef>,
    },
    While {
        cond: ExprRef,
        block: StmtRef,
    },
    Expr(ExprRef),
    Block(Block),
}

pub struct Block {
    pub stmts: Vec<StmtRef>,
}

#[derive(Clone, Copy, Debug)]
pub struct StmtRef(u32);
impl_entity!(StmtRef);