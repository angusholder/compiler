use std::fmt;

use entity::{ PrimaryMap, EntityMap };
use lexer::{ Lexer, Token, TokenKind };
use result::{ Span, CompileResult };

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    expressions: PrimaryMap<ExprRef, Expr>,
    expr_locs: EntityMap<ExprRef, Span>,
    statements: PrimaryMap<StmtRef, Stmt>,
    stmt_locs: EntityMap<StmtRef, Span>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &str) -> Parser {
        Parser {
            lexer: Lexer::new(src),
            expressions: PrimaryMap::new(),
            expr_locs: EntityMap::new(),
            statements: PrimaryMap::new(),
            stmt_locs: EntityMap::new(),
        }
    }

    fn make_expr(&mut self, expr: Expr, span: Span) -> ExprRef {
        let expr_ref = self.expressions.push(expr);
        self.expr_locs[expr_ref] = span;
        expr_ref
    }

    fn get_expr(&self, expr_ent: ExprRef) -> &Expr {
        &self.expressions[expr_ent]
    }

    fn make_stmt(&mut self, stmt: Stmt, span: Span) -> StmtRef {
        let stmt_ref = self.statements.push(stmt);
        self.stmt_locs[stmt_ref] = span;
        stmt_ref
    }

    fn get_stmt(&self, stmt_ref: StmtRef) -> &Stmt {
        &self.statements[stmt_ref]
    }
}



//
//
// Expression parsing
//
//

// Binding power of operators
const BP_PARENS: i32   = 0; // ( <some expression> )
const BP_COMMA: i32    = 1;
const BP_OR: i32       = 17;
const BP_AND: i32      = 19;
const BP_COMPARE: i32  = 21; // ==, !=, <=, <, >=, >
const BP_SUM: i32      = 23; // +, -
const BP_PRODUCT: i32  = 25; // *, /, %
const BP_UNARY: i32    = 29; // -
const BP_ACCESSOR: i32 = 31; // a(), a[b], a.b

type NullDenotation = fn(p: &mut Parser, token: Token, bp: i32) -> CompileResult<ExprRef>;
type LeftDenotation = fn(p: &mut Parser, token: Token, left: ExprRef, rbp: i32) -> CompileResult<ExprRef>;

fn null_constant(p: &mut Parser, token: Token, _bp: i32) -> CompileResult<ExprRef> {
    let atom = match token.kind {
        TokenKind::KTrue => Atom::Bool(true),
        TokenKind::KFalse => Atom::Bool(false),
        TokenKind::Ident(i) => Atom::Ident(i),
        _ => unreachable!(),
    };

    Ok(p.make_expr(Expr::Atom(atom), token.span))
}

fn null_paren(p: &mut Parser, start_token: Token, bp: i32) -> CompileResult<ExprRef> {
    let result = p.parse_expr_until(bp)?;
    let end_span = p.lexer.expect(TokenKind::RParen)?;
    p.expr_locs[result] = Span::between(start_token.span, end_span);
    Ok(result)
}

// Prefix operator.
//
// Low precedence:  return, raise, etc.
//   return x+y is return (x+y), not (return x) + y
//
// High precedence: logical negation, bitwise complement, etc.
//   !x && y is (!x) && y, not !(x && y)
fn null_prefix_op(p: &mut Parser, token: Token, bp: i32) -> CompileResult<ExprRef> {
    let child = p.parse_expr_until(bp)?;
    let kind = match token.kind {
        TokenKind::Sub => UnaryOpKind::Neg,
        _ => unreachable!(),
    };

    let span = Span::between(token.span, p.expr_locs[child]);
    Ok(p.make_expr(Expr::UnaryOp { kind, child }, span))
}

// Left Denotations -- token that takes an expression on the left
fn left_index(p: &mut Parser, _token: Token, left: ExprRef, _rbp: i32) -> CompileResult<ExprRef> {
    let right = p.parse_expr_until(0)?;
    let end_span = p.lexer.expect(TokenKind::RBracket)?;
    let span = Span::between(p.expr_locs[left], end_span);
    Ok(p.make_expr(Expr::BinOp { left, right, kind: BinOpKind::Index }, span))
}

fn left_attr(p: &mut Parser, _token: Token, left: ExprRef, _rbp: i32) -> CompileResult<ExprRef> {
    let (ident, end_span) = p.lexer.expect_ident()?;
    let span = Span::between(p.expr_locs[left], end_span);
    Ok(p.make_expr(Expr::Attr { left, ident }, span))
}

fn left_binary_op(p: &mut Parser, token: Token, left: ExprRef, rbp: i32) -> CompileResult<ExprRef> {
    let kind = match token.kind {
        TokenKind::Add => BinOpKind::Add,
        TokenKind::Sub => BinOpKind::Sub,
        TokenKind::Multiply => BinOpKind::Mul,
        TokenKind::Remainder => BinOpKind::Rem,
        TokenKind::Divide => BinOpKind::Div,

        TokenKind::Lt => BinOpKind::Lt,
        TokenKind::LtEq => BinOpKind::LtEq,
        TokenKind::Gt => BinOpKind::Gt,
        TokenKind::GtEq => BinOpKind::GtEq,
        TokenKind::Eq => BinOpKind::Eq,
        TokenKind::NotEq => BinOpKind::NotEq,
        _ => panic!("Unexpected token {:?}", token),
    };

    let right = p.parse_expr_until(rbp)?;

    let span = Span::between(p.expr_locs[left], p.expr_locs[right]);
    Ok(p.make_expr(Expr::BinOp { kind, left, right }, span))
}

fn left_func_call(p: &mut Parser, _token: Token, left: ExprRef, _rbp: i32) -> CompileResult<ExprRef> {
    let mut args = Vec::new();
    let span = if let Some(end_span) = p.lexer.matches(TokenKind::RParen) {
        Span::between(p.expr_locs[left], end_span)
    } else {
        loop {
            args.push(p.parse_expr_until(BP_COMMA)?);
            if !p.lexer.matches(TokenKind::Comma).is_some() {
                let end_span = p.lexer.expect(TokenKind::RParen)?;
                break Span::between(p.expr_locs[left], end_span);
            }
        }
    };

    Ok(p.make_expr(Expr::FuncCall {
        left,
        args: args.into_boxed_slice(),
    }, span))
}

impl<'a> Parser<'a> {
    fn parse_null(&mut self, token: Token) -> CompileResult<ExprRef> {
        use lexer::TokenKind::*;

        let (nud, bp): (NullDenotation, i32) = match token.kind {
            Ident(_) | KTrue | KFalse => {
                (null_constant, -1)
            }
            LParen => {
                (null_paren, BP_PARENS)
            }
            Sub => {
                (null_prefix_op, BP_UNARY)
            }
            _ => {
                return err!(token.span, "expected expression, found {:?}", token.kind);
            }
        };

        nud(self, token, bp)
    }

    fn parse_left(&mut self, min_rbp: i32, node: &mut ExprRef) -> CompileResult<bool> {
        use lexer::TokenKind::*;

        let (led, bp): (LeftDenotation, i32) = {
            let token = if let Some(token) = self.lexer.peek()? {
                token
            } else {
                return Ok(true); // No tokens left, terminate expression.
            };

            match token.kind {
                LParen => (left_func_call, BP_ACCESSOR),
                LBracket => (left_index, BP_ACCESSOR),
                Dot => (left_attr, BP_ACCESSOR),

                Multiply | Divide | Remainder => (left_binary_op, BP_PRODUCT),
                Add | Sub => (left_binary_op, BP_SUM),
                Eq | NotEq | Lt | LtEq | Gt | GtEq => (left_binary_op, BP_COMPARE),

                _ => {
                    // This token cannot form part of the expression, so terminate
                    // TODO: Are there any tokens that shouldn't terminate an expression error free?
                    return Ok(true);
                }
            }
        };

        let (lbp, rbp) = (bp, bp);

        if min_rbp >= lbp {
            // We've reached something that binds tighter than us, so terminate this iteration.
            return Ok(true);
        }

        // The use of peek above has already ensured there's a token, so unwrap here.
        let token = self.lexer.next()?.unwrap();

        *node = led(self, token, *node, rbp)?;

        Ok(false) // Not done
    }

    fn parse_expr_until(&mut self, min_rbp: i32) -> CompileResult<ExprRef> {
        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            let span = Span::new(self.lexer.offset(), self.lexer.offset());
            return err!(span, "expected expression, found end of file");
        };

        let mut node = self.parse_null(token)?;

        loop {
            let done = self.parse_left(min_rbp, &mut node)?;
            if done {
                return Ok(node);
            }
        }
    }

    pub fn parse_expr(&mut self) -> CompileResult<ExprRef> {
        self.parse_expr_until(0)
    }

    pub fn fmt_expr(&self, expr: ExprRef) -> ExprFormatter {
        ExprFormatter {
            root: expr,
            parser: self,
        }
    }
}

pub struct ExprFormatter<'a, 'src: 'a> {
    root: ExprRef,
    parser: &'a Parser<'src>,
}

impl<'a, 'src> fmt::Display for ExprFormatter<'a, 'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_expr(f, self.parser, self.root, 0)
    }
}

fn print_expr(f: &mut fmt::Formatter, p: &Parser, expr: ExprRef, indentation: i32) -> fmt::Result {
    indent(f, indentation)?;
    match *p.get_expr(expr) {
        Expr::BinOp { left, right, kind } => {
            writeln!(f, "{}", kind)?;
            print_expr(f, p, left, indentation + 1)?;
            print_expr(f, p, right, indentation + 1)?;
        }
        Expr::UnaryOp { child, kind } => {
            writeln!(f, "{}", kind)?;
            print_expr(f, p, child, indentation + 1)?;
        }
        Expr::Atom(ref atom) => {
            writeln!(f, "{}", atom)?;
        }
        Expr::Attr { left, ref ident } => {
            writeln!(f, "getattr {}", ident)?;
            print_expr(f, p, left, indentation + 1)?;
        }
        Expr::FuncCall { left, ref args } => {
            writeln!(f, "call")?;
            print_expr(f, p, left, indentation + 1)?;

            indent(f, indentation + 1)?;
            writeln!(f, "(")?;

            for &arg in args.iter() {
                print_expr(f, p, arg, indentation + 2)?;
            }

            indent(f, indentation + 1)?;
            writeln!(f, ")")?;
        }
    }

    Ok(())
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
    Attr {
        left: ExprRef,
        ident: String,
    },
    FuncCall {
        left: ExprRef,
        args: Box<[ExprRef]>,
    },
    Atom(Atom),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Ident(String),
    Bool(bool),
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match *self {
            Atom::Bool(true) => "true",
            Atom::Bool(false) => "false",
            Atom::Ident(ref ident) => ident,
        };
        f.write_str(s)
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



impl<'a> Parser<'a> {
    pub fn parse_stmt(&mut self) -> CompileResult<StmtRef> {
        match self.lexer.next()? {
            Some(Token { kind: TokenKind::KIf, span }) => {
                self.parse_if_stmt(span)
            }
            Some(Token { kind: TokenKind::KWhile, span }) => {
                let cond = self.parse_expr()?;
                self.lexer.expect(TokenKind::LBrace)?;
                let block = self.parse_block()?;
                let end_span = self.lexer.expect(TokenKind::RBrace)?;
                let span = Span::between(span, end_span);
                Ok(self.make_stmt(Stmt::While { cond, block }, span))
            }
            Some(Token { kind: TokenKind::KLet, span }) => {
                let (name, _) = self.lexer.expect_ident()?;
                self.lexer.expect(TokenKind::Colon)?;
                let (ty, _) = self.lexer.expect_ident()?;
                self.lexer.expect(TokenKind::Assign)?;
                let expr = self.parse_expr()?;
                let end_span = self.lexer.expect(TokenKind::Semicolon)?;
                let span = Span::between(span, end_span);
                Ok(self.make_stmt(Stmt::Let { name, ty, expr }, span))
            }
            Some(token) => {
                self.lexer.unget(token);
                let expr = self.parse_expr()?;
                let end_span = self.lexer.expect(TokenKind::Semicolon)?;
                let span = Span::between(self.expr_locs[expr], end_span);
                Ok(self.make_stmt(Stmt::Expr(expr), span))
            }
            None => {
                return err!(self.lexer.eof_span(), "expected statement, found end of file");
            }
        }
    }

    fn parse_if_stmt(&mut self, token_span: Span) -> CompileResult<StmtRef> {
        let cond = self.parse_expr()?;
        let then = self.parse_brace_block()?;

        let mut span = Span::between(token_span, self.stmt_locs[then]);
        let mut els = None;

        if self.lexer.matches(TokenKind::KElse).is_some() {
            if let Some(token_span) = self.lexer.matches(TokenKind::KIf) {
                let stmt = self.parse_if_stmt(token_span)?;
                els = Some(stmt);
                span = Span::between(span, self.stmt_locs[stmt]);
            } else {
                let block = self.parse_brace_block()?;
                els = Some(block);
                span = Span::between(span, self.stmt_locs[block]);
            }
        }

        Ok(self.make_stmt(Stmt::If { cond, then, els }, span))
    }

    fn parse_brace_block(&mut self) -> CompileResult<StmtRef> {
        let start_span = self.lexer.expect(TokenKind::LBrace)?;

        let mut stmts = Vec::new();
        while !self.lexer.is_eof() {
            if let Ok(Some(&Token { kind: TokenKind::RBrace, .. })) = self.lexer.peek() {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }

        let end_span = self.lexer.expect(TokenKind::RBrace)?;
        let span = Span::between(start_span, end_span);
        Ok(self.make_stmt(Stmt::Block(Block { stmts }), span))
    }

    pub fn parse_block(&mut self) -> CompileResult<StmtRef> {
        let mut stmts = Vec::new();
        while !self.lexer.is_eof() {
            stmts.push(self.parse_stmt()?);
        }
        let span = if stmts.len() == 0 {
            Span::new(0, 0)
        } else {
            Span::between(self.stmt_locs[*stmts.first().unwrap()], self.stmt_locs[*stmts.last().unwrap()])
        };
        Ok(self.make_stmt(Stmt::Block(Block { stmts }), span))
    }

    pub fn fmt_stmt(&self, stmt: StmtRef) -> StmtFormatter {
        StmtFormatter {
            parser: self,
            stmt
        }
    }
}

pub struct StmtFormatter<'a, 'src: 'a> {
    parser: &'a Parser<'src>,
    stmt: StmtRef,
}

impl<'a, 'src: 'a> fmt::Display for StmtFormatter<'a, 'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_stmt(f, self.parser, self.stmt, 0)
    }
}

fn print_stmt(f: &mut fmt::Formatter, p: &Parser, stmt: StmtRef, indentation: i32) -> fmt::Result {
    match *p.get_stmt(stmt) {
        Stmt::Let { ref name, ref ty, expr } => {
            indent(f, indentation)?;
            writeln!(f, "let {}: {} =", name, ty)?;
            print_expr(f, p, expr, indentation + 1)?;
        }
        Stmt::Block(Block { ref stmts }) => {
            for &stmt in stmts {
                print_stmt(f, p, stmt, indentation)?;
            }
        }
        Stmt::Expr(expr) => print_expr(f, p, expr, indentation)?,
        Stmt::If { cond, then, els } => {
            indent(f, indentation)?;
            writeln!(f, "if")?;
            print_expr(f, p, cond, indentation + 1)?;
            indent(f, indentation)?;
            writeln!(f, "then")?;
            print_stmt(f, p, then, indentation + 1)?;
            if let Some(els) = els {
                indent(f, indentation)?;
                writeln!(f, "else")?;
                print_stmt(f, p, els, indentation + 1)?;
            }
        }
        Stmt::While { cond, block } => {
            indent(f, indentation)?;
            writeln!(f, "while")?;
            print_expr(f, p, cond, indentation + 1)?;
            print_stmt(f, p, block, indentation + 1)?;
        }
    }
    Ok(())
}

enum Stmt {
    Let {
        name: String,
        ty: String,
        expr: ExprRef,
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

struct Block {
    stmts: Vec<StmtRef>,
}

#[derive(Clone, Copy, Debug)]
pub struct StmtRef(u32);
impl_entity!(StmtRef);

fn indent(f: &mut fmt::Formatter, n: i32) -> fmt::Result {
    for _ in 0..n {
        write!(f, "    ")?;
    }
    Ok(())
}