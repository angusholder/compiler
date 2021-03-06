use ast::{ StmtRef, Stmt, ExprRef, Expr, Lit, UnaryOpKind, BinOpKind, Block, Ast };
use ast_printer::{ ExprFormatter, StmtFormatter };
use entity::{ PrimaryMap, EntityMap };
use lexer::Lexer;
use result::{ Span, CompileResult };
use tokens::{ TokenKind, Token };

pub struct Parser<'src> {
    lexer: Lexer<'src>,
    expressions: PrimaryMap<ExprRef, Expr>,
    expr_locs: EntityMap<ExprRef, Span>,
    statements: PrimaryMap<StmtRef, Stmt>,
    stmt_locs: EntityMap<StmtRef, Span>,
}

impl<'src> Parser<'src> {
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

    pub fn get_expr(&self, expr_ent: ExprRef) -> &Expr {
        &self.expressions[expr_ent]
    }

    fn make_stmt(&mut self, stmt: Stmt, span: Span) -> StmtRef {
        let stmt_ref = self.statements.push(stmt);
        self.stmt_locs[stmt_ref] = span;
        stmt_ref
    }

    pub fn get_stmt(&self, stmt_ref: StmtRef) -> &Stmt {
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
        TokenKind::KTrue => Lit::Bool(true),
        TokenKind::KFalse => Lit::Bool(false),
        TokenKind::Ident(i) => Lit::Ident(i),
        TokenKind::Int(n) => Lit::Int(n),
        TokenKind::Float(n) => Lit::Float(n),
        _ => unreachable!(),
    };

    Ok(p.make_expr(Expr::Lit(atom), token.span))
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
    Ok(p.make_expr(Expr::Field { left, ident }, span))
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
        use tokens::TokenKind::*;

        let (nud, bp): (NullDenotation, i32) = match token.kind {
            Ident(_) | KTrue | KFalse | Int(_) | Float(_) => {
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
        use tokens::TokenKind::*;

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
        let left = self.parse_expr_until(0)?;
        if self.lexer.matches(TokenKind::Assign).is_some() {
            let right = self.parse_expr_until(0)?;
            let span = Span::between(self.expr_locs[left], self.expr_locs[right]);
            Ok(self.make_expr(Expr::Assignment { left, right }, span))
        } else {
            Ok(left)
        }
    }

    pub fn fmt_expr(&self, expr: ExprRef) -> ExprFormatter {
        ExprFormatter::new(expr, self)
    }
}



//
// Statement parsing
//
impl<'src> Parser<'src> {
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
                Ok(self.make_stmt(Stmt::Let {
                    name,
                    ty: Some(ty),
                    expr: Some(expr)
                }, span))
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

    pub fn parse(mut self) -> CompileResult<Ast<'src>> {
        let root_block = self.parse_block()?;
        Ok(Ast {
            root_block,
            expressions: self.expressions,
            expr_locs: self.expr_locs,
            statements: self.statements,
            stmt_locs: self.stmt_locs,
            src: self.lexer.src(),
        })
    }

    pub fn fmt_stmt(&self, stmt: StmtRef) -> StmtFormatter {
        StmtFormatter::new(stmt, self)
    }
}