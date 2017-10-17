use lexer::{ Lexer, Token, TokenKind };
use result::{ Span, CompileResult };

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    expressions: Vec<Expr>,
}

impl<'a> Parser<'a> {
    pub fn new(src: &str) -> Parser {
        Parser {
            lexer: Lexer::new(src),
            expressions: Vec::new(),
        }
    }

    fn make_expr(&mut self, expr: Expr) -> ExprEnt {
        let ent = ExprEnt(self.expressions.len() as u32);
        self.expressions.push(expr);
        ent
    }

    fn get_expr(&self, expr_ent: ExprEnt) -> &Expr {
        &self.expressions[expr_ent.0 as usize]
    }

    fn get_expr_mut(&mut self, expr_ent: ExprEnt) -> &mut Expr {
        &mut self.expressions[expr_ent.0 as usize]
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

type NullDenotation = fn(p: &mut Parser, token: Token, bp: i32) -> CompileResult<ExprEnt>;
type LeftDenotation = fn(p: &mut Parser, token: Token, left: ExprEnt, rbp: i32) -> CompileResult<ExprEnt>;

fn null_constant(p: &mut Parser, token: Token, _bp: i32) -> CompileResult<ExprEnt> {
    let atom = match token.kind {
        TokenKind::KTrue => Atom::Bool(true),
        TokenKind::KFalse => Atom::Bool(false),
        TokenKind::Ident(i) => Atom::Ident(i),
        _ => unreachable!(),
    };

    Ok(p.make_expr(Expr::Atom(atom)))
}

fn null_paren(p: &mut Parser, _token: Token, bp: i32) -> CompileResult<ExprEnt> {
    let result = p.parse_expr_until(bp)?;
    p.lexer.expect(TokenKind::RParen)?;
    Ok(result)
}

// Prefix operator.
//
// Low precedence:  return, raise, etc.
//   return x+y is return (x+y), not (return x) + y
//
// High precedence: logical negation, bitwise complement, etc.
//   !x && y is (!x) && y, not !(x && y)
fn null_prefix_op(p: &mut Parser, token: Token, bp: i32) -> CompileResult<ExprEnt> {
    let child = p.parse_expr_until(bp)?;
    let kind = match token.kind {
        TokenKind::Sub => UnaryOpKind::Neg,
        _ => unreachable!(),
    };

    Ok(p.make_expr(Expr::UnaryOp { kind, child }))
}

// Left Denotations -- token that takes an expression on the left
fn left_index(p: &mut Parser, _token: Token, left: ExprEnt, _rbp: i32) -> CompileResult<ExprEnt> {
    let right = p.parse_expr_until(0)?;
    p.lexer.expect(TokenKind::RBracket)?;
    Ok(p.make_expr(Expr::BinOp { left, right, kind: BinOpKind::Index }))
}

fn left_attr(p: &mut Parser, _token: Token, left: ExprEnt, _rbp: i32) -> CompileResult<ExprEnt> {
    let ident = p.lexer.expect_ident()?;
    Ok(p.make_expr(Expr::Attr { left, ident }))
}

fn left_binary_op(p: &mut Parser, token: Token, left: ExprEnt, rbp: i32) -> CompileResult<ExprEnt> {
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

    Ok(p.make_expr(Expr::BinOp { kind, left, right }))
}

const COMMA_PREC: i32 = 1;
fn left_func_call(p: &mut Parser, _token: Token, left: ExprEnt, _rbp: i32) -> CompileResult<ExprEnt> {
    let mut args = Vec::new();
    if !p.lexer.matches(TokenKind::RParen) {
        loop {
            args.push(p.parse_expr_until(COMMA_PREC)?);
            if p.lexer.matches(TokenKind::RParen) {
                break;
            } else {
                p.lexer.expect(TokenKind::Comma)?;
            }
        }
    }

    Ok(p.make_expr(Expr::FuncCall {
        left,
        args: args.into_boxed_slice(),
    }))
}

impl<'a> Parser<'a> {
    fn parse_null(&mut self, token: Token) -> CompileResult<ExprEnt> {
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
                return err!(token.span, "Expected expression, found {:?}", token.kind);
            }
        };

        nud(self, token, bp)
    }

    fn parse_left(&mut self, min_rbp: i32, node: &mut ExprEnt) -> CompileResult<bool> {
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

    pub fn parse_expr_until(&mut self, min_rbp: i32) -> CompileResult<ExprEnt> {
        let token = if let Some(token) = self.lexer.next()? {
            token
        } else {
            let span = Span::new(self.lexer.offset(), self.lexer.offset());
            return err!(span, "Expected expression, found end of file");
        };

        let mut node = self.parse_null(token)?;

        loop {
            let done = self.parse_left(min_rbp, &mut node)?;
            if done {
                return Ok(node);
            }
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    BinOp {
        kind: BinOpKind,
        left: ExprEnt,
        right: ExprEnt,
    },
    UnaryOp {
        kind: UnaryOpKind,
        child: ExprEnt,
    },
    Attr {
        left: ExprEnt,
        ident: String,
    },
    FuncCall {
        left: ExprEnt,
        args: Box<[ExprEnt]>,
    },
    Atom(Atom),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    Ident(String),
    Bool(bool),
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum UnaryOpKind {
    Neg,
}

#[derive(Clone, Copy, Debug)]
pub struct ExprEnt(u32);