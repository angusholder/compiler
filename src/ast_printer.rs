use std::fmt;

use ast::{ StmtRef, Stmt, ExprRef, Expr, Block };
use parser::Parser;

pub struct ExprFormatter<'a, 'src: 'a> {
    root: ExprRef,
    parser: &'a Parser<'src>,
}

impl<'a, 'src: 'a> ExprFormatter<'a, 'src> {
    pub fn new(root: ExprRef, parser: &'a Parser<'src>) -> ExprFormatter<'a, 'src> {
        ExprFormatter { root, parser }
    }
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
        Expr::Lit(ref lit) => {
            writeln!(f, "{}", lit)?;
        }
        Expr::Field { left, ref ident } => {
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
        Expr::Assignment { left, right } => {
            writeln!(f, "assign")?;
            print_expr(f, p, left, indentation + 1)?;
            print_expr(f, p, right, indentation + 1)?;
        }
    }

    Ok(())
}



pub struct StmtFormatter<'a, 'src: 'a> {
    stmt: StmtRef,
    parser: &'a Parser<'src>,
}

impl<'a, 'src: 'a> StmtFormatter<'a, 'src> {
    pub fn new(stmt: StmtRef, parser: &'a Parser<'src>) -> StmtFormatter<'a, 'src> {
        StmtFormatter { stmt, parser }
    }
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
            write!(f, "let {}", name)?;
            if let &Some(ref ty) = ty {
                write!(f, ": {}", ty)?;
            }
            if let Some(expr) = expr {
                writeln!(f, " =")?;
                print_expr(f, p, expr, indentation + 1)?;
            } else {
                writeln!(f)?;
            }
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

fn indent(f: &mut fmt::Formatter, n: i32) -> fmt::Result {
    for _ in 0..n {
        write!(f, "    ")?;
    }
    Ok(())
}