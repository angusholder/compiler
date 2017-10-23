use std::collections::HashMap;
use std::fmt;

use ast::{ Ast, ExprRef, Expr, StmtRef, Stmt, Lit, Block };
use entity::EntityMap;
use result::{ CompileResult, Span };

pub struct TypeChecker {
    variable_types: HashMap<String, Type>,
    expr_types: EntityMap<ExprRef, Type>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Infer,
    Void,
    I32,
    F32,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match *self {
            Type::Infer => "<infer>",
            Type::Void => "void",
            Type::I32 => "i32",
            Type::F32 => "f32",
        })
    }
}

impl Type {
    pub fn from_str(s: &str) -> CompileResult<Type> {
        match s {
            "i32" => Ok(Type::I32),
            "f32" => Ok(Type::F32),
            _ => err!(Span::INVALID, "Invalid type {}", s),
        }
    }
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            variable_types: HashMap::new(),
            expr_types: EntityMap::with_default(Type::Infer),
        }
    }

    pub fn check(&mut self, ast: &Ast) -> CompileResult<()> {
        if let Stmt::Block(_) = ast.statements[ast.root_block] {
            self.check_stmt(ast, ast.root_block)?;
            self.verify_all_exprs_have_types(ast)?;
        } else {
            panic!("Root node must be a block");
        }
        Ok(())
    }

    fn check_stmt(&mut self, ast: &Ast, stmt: StmtRef) -> CompileResult<()> {
        match ast.statements[stmt] {
            Stmt::Block(Block { ref stmts }) => {
                for &inner_stmt in stmts.iter() {
                    self.check_stmt(ast, inner_stmt)?;
                }
            }
            Stmt::While { cond, block } => {
                self.check_expr(ast, cond)?;
                self.check_stmt(ast, block)?;
            }
            Stmt::If { cond, then, els } => {
                self.check_expr(ast, cond)?;
                self.check_stmt(ast, then)?;
                if let Some(els) = els {
                    self.check_stmt(ast, els)?;
                }
            }
            Stmt::Expr(expr) => {
                self.check_expr(ast, expr)?;
            }
            Stmt::Let { ref name, ref ty, expr } => {
                // For now assume neither of these are optional
                let ty = ty.as_ref().unwrap();
                let expr = expr.unwrap();

                let left_ty = Type::from_str(ty)?;
                let prev = self.variable_types.insert(name.to_string(), left_ty);
                if let Some(prev_type) = prev {
                    return err!(ast.stmt_locs[stmt],
                        "variable `{}` shadows variable of type {}", name, ty);
                }

                let right_ty = self.check_expr(ast, expr)?;

                if left_ty != right_ty {
                    return err!(ast.stmt_locs[stmt],
                        "tried to assign expression of type {} to variable of type {}",
                        right_ty, left_ty)
                }
            }
        }
        Ok(())
    }

    fn lookup_variable(&self, ident: &str, span: Span) -> CompileResult<Type> {
        if let Some(&ty) = self.variable_types.get(ident) {
            Ok(ty)
        } else {
            return err!(span, "use of undeclared variable `{}`", ident);
        }
    }

    fn check_expr(&mut self, ast: &Ast, expr: ExprRef) -> CompileResult<Type> {
        let ty: Type = match ast.expressions[expr] {
            Expr::Lit(Lit::Ident(ref ident)) => {
                self.lookup_variable(ident, ast.expr_locs[expr])?
            }
            Expr::Lit(Lit::Bool(_)) => unimplemented!(),
            Expr::Lit(Lit::Int(_)) => Type::I32,
            Expr::Lit(Lit::Float(_)) => Type::F32,

            Expr::Field { .. } => unimplemented!(),
            Expr::FuncCall { .. } => unimplemented!(),

            Expr::UnaryOp { child, .. } => {
                self.check_expr(ast, child)?
            }
            Expr::BinOp { left, right, .. } => {
                let left_ty = self.check_expr(ast, left)?;
                let right_ty = self.check_expr(ast, right)?;
                if left_ty != right_ty {
                    return err!(ast.expr_locs[expr],
                        "left and right must have same type, got {} and {}", left_ty, right_ty);
                } else {
                    left_ty
                }
            }
            Expr::Assignment { left, right } => {
                if let Expr::Lit(Lit::Ident(ref ident)) = ast.expressions[left] {
                    let left_ty = self.lookup_variable(ident, ast.expr_locs[left])?;
                    let right_ty = self.check_expr(ast, right)?;
                    if left_ty != right_ty {
                        return err!(ast.expr_locs[expr],
                            "tried to assign expression of type {} to variable of type {}",
                            right_ty, left_ty);
                    } else {
                        Type::Void
                    }
                } else {
                    return err!(ast.expr_locs[left], "LHS of assignment must be an identifier");
                }
            }
        };

        self.expr_types[expr] = ty;
        Ok(ty)
    }

    fn verify_all_exprs_have_types(&self, ast: &Ast) -> CompileResult<()> {
        for expr in self.expr_types.keys() {
            let ty = self.expr_types[expr];
            if ty == Type::Infer {
                return err!(ast.expr_locs[expr], "subexpression has no type");
            }
        }
        Ok(())
    }
}