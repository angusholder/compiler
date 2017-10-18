#[macro_use]
mod result;
mod chars;
mod lexer;
mod parser;

fn main() {
    use parser::Parser;
    use result::Error;

    let program = "-b + sqrt(b*b - four*a*c) / (two*a)";
//    let program = "Span_new(index, self.iter.offset())";
    let mut parser = Parser::new(program);

    match parser.parse_expr() {
        Ok(expr) => {
            println!("{}", parser.fmt_expr(expr));
        }
        Err(Error { span, msg }) => {
            println!("error {}:{}: {}", span.start, span.end, msg);
        }
    }
}
