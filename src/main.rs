// Utility modules
#[macro_use]
mod result;
#[macro_use]
mod entity;
mod chars;

mod ast;
mod ast_printer;
mod lexer;
mod parser;
mod type_checker;

extern crate clap;
extern crate backtrace;

use std::error::Error;
use std::fs::File;
use std::io::prelude::*;

use lexer::Lexer;
use parser::Parser;
use type_checker::TypeChecker;

use clap::{ App, Arg, ArgMatches };



fn main() {
    let matches = App::new("Compiler")
        .version("0.1")
        .author("Angus Holder")
        .about("Compiler for a C-like language")
        .arg(Arg::with_name("mode")
            .short("m")
            .long("mode")
            .possible_values(&["tokens", "ast", "typecheck"])
            .default_value("ast"))
        .arg(Arg::with_name("INPUT_FILE")
            .help("Set the input source file to use")
            .index(1)
            .conflicts_with("expression")
            .required_unless("expression"))
        .arg(Arg::with_name("expression")
            .help("An expression to evaluate as input")
            .short("e")
            .long("expr")
            .takes_value(true))
        .get_matches();

    let program = match read_source_code(&matches) {
        Ok(src) => src,
        Err(err) => {
            eprintln!("{}", err);
            return;
        }
    };

    match matches.value_of("mode").unwrap() {
        "tokens" => {
            let mut lexer = Lexer::new(&program);
            loop {
                match lexer.next() {
                    Ok(Some(token)) => {
                        println!("{}: {:?}", token.span.fmt(&program), token.kind);
                    }
                    Ok(None) => break,
                    Err(mut error) => {
                        eprintln!("{}", error.fmt(&program));
                        return;
                    }
                }
            }
        }
        "ast" => {
            let mut parser = Parser::new(&program);
            match parser.parse_block() {
                Ok(stmt) => {
                    println!("{}", parser.fmt_stmt(stmt));
                }
                Err(mut error) => {
                    eprintln!("{}", error.fmt(&program));
                    return;
                }
            }
        }
        "typecheck" => {
            match Parser::new(&program).parse() {
                Ok(ast) => {
                    if let Err(mut error) = TypeChecker::new().check(&ast) {
                        eprintln!("{}", error.fmt(&program));
                        return;
                    }
                }
                Err(mut error) => {
                    eprintln!("{}", error.fmt(&program));
                    return;
                }
            }
        }
        _ => unreachable!()
    }
}

fn read_source_code(matches: &ArgMatches) -> Result<String, String> {
    let mut program = String::new();
    if let Some(filename) = matches.value_of("INPUT_FILE") {
        match File::open(filename) {
            Ok(mut file) => {
                let result = file.read_to_string(&mut program);
                if let Err(e) = result {
                    return Err(format!("Error reading source file: {}", e.description()));
                }
            }
            Err(error) => {
                return Err(format!("Failed to open source file: {}", error.description()));
            }
        }
    } else {
        let expression = matches.value_of("expression").unwrap();
        program.push_str(expression);
    }

    Ok(program)
}
