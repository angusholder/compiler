use std::env;
use std::fmt;
use std::u32;

use backtrace::Backtrace;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span {
            start: start as u32,
            end: end as u32,
        }
    }

    pub fn between(start: Span, end: Span) -> Span {
        Span {
            start: start.start,
            end: end.end,
        }
    }

    pub const INVALID: Span = Span {
        start: u32::MAX,
        end: u32::MAX,
    };

    pub fn as_str<'a>(&self, string: &'a str) -> &'a str {
        &string[self.start as usize..self.end as usize]
    }

    pub fn fmt<'a>(&self, src: &'a str) -> SpanFormatter<'a> {
        SpanFormatter {
            span: *self,
            src,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Span::INVALID
    }
}

pub struct SpanFormatter<'a> {
    span: Span,
    src: &'a str
}

impl<'a> fmt::Display for SpanFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut line = 1;
        let mut column = 1;
        for ch in self.src[..self.span.start as usize].chars() {
            if ch == '\n' {
                line += 1;
                column = 1;
            } else {
                column += 1;
            }
        }
        write!(f, "[{}:{}]", line, column)
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

pub struct CompileError {
    pub span: Span,
    pub msg: String,
    pub backtrace: Backtrace,
}

impl CompileError {
    pub fn new(span: Span, msg: String, backtrace: Backtrace) -> CompileError {
        CompileError { span, msg, backtrace }
    }

    pub fn fmt<'a, 'b>(&'a mut self, src: &'b str) -> CompileErrorFormatter<'a, 'b> {
        if backtrace_enabled() {
            self.backtrace.resolve();
        }
        CompileErrorFormatter { error: self, src }
    }
}

pub struct CompileErrorFormatter<'a, 'b> {
    error: &'a CompileError,
    src: &'b str,
}

impl<'a, 'b> fmt::Display for CompileErrorFormatter<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Error {}: {}", self.error.span.fmt(self.src), self.error.msg)?;

        if backtrace_enabled() {
            writeln!(f, "Backtrace: {:?}", self.error.backtrace)?;
        }
        Ok(())
    }
}

fn backtrace_enabled() -> bool {
    env::vars()
        .find(|&(ref name, _)| name == "RUST_BACKTRACE")
        .map(|(_, value)| value)
        .unwrap_or("0".to_string()) == "1"
}

macro_rules! err {
    ($span:expr, $fmt:expr) => {
        Err($crate::result::CompileError::new($span, format!($fmt), $crate::backtrace::Backtrace::new_unresolved()))
    };
    ($span:expr, $fmt:expr, $($arg:expr),+) => {
        Err($crate::result::CompileError::new($span, format!($fmt, $($arg),+), $crate::backtrace::Backtrace::new_unresolved()))
    };
}