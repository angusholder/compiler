pub type CompileResult<T> = Result<T, Error>;

#[derive(Debug, PartialEq, Eq)]
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

    pub fn as_str<'a>(&self, string: &'a str) -> &'a str {
        &string[self.start as usize..self.end as usize]
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub span: Span,
    pub msg: String,
}

impl Error {
    pub fn new(span: Span, msg: String) -> Error {
        Error { span, msg }
    }
}

macro_rules! err {
    ($span:expr, $fmt:expr) => {
        Err(::result::Error::new($span, format!($fmt)))
    };
    ($span:expr, $fmt:expr, $($arg:expr),+) => {
        Err(::result::Error::new($span, format!($fmt, $($arg),+)))
    };
}