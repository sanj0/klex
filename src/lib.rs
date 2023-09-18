use std::fmt::{self, Display};

use thiserror::Error;

pub mod lexer;

pub use lexer::Lexer;

#[derive(Error, Debug)]
pub enum KlexError {
    #[error("unterminated string literal starting at {0}")]
    UnterminatedStringLiteral(Loc),
    #[error("invalid escape sequence at {0}")]
    InvalidEscapeSequence(Loc),
}

/// Holds the location of a token within the code.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Loc {
    pub file_index: usize,
    pub row: usize,
    pub col: usize,
}

/// A rich token, which includes an inner [token](Token), its [location](Loc) and its length in
/// characters.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RichToken {
    pub inner: Token,
    pub loc: Loc,
    pub len: usize,
}

/// A token.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Token {
    Sym(String),
    Num(String),
    Str(String),

    Bang,
    Dollar,
    Percent,
    Ampersand,
    Equal,
    EqualEqual,
    Question,
    Tick,
    Comma,
    SemiColon,
    Period,
    Colon,
    ColonColon,

    Slash,
    SlashSlash,
    SlashEq,
    Aster,
    AsterAster,
    AsterEq,
    Plus,
    PlusPlus,
    PlusEq,
    Dash,
    DashDash,
    DashEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,

    LBrace,
    RBrace,
    LBrack,
    RBrack,
    LParen,
    RParen,
}

impl Token {
    pub fn spelling(&self) -> &str {
        match self {
            Self::Sym(s) | Self::Num(s) | Self::Str(s) => s,
            Self::Bang => "!",
            Self::Dollar => "$",
            Self::Percent => "%",
            Self::Ampersand => "&",
            Self::Equal => "=",
            Self::EqualEqual => "==",
            Self::Question => "?",
            Self::Tick => "'",
            Self::Comma => ",",
            Self::SemiColon => ";",
            Self::Period => ".",
            Self::Colon => ":",
            Self::ColonColon => "::",

            Self::Slash => "/",
            Self::SlashSlash => "//",
            Self::SlashEq => "/=",
            Self::Aster => "*",
            Self::AsterAster => "**",
            Self::AsterEq => "*=",
            Self::Plus => "+",
            Self::PlusPlus => "++",
            Self::PlusEq => "+=",
            Self::Dash => "-",
            Self::DashDash => "--",
            Self::DashEq => "-=",
            Self::Less => "<",
            Self::LessEq => "<=",
            Self::Greater => ">",
            Self::GreaterEq => ">=",

            Self::LBrace => "{",
            Self::RBrace => "}",
            Self::LBrack => "[",
            Self::RBrack => "]",
            Self::LParen => "(",
            Self::RParen => ")",
        }
    }
}

impl RichToken {
    pub fn new(inner: Token, loc: Loc, len: usize) -> Self {
        Self { inner, loc, len }
    }
}

impl Loc {
    pub fn start_of_file(file_index: usize) -> Self {
        Self {
            file_index,
            row: 1,
            col: 1,
        }
    }
}

impl std::ops::Sub<Loc> for Loc {
    type Output = usize;

    fn sub(self, other: Loc) -> Self::Output {
        self.col - other.col
    }
}

impl std::ops::Sub<usize> for Loc {
    type Output = Loc;

    fn sub(self, other: usize) -> Self::Output {
        Loc {
            file_index: self.file_index,
            row: self.row,
            col: self.col - other,
        }
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "file{}:{}:{}", self.file_index, self.row, self.col)
    }
}

pub fn format_tokens(xs: &[RichToken]) -> String {
    let mut buf = String::new();
    for (i, rt) in xs.into_iter().enumerate() {
        let token = &rt.inner;
        buf += token.spelling();
        match token {
            Token::LParen => (),
            Token::LBrace => {
                buf.push('\n');
            }
            Token::RBrace => {
                buf.push('\n');
            }
            _ => {
                if !matches!(xs.get(i+1).map(|t| &t.inner), Some(Token::RParen)) {
                    buf.push(' ');
                }
            }
        }
    }
    buf = buf.trim_end().into();
    buf
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn format_tokens() {
        let fmt = super::format_tokens(&Lexer::new("   a* (   b+   c  )     =d", 0).lex().unwrap());
        assert_eq!(fmt, "a * (b + c) = d");
    }
}
