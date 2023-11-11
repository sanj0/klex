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
    #[error("unterminated block comment starting at {0}")]
    UnterminatedBlockComment(Loc),
}

/// Holds the location of a token within the code.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct Loc {
    pub file_index: usize,
    pub row: usize,
    pub col: usize,
}

// A Line comment // ... or a Block comment /* ... */
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Comment {
    LineComment(String),
    BlockComment(String),
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

    Comment(Comment),

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
    SemiSemi,
    Period,
    Colon,
    ColonColon,

    Slash,
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

    Arrow,
    BigArrow,

    LBrace,
    RBrace,
    LBrack,
    RBrack,
    LParen,
    RParen,
}

impl Token {
    pub fn spelling(&self) -> String {
        match self {
            Self::Sym(s) | Self::Num(s) => s.into(),
                Self::Str(s) => format!("{s:?}"),

            Self::Comment(c) => c.get().into(),

            s => s.static_spelling().unwrap().into(),
        }
    }

    pub fn static_spelling(&self) -> Option<&'static str> {
        match self {
            Self::Sym(_) | Self::Num(_) | Self::Str(_) | Self::Comment(_) => None,

            Self::Bang => Some("!"),
            Self::Dollar => Some("$"),
            Self::Percent => Some("%"),
            Self::Ampersand => Some("&"),
            Self::Equal => Some("="),
            Self::EqualEqual => Some("=="),
            Self::Question => Some("?"),
            Self::Tick => Some("'"),
            Self::Comma => Some(","),
            Self::SemiColon => Some(";"),
            Self::SemiSemi => Some(";;"),
            Self::Period => Some("."),
            Self::Colon => Some(":"),
            Self::ColonColon => Some("::"),

            Self::Slash => Some("/"),
            Self::SlashEq => Some("/="),
            Self::Aster => Some("*"),
            Self::AsterAster => Some("**"),
            Self::AsterEq => Some("*="),
            Self::Plus => Some("+"),
            Self::PlusPlus => Some("++"),
            Self::PlusEq => Some("+="),
            Self::Dash => Some("-"),
            Self::DashDash => Some("--"),
            Self::DashEq => Some("-="),
            Self::Less => Some("<"),
            Self::LessEq => Some("<="),
            Self::Greater => Some(">"),
            Self::GreaterEq => Some(">="),

            Self::Arrow => Some("->"),
            Self::BigArrow => Some("=>"),

            Self::LBrace => Some("{"),
            Self::RBrace => Some("}"),
            Self::LBrack => Some("["),
            Self::RBrack => Some("]"),
            Self::LParen => Some("("),
            Self::RParen => Some(")"),
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

/// writes the tokens so that they can be parsed. The result is computer readable but not human
/// redable.
pub fn write_tokens(xs: &[Token]) -> String {
    xs.iter()
        .map(|t| t.spelling() + " ")
        .collect()
}

impl Comment {
    pub fn get(&self) -> &String {
        match self {
            Self::LineComment(s) | Self::BlockComment(s) => s,
        }
    }
}

