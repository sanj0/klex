//! Turns an [`Iterator`] over [`char`]s into an `Iterator` over [`Token`](RichToken).
//!
//! # Basic Usage
//! Create a [`Lexer`] and iterate over the `Token`s it yields or collect them all into a
//! [`Vec`].
//!
//! # Examples
//! ## Collect `Token`s a source `String`:
//! ```rust
//! use klex::{Lexer, Token};
//! let tokens = Lexer::new("greet = \"Hello, \" + name", 0).lex().unwrap();
//! // Extract the inner token, discarding location information
//! let inner_tokens: Vec<_> = tokens.into_iter().map(|t| t.inner).collect();
//! assert_eq!(
//!     inner_tokens,
//!     vec![
//!         Token::Sym("greet".into()),
//!         Token::Equal,
//!         Token::Str("Hello, ".into()),
//!         Token::Plus,
//!         Token::Sym("name".into()),
//!     ])
//! ```
//!
//! ## Iterate over `Token`s from source `String`
//! ```rust
//! use klex::Lexer;
//! let lexer = Lexer::new("source code", 0);
//! for t in lexer {
//!     match t {
//!         Ok(t) => (),// do something
//!         Err(e) => (),// do something
//!     }
//! }
//! ```
//!
//! ## Iterate over `Token`s from source `Iterator`
//! ```rust
//! use klex::Lexer;
//! let iter = ['a', '=', 'b'].into_iter();
//! let lexer = Lexer::from_iter(iter, 0);
//! for token in lexer.filter_map(|res| res.ok().map(|rt| rt.inner)) {
//!     println!("{}", token.spelling());
//! }
//! ```

use std::fmt::{self, Display};

use thiserror::Error;

pub mod lexer;

pub use lexer::Lexer;

/// Something went wrong while lexing ...
#[derive(Error, Debug, Clone, PartialEq)]
pub enum KlexError {
    #[error("unterminated string literal starting at {0}")]
    UnterminatedStringLiteral(Loc),
    #[error("unterminated char literal starting at {0}")]
    UnterminatedCharLiteral(Loc),
    #[error("invalid escape sequence at {0}")]
    InvalidEscapeSequence(Loc),
    #[error("unterminated block comment starting at {0}")]
    UnterminatedBlockComment(Loc),
}

/// Holds the location of a [`Token`](RichToken) within the code.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[cfg_attr(not(feature = "loc_with_origin"), derive(Copy))]
pub struct Loc {
    /// The index of the source file (or equivalent). This has no intrinsic meaning.
    pub file_index: usize,
    /// The row where the `Token` starts, starting with 1
    pub row: usize,
    /// The col (i. e. line) where the `Token` starts, starting with 1
    pub col: usize,
    /// The origin token (e. g. macro invocation)
    #[cfg(feature = "loc_with_origin")]
    pub origin: Option<Box<Loc>>,
}

/// A comment!
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Comment {
    /// A line comment:
    /// // Lorem Ipsum
    /// -> Comment::LineComment(" Lorem Ipsum")
    LineComment(String),
    /// A block comment:
    /// /* Lorem Ipsum */
    /// -> Comment::BlockComment(" Lorem Ipsum ")
    BlockComment(String),
    /// A waffle comment:
    /// \# Lorem Ipsum
    /// -> Comment::WaffleComment(" Lorem Ipsum")
    #[cfg(feature = "waffle_comments")]
    WaffleComment(String),
}

/// A rich token, which includes the actual [token](Token), its [location](Loc) and its length in
/// characters.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RichToken {
    /// The actual token
    pub inner: Token,
    /// The location of the token within source
    pub loc: Loc,
    /// The length of the tokens in characters
    pub len: usize,
}

/// A token.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub enum Token {
    /// A symbol is a keyword, identifier, etc.
    Sym(String),
    /// A number literal
    Num(String),
    /// A string literal
    Str(String),
    /// A char literal
    Chr(char),

    /// A comment
    Comment(Comment),

    /// !
    Bang,
    /// $
    Dollar,
    /// %
    Percent,
    /// %
    Ampersand,
    /// =
    Equal,
    /// ==
    EqualEqual,
    /// ?
    Question,
    /// '
    Tick,
    /// ,
    Comma,
    /// ;
    SemiColon,
    /// ;;
    SemiSemi,
    /// .
    Period,
    /// :
    Colon,
    /// ::
    ColonColon,

    /// /
    Slash,
    /// /=
    SlashEq,
    /// \
    BackSlash,
    /// *
    Aster,
    /// **
    AsterAster,
    /// *=
    AsterEq,
    /// +
    Plus,
    /// ++
    PlusPlus,
    /// +=
    PlusEq,
    /// -
    Dash,
    /// --
    DashDash,
    /// -=
    DashEq,
    /// <
    Less,
    /// <=
    LessEq,
    /// >
    Greater,
    /// >=
    GreaterEq,

    /// ->
    Arrow,
    /// =>
    BigArrow,

    /// {
    LBrace,
    /// }
    RBrace,
    /// [
    LBrack,
    /// ]
    RBrack,
    /// (
    LParen,
    /// )
    RParen,
}

impl Token {
    /// Converts the `Token` into a representation that spells it.
    /// This means that when lexing the returned String, a copy of the original `Token` would be
    /// produced.
    /// # Example
    /// ```rust
    /// use klex::Token;
    /// assert_eq!("\"hello, world!\"", Token::Str("hello, world!".into()).spelling())
    /// ```
    pub fn spelling(&self) -> String {
        match self {
            Self::Sym(s) | Self::Num(s) => s.into(),
            #[cfg(feature = "raw_strings")]
            Self::Str(s) => format!("\"{s}\""),
            #[cfg(not(feature = "raw_strings"))]
            Self::Str(s) => format!("{s:?}"),
            Self::Chr(c) => format!("{c:?}"),
            Self::Comment(c) => c.spelling(),
            s => s.static_spelling().unwrap().into(),
        }
    }

    /// The same as [`Self::spelling()`] but returns `None` cases where a `&'static str` is not
    /// possible. This is the case for `Sym`, `Num`, `Str` and `Comment`.
    pub fn static_spelling(&self) -> Option<&'static str> {
        match self {
            Self::Sym(_) | Self::Num(_) | Self::Str(_) | Self::Chr(_) | Self::Comment(_) => None,

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
            Self::BackSlash => Some("\\"),
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
    /// Returns a `Loc` that represents the start of the file with the given file id.
    pub fn start_of_file(file_index: usize) -> Self {
        Self {
            file_index,
            row: 1,
            col: 1,
            #[cfg(feature = "loc_with_origin")]
            origin: None,
        }
    }

    /// Prints this location and the correct line of the correct file.
    /// Reads the file at the path at `file_index` in the given slice.
    pub fn pretty_print_with_line(&self, files: &[String]) -> Result<(), String> {
        use std::fs::File;
        use std::io::{BufRead, BufReader};

        let path = files
            .get(self.file_index)
            .ok_or_else(|| format!("file index {} out of range", self.file_index))?;
        let file = File::open(path).map_err(|e| format!("error opening file {path}: {e}"))?;
        let reader = BufReader::new(file);
        let line = reader
            .lines()
            .nth(self.row - 1)
            .ok_or_else(|| format!("no line {} in file {path}", self.file_index))?
            .map_err(|e| format!("error reading {path} to line {}: {e}", self.row))?;
        let line_num_len = (self.col.ilog10() + 1) as usize;
        println!("  --> {path}:{}:{}\n{:width$} | ", self.row, self.col, "", width = line_num_len);
        println!("{:width$} | {line}", self.col, width = line_num_len);
        print!("{:width$} | ", "", width = line_num_len);
        for _ in 0..self.col-1 {
            print!("-");
        }
        println!("^ here");
        Ok(())
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
            #[cfg(feature = "loc_with_origin")]
            origin: self.origin,
        }
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "file{}:{}:{}", self.file_index, self.row, self.col)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Str(_) | Self::Sym(_) | Self::Num(_) | Self::Chr(_) | Self::Comment(_) => {
                write!(f, "{}", self.spelling())
            }
            _ => write!(f, "{}", self.static_spelling().unwrap()),
        }
    }
}

/// Concatenates the [`Token::spelling`] of all tokens from the given `Vec` with a single space
/// after every token. This makes it re-parseable but not very pleasant to read as a human.
pub fn write_tokens(xs: &[Token]) -> String {
    xs.iter().map(|t| t.spelling() + " ").collect()
}

impl Comment {
    pub fn get(&self) -> &String {
        match self {
            Self::LineComment(s) | Self::BlockComment(s) => s,
            #[cfg(feature = "waffle_comments")]
            Self::WaffleComment(s) => s,
        }
    }

    pub fn spelling(&self) -> String {
        match self {
            Self::LineComment(s) => format!("//{s}"),
            Self::BlockComment(s) => format!("/*{s}*/"),
            #[cfg(feature = "waffle_comments")]
            Self::WaffleComment(s) => format!("#{s}"),
        }
    }
}

impl From<KlexError> for String {
    fn from(e: KlexError) -> String {
        e.to_string()
    }
}

impl KlexError {
    pub fn loc(&self) -> Loc {
        match self {
            #[cfg(feature = "loc_with_origin")]
            Self::UnterminatedStringLiteral(loc)
            | Self::UnterminatedCharLiteral(loc)
            | Self::InvalidEscapeSequence(loc)
            | Self::UnterminatedBlockComment(loc) => loc.clone(),
            #[cfg(not(feature = "loc_with_origin"))]
            Self::UnterminatedStringLiteral(loc)
            | Self::UnterminatedCharLiteral(loc)
            | Self::InvalidEscapeSequence(loc)
            | Self::UnterminatedBlockComment(loc) => *loc,
        }
    }
}
