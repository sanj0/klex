use criterion::{black_box, criterion_group, criterion_main, Criterion};
use klex::Lexer;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("lex 1k", |b| b.iter(|| Lexer::new(black_box(SRC), 0).lex()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);

const SRC: &str = r#"
use std::{iter::Peekable, str::Chars};

use crate::{KlexError, Loc, RichToken, Token};

#[derive(Clone, Debug)]
struct CharStream<'a> {
    inner: Peekable<Chars<'a>>,
    loc: Loc,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    chars: CharStream<'a>,
}

fn is_separator_char(c: char) -> bool {
    !(c.is_ascii_alphabetic() || c.is_ascii_digit() || "_$".contains(c))
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            chars: CharStream::new(src, file_index),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<RichToken>, KlexError> {
        macro_rules! extend {
            ($base:ident to $ex:ident if $c:literal) => {
                if self.chars.peek() == Some(&$c) {
                    self.chars.next();
                    $ex
                } else {
                    $base
                }
            };
            ($base:ident to $ex1:ident if $c1:literal or $ex2:ident if $c2:literal) => {
                match self.chars.peek() {
                    Some(&$c1) => {
                        self.chars.next();
                        $ex1
                    }
                    Some(&$c2) => {
                        self.chars.next();
                        $ex2
                    }
                    _ => $base,
                }
            };
        }
        let mut tokens = Vec::new();

        use Token::*;
        while let Some(c0) = self.chars.next_skip_ws() {
            let token_start = self.chars.loc - 1;
            let token = match c0 {
                '0'..='9' => self.consume_num(c0),
                '"' => self.consume_string_after_quote(token_start)?,

                '!' => Bang,
                '$' => Dollar,
                '%' => Percent,
                '&' => Ampersand,
                '=' => extend!(Equal to EqualEqual if '='),
                '?' => Question,
                '\'' => Tick,
                ',' => Comma,
                ';' => SemiColon,
                '.' => Period,
                ':' => extend!(Colon to ColonColon if ':'),

                '/' => extend!(Slash to SlashSlash if '/' or SlashEq if '='),
                '*' => extend!(Aster to AsterAster if '*' or AsterEq if '='),
                '+' => extend!(Plus to PlusPlus if '+' or PlusEq if '='),
                '-' => extend!(Dash to DashDash if '-' or DashEq if '='),
                '<' => extend!(Less to LessEq if '='),
                '>' => extend!(Greater to GreaterEq if '='),

                '{' => LBrace,
                '}' => RBrace,
                '[' => LBrack,
                ']' => RBrack,
                '(' => LParen,
                ')' => RParen,
                _ => self.consume_symbol(c0),
            };
            tokens.push(RichToken::new(token, token_start, self.chars.loc - token_start));
        }
        Ok(tokens)
    }

    fn consume_num(&mut self, c0: char) -> Token {
        Token::Num(self.buf_while(c0, |c| c.is_ascii_digit() || c == '.'))
    }

    fn consume_string_after_quote(&mut self, loc: Loc) -> Result<Token, KlexError> {
        let mut buf = String::new();

        while let Some(c) = self.chars.next() {
            match c {
                '"' => return Ok(Token::Str(buf)),
                '\\' => match self.chars.next() {
                    Some(c @ '"') => buf.push(c),
                    Some(c @ '\\') => buf.push(c),
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some('r') => buf.push('\r'),
                    Some(_) => return Err(KlexError::InvalidEscapeSequence(self.chars.loc - 1)),
                    None => return Err(KlexError::UnterminatedStringLiteral(loc)),
                }
                '\n' => return Err(KlexError::UnterminatedStringLiteral(loc)),
                _ => buf.push(c),
            }
        }
        Err(KlexError::UnterminatedStringLiteral(loc))
    }

    fn consume_symbol(&mut self, c0: char) -> Token {
        Token::Sym(self.buf_while(c0, |c| !is_separator_char(c)))
    }

    fn buf_while<F>(&mut self, c0: char, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut buf = String::new();
        buf.push(c0);
        while let Some(&c) = self.chars.peek() {
            if f(c) {
                buf.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        buf
    }
}

impl<'a> Iterator for CharStream<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let c = self.inner.next()?;
        if c == '\n' {
            self.loc.row += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }
        Some(c)
    }
}

impl<'a> CharStream<'a> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            inner: src.chars().peekable(),
            loc: Loc::start_of_file(file_index),
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    pub fn next_skip_ws(&mut self) -> Option<char> {
        self.by_ref().find(|c| !c.is_whitespace())
    }
}

#[cfg(test)]
mod tests {
    const SEPARATOR_SRC: &str = "!$% &= ==? ' , ;.: ::";
    const SYMBOL_SRC: &str = "foo+bar =baz_or$not";
    use super::*;

    fn unwrap_rich_tokens(xs: Vec<RichToken>) -> Vec<Token> {
        xs.into_iter().map(|t| t.inner).collect()
    }

    #[test]
    fn lex_empty_str() {
        let mut lexer = Lexer::new("", 0);
        assert_eq!(lexer.lex().unwrap(), Vec::new());
    }

    #[test]
    fn lex_separators() {
        use crate::Token::*;
        let mut lexer = Lexer::new(SEPARATOR_SRC, 0);
        let tokens = unwrap_rich_tokens(lexer.lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Bang, Dollar, Percent, Ampersand, Equal, EqualEqual, Question, Tick, Comma,
                SemiColon, Period, Colon, ColonColon,
            ]
        );
    }

    #[test]
    fn spell_separators() {
        let tokens = unwrap_rich_tokens(Lexer::new(SEPARATOR_SRC, 0).lex().unwrap());
        let actual: String = tokens.iter().map(Token::spelling).collect();
        let expected: String = SEPARATOR_SRC
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_symbols() {
        use crate::Token::*;
        let tokens = unwrap_rich_tokens(Lexer::new(SYMBOL_SRC, 0).lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Sym("foo".into()),
                Plus,
                Sym("bar".into()),
                Equal,
                Sym("baz_or$not".into()),
            ]
        );
    }

    #[test]
    fn loc() {
        macro_rules! loc { ($row:literal, $col:literal) => {Loc { file_index: 0, row: $row, col: $col }}}
        let mut src = String::from(SYMBOL_SRC);
        src += "\n\n  ::\n     hi_hello";
        let tokens = Lexer::new(&src, 0).lex().unwrap();
        let locs: Vec<_> = tokens.into_iter().map(|t| (t.loc, t.len)).collect();
        assert_eq!(
            locs,
            vec![
                (loc!(1, 1), 3),
                (loc!(1, 4), 1),
                (loc!(1, 5), 3),
                (loc!(1, 9), 1),
                (loc!(1, 10), 10),
                (loc!(3, 3), 2),
                (loc!(4, 6), 8),
            ]
        );
    }

    #[test]
    fn lex_string() {
        let s = "\"This is a string!\", said klex.";
        let tokens = unwrap_rich_tokens(Lexer::new(s, 0).lex().unwrap());
        assert_eq!(tokens, vec![Token::Str(s.into())]);
    }
}
use std::{iter::Peekable, str::Chars};

use crate::{KlexError, Loc, RichToken, Token};

#[derive(Clone, Debug)]
struct CharStream<'a> {
    inner: Peekable<Chars<'a>>,
    loc: Loc,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    chars: CharStream<'a>,
}

fn is_separator_char(c: char) -> bool {
    !(c.is_ascii_alphabetic() || c.is_ascii_digit() || "_$".contains(c))
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            chars: CharStream::new(src, file_index),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<RichToken>, KlexError> {
        macro_rules! extend {
            ($base:ident to $ex:ident if $c:literal) => {
                if self.chars.peek() == Some(&$c) {
                    self.chars.next();
                    $ex
                } else {
                    $base
                }
            };
            ($base:ident to $ex1:ident if $c1:literal or $ex2:ident if $c2:literal) => {
                match self.chars.peek() {
                    Some(&$c1) => {
                        self.chars.next();
                        $ex1
                    }
                    Some(&$c2) => {
                        self.chars.next();
                        $ex2
                    }
                    _ => $base,
                }
            };
        }
        let mut tokens = Vec::new();

        use Token::*;
        while let Some(c0) = self.chars.next_skip_ws() {
            let token_start = self.chars.loc - 1;
            let token = match c0 {
                '0'..='9' => self.consume_num(c0),
                '"' => self.consume_string_after_quote(token_start)?,

                '!' => Bang,
                '$' => Dollar,
                '%' => Percent,
                '&' => Ampersand,
                '=' => extend!(Equal to EqualEqual if '='),
                '?' => Question,
                '\'' => Tick,
                ',' => Comma,
                ';' => SemiColon,
                '.' => Period,
                ':' => extend!(Colon to ColonColon if ':'),

                '/' => extend!(Slash to SlashSlash if '/' or SlashEq if '='),
                '*' => extend!(Aster to AsterAster if '*' or AsterEq if '='),
                '+' => extend!(Plus to PlusPlus if '+' or PlusEq if '='),
                '-' => extend!(Dash to DashDash if '-' or DashEq if '='),
                '<' => extend!(Less to LessEq if '='),
                '>' => extend!(Greater to GreaterEq if '='),

                '{' => LBrace,
                '}' => RBrace,
                '[' => LBrack,
                ']' => RBrack,
                '(' => LParen,
                ')' => RParen,
                _ => self.consume_symbol(c0),
            };
            tokens.push(RichToken::new(token, token_start, self.chars.loc - token_start));
        }
        Ok(tokens)
    }

    fn consume_num(&mut self, c0: char) -> Token {
        Token::Num(self.buf_while(c0, |c| c.is_ascii_digit() || c == '.'))
    }

    fn consume_string_after_quote(&mut self, loc: Loc) -> Result<Token, KlexError> {
        let mut buf = String::new();

        while let Some(c) = self.chars.next() {
            match c {
                '"' => return Ok(Token::Str(buf)),
                '\\' => match self.chars.next() {
                    Some(c @ '"') => buf.push(c),
                    Some(c @ '\\') => buf.push(c),
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some('r') => buf.push('\r'),
                    Some(_) => return Err(KlexError::InvalidEscapeSequence(self.chars.loc - 1)),
                    None => return Err(KlexError::UnterminatedStringLiteral(loc)),
                }
                '\n' => return Err(KlexError::UnterminatedStringLiteral(loc)),
                _ => buf.push(c),
            }
        }
        Err(KlexError::UnterminatedStringLiteral(loc))
    }

    fn consume_symbol(&mut self, c0: char) -> Token {
        Token::Sym(self.buf_while(c0, |c| !is_separator_char(c)))
    }

    fn buf_while<F>(&mut self, c0: char, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut buf = String::new();
        buf.push(c0);
        while let Some(&c) = self.chars.peek() {
            if f(c) {
                buf.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        buf
    }
}

impl<'a> Iterator for CharStream<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let c = self.inner.next()?;
        if c == '\n' {
            self.loc.row += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }
        Some(c)
    }
}

impl<'a> CharStream<'a> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            inner: src.chars().peekable(),
            loc: Loc::start_of_file(file_index),
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    pub fn next_skip_ws(&mut self) -> Option<char> {
        self.by_ref().find(|c| !c.is_whitespace())
    }
}

#[cfg(test)]
mod tests {
    const SEPARATOR_SRC: &str = "!$% &= ==? ' , ;.: ::";
    const SYMBOL_SRC: &str = "foo+bar =baz_or$not";
    use super::*;

    fn unwrap_rich_tokens(xs: Vec<RichToken>) -> Vec<Token> {
        xs.into_iter().map(|t| t.inner).collect()
    }

    #[test]
    fn lex_empty_str() {
        let mut lexer = Lexer::new("", 0);
        assert_eq!(lexer.lex().unwrap(), Vec::new());
    }

    #[test]
    fn lex_separators() {
        use crate::Token::*;
        let mut lexer = Lexer::new(SEPARATOR_SRC, 0);
        let tokens = unwrap_rich_tokens(lexer.lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Bang, Dollar, Percent, Ampersand, Equal, EqualEqual, Question, Tick, Comma,
                SemiColon, Period, Colon, ColonColon,
            ]
        );
    }

    #[test]
    fn spell_separators() {
        let tokens = unwrap_rich_tokens(Lexer::new(SEPARATOR_SRC, 0).lex().unwrap());
        let actual: String = tokens.iter().map(Token::spelling).collect();
        let expected: String = SEPARATOR_SRC
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_symbols() {
        use crate::Token::*;
        let tokens = unwrap_rich_tokens(Lexer::new(SYMBOL_SRC, 0).lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Sym("foo".into()),
                Plus,
                Sym("bar".into()),
                Equal,
                Sym("baz_or$not".into()),
            ]
        );
    }

    #[test]
    fn loc() {
        macro_rules! loc { ($row:literal, $col:literal) => {Loc { file_index: 0, row: $row, col: $col }}}
        let mut src = String::from(SYMBOL_SRC);
        src += "\n\n  ::\n     hi_hello";
        let tokens = Lexer::new(&src, 0).lex().unwrap();
        let locs: Vec<_> = tokens.into_iter().map(|t| (t.loc, t.len)).collect();
        assert_eq!(
            locs,
            vec![
                (loc!(1, 1), 3),
                (loc!(1, 4), 1),
                (loc!(1, 5), 3),
                (loc!(1, 9), 1),
                (loc!(1, 10), 10),
                (loc!(3, 3), 2),
                (loc!(4, 6), 8),
            ]
        );
    }

    #[test]
    fn lex_string() {
        let s = "\"This is a string!\", said klex.";
        let tokens = unwrap_rich_tokens(Lexer::new(s, 0).lex().unwrap());
        assert_eq!(tokens, vec![Token::Str(s.into())]);
    }
}
use std::{iter::Peekable, str::Chars};

use crate::{KlexError, Loc, RichToken, Token};

#[derive(Clone, Debug)]
struct CharStream<'a> {
    inner: Peekable<Chars<'a>>,
    loc: Loc,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    chars: CharStream<'a>,
}

fn is_separator_char(c: char) -> bool {
    !(c.is_ascii_alphabetic() || c.is_ascii_digit() || "_$".contains(c))
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            chars: CharStream::new(src, file_index),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<RichToken>, KlexError> {
        macro_rules! extend {
            ($base:ident to $ex:ident if $c:literal) => {
                if self.chars.peek() == Some(&$c) {
                    self.chars.next();
                    $ex
                } else {
                    $base
                }
            };
            ($base:ident to $ex1:ident if $c1:literal or $ex2:ident if $c2:literal) => {
                match self.chars.peek() {
                    Some(&$c1) => {
                        self.chars.next();
                        $ex1
                    }
                    Some(&$c2) => {
                        self.chars.next();
                        $ex2
                    }
                    _ => $base,
                }
            };
        }
        let mut tokens = Vec::new();

        use Token::*;
        while let Some(c0) = self.chars.next_skip_ws() {
            let token_start = self.chars.loc - 1;
            let token = match c0 {
                '0'..='9' => self.consume_num(c0),
                '"' => self.consume_string_after_quote(token_start)?,

                '!' => Bang,
                '$' => Dollar,
                '%' => Percent,
                '&' => Ampersand,
                '=' => extend!(Equal to EqualEqual if '='),
                '?' => Question,
                '\'' => Tick,
                ',' => Comma,
                ';' => SemiColon,
                '.' => Period,
                ':' => extend!(Colon to ColonColon if ':'),

                '/' => extend!(Slash to SlashSlash if '/' or SlashEq if '='),
                '*' => extend!(Aster to AsterAster if '*' or AsterEq if '='),
                '+' => extend!(Plus to PlusPlus if '+' or PlusEq if '='),
                '-' => extend!(Dash to DashDash if '-' or DashEq if '='),
                '<' => extend!(Less to LessEq if '='),
                '>' => extend!(Greater to GreaterEq if '='),

                '{' => LBrace,
                '}' => RBrace,
                '[' => LBrack,
                ']' => RBrack,
                '(' => LParen,
                ')' => RParen,
                _ => self.consume_symbol(c0),
            };
            tokens.push(RichToken::new(token, token_start, self.chars.loc - token_start));
        }
        Ok(tokens)
    }

    fn consume_num(&mut self, c0: char) -> Token {
        Token::Num(self.buf_while(c0, |c| c.is_ascii_digit() || c == '.'))
    }

    fn consume_string_after_quote(&mut self, loc: Loc) -> Result<Token, KlexError> {
        let mut buf = String::new();

        while let Some(c) = self.chars.next() {
            match c {
                '"' => return Ok(Token::Str(buf)),
                '\\' => match self.chars.next() {
                    Some(c @ '"') => buf.push(c),
                    Some(c @ '\\') => buf.push(c),
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some('r') => buf.push('\r'),
                    Some(_) => return Err(KlexError::InvalidEscapeSequence(self.chars.loc - 1)),
                    None => return Err(KlexError::UnterminatedStringLiteral(loc)),
                }
                '\n' => return Err(KlexError::UnterminatedStringLiteral(loc)),
                _ => buf.push(c),
            }
        }
        Err(KlexError::UnterminatedStringLiteral(loc))
    }

    fn consume_symbol(&mut self, c0: char) -> Token {
        Token::Sym(self.buf_while(c0, |c| !is_separator_char(c)))
    }

    fn buf_while<F>(&mut self, c0: char, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut buf = String::new();
        buf.push(c0);
        while let Some(&c) = self.chars.peek() {
            if f(c) {
                buf.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        buf
    }
}

impl<'a> Iterator for CharStream<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let c = self.inner.next()?;
        if c == '\n' {
            self.loc.row += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }
        Some(c)
    }
}

impl<'a> CharStream<'a> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            inner: src.chars().peekable(),
            loc: Loc::start_of_file(file_index),
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    pub fn next_skip_ws(&mut self) -> Option<char> {
        self.by_ref().find(|c| !c.is_whitespace())
    }
}

#[cfg(test)]
mod tests {
    const SEPARATOR_SRC: &str = "!$% &= ==? ' , ;.: ::";
    const SYMBOL_SRC: &str = "foo+bar =baz_or$not";
    use super::*;

    fn unwrap_rich_tokens(xs: Vec<RichToken>) -> Vec<Token> {
        xs.into_iter().map(|t| t.inner).collect()
    }

    #[test]
    fn lex_empty_str() {
        let mut lexer = Lexer::new("", 0);
        assert_eq!(lexer.lex().unwrap(), Vec::new());
    }

    #[test]
    fn lex_separators() {
        use crate::Token::*;
        let mut lexer = Lexer::new(SEPARATOR_SRC, 0);
        let tokens = unwrap_rich_tokens(lexer.lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Bang, Dollar, Percent, Ampersand, Equal, EqualEqual, Question, Tick, Comma,
                SemiColon, Period, Colon, ColonColon,
            ]
        );
    }

    #[test]
    fn spell_separators() {
        let tokens = unwrap_rich_tokens(Lexer::new(SEPARATOR_SRC, 0).lex().unwrap());
        let actual: String = tokens.iter().map(Token::spelling).collect();
        let expected: String = SEPARATOR_SRC
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_symbols() {
        use crate::Token::*;
        let tokens = unwrap_rich_tokens(Lexer::new(SYMBOL_SRC, 0).lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Sym("foo".into()),
                Plus,
                Sym("bar".into()),
                Equal,
                Sym("baz_or$not".into()),
            ]
        );
    }

    #[test]
    fn loc() {
        macro_rules! loc { ($row:literal, $col:literal) => {Loc { file_index: 0, row: $row, col: $col }}}
        let mut src = String::from(SYMBOL_SRC);
        src += "\n\n  ::\n     hi_hello";
        let tokens = Lexer::new(&src, 0).lex().unwrap();
        let locs: Vec<_> = tokens.into_iter().map(|t| (t.loc, t.len)).collect();
        assert_eq!(
            locs,
            vec![
                (loc!(1, 1), 3),
                (loc!(1, 4), 1),
                (loc!(1, 5), 3),
                (loc!(1, 9), 1),
                (loc!(1, 10), 10),
                (loc!(3, 3), 2),
                (loc!(4, 6), 8),
            ]
        );
    }

    #[test]
    fn lex_string() {
        let s = "\"This is a string!\", said klex.";
        let tokens = unwrap_rich_tokens(Lexer::new(s, 0).lex().unwrap());
        assert_eq!(tokens, vec![Token::Str(s.into())]);
    }
}
use std::{iter::Peekable, str::Chars};

use crate::{KlexError, Loc, RichToken, Token};

#[derive(Clone, Debug)]
struct CharStream<'a> {
    inner: Peekable<Chars<'a>>,
    loc: Loc,
}

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    chars: CharStream<'a>,
}

fn is_separator_char(c: char) -> bool {
    !(c.is_ascii_alphabetic() || c.is_ascii_digit() || "_$".contains(c))
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            chars: CharStream::new(src, file_index),
        }
    }

    pub fn lex(&mut self) -> Result<Vec<RichToken>, KlexError> {
        macro_rules! extend {
            ($base:ident to $ex:ident if $c:literal) => {
                if self.chars.peek() == Some(&$c) {
                    self.chars.next();
                    $ex
                } else {
                    $base
                }
            };
            ($base:ident to $ex1:ident if $c1:literal or $ex2:ident if $c2:literal) => {
                match self.chars.peek() {
                    Some(&$c1) => {
                        self.chars.next();
                        $ex1
                    }
                    Some(&$c2) => {
                        self.chars.next();
                        $ex2
                    }
                    _ => $base,
                }
            };
        }
        let mut tokens = Vec::new();

        use Token::*;
        while let Some(c0) = self.chars.next_skip_ws() {
            let token_start = self.chars.loc - 1;
            let token = match c0 {
                '0'..='9' => self.consume_num(c0),
                '"' => self.consume_string_after_quote(token_start)?,

                '!' => Bang,
                '$' => Dollar,
                '%' => Percent,
                '&' => Ampersand,
                '=' => extend!(Equal to EqualEqual if '='),
                '?' => Question,
                '\'' => Tick,
                ',' => Comma,
                ';' => SemiColon,
                '.' => Period,
                ':' => extend!(Colon to ColonColon if ':'),

                '/' => extend!(Slash to SlashSlash if '/' or SlashEq if '='),
                '*' => extend!(Aster to AsterAster if '*' or AsterEq if '='),
                '+' => extend!(Plus to PlusPlus if '+' or PlusEq if '='),
                '-' => extend!(Dash to DashDash if '-' or DashEq if '='),
                '<' => extend!(Less to LessEq if '='),
                '>' => extend!(Greater to GreaterEq if '='),

                '{' => LBrace,
                '}' => RBrace,
                '[' => LBrack,
                ']' => RBrack,
                '(' => LParen,
                ')' => RParen,
                _ => self.consume_symbol(c0),
            };
            tokens.push(RichToken::new(token, token_start, self.chars.loc - token_start));
        }
        Ok(tokens)
    }

    fn consume_num(&mut self, c0: char) -> Token {
        Token::Num(self.buf_while(c0, |c| c.is_ascii_digit() || c == '.'))
    }

    fn consume_string_after_quote(&mut self, loc: Loc) -> Result<Token, KlexError> {
        let mut buf = String::new();

        while let Some(c) = self.chars.next() {
            match c {
                '"' => return Ok(Token::Str(buf)),
                '\\' => match self.chars.next() {
                    Some(c @ '"') => buf.push(c),
                    Some(c @ '\\') => buf.push(c),
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some('r') => buf.push('\r'),
                    Some(_) => return Err(KlexError::InvalidEscapeSequence(self.chars.loc - 1)),
                    None => return Err(KlexError::UnterminatedStringLiteral(loc)),
                }
                '\n' => return Err(KlexError::UnterminatedStringLiteral(loc)),
                _ => buf.push(c),
            }
        }
        Err(KlexError::UnterminatedStringLiteral(loc))
    }

    fn consume_symbol(&mut self, c0: char) -> Token {
        Token::Sym(self.buf_while(c0, |c| !is_separator_char(c)))
    }

    fn buf_while<F>(&mut self, c0: char, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut buf = String::new();
        buf.push(c0);
        while let Some(&c) = self.chars.peek() {
            if f(c) {
                buf.push(c);
                self.chars.next();
            } else {
                break;
            }
        }
        buf
    }
}

impl<'a> Iterator for CharStream<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let c = self.inner.next()?;
        if c == '\n' {
            self.loc.row += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }
        Some(c)
    }
}

impl<'a> CharStream<'a> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            inner: src.chars().peekable(),
            loc: Loc::start_of_file(file_index),
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    pub fn next_skip_ws(&mut self) -> Option<char> {
        self.by_ref().find(|c| !c.is_whitespace())
    }
}

#[cfg(test)]
mod tests {
    const SEPARATOR_SRC: &str = "!$% &= ==? ' , ;.: ::";
    const SYMBOL_SRC: &str = "foo+bar =baz_or$not";
    use super::*;

    fn unwrap_rich_tokens(xs: Vec<RichToken>) -> Vec<Token> {
        xs.into_iter().map(|t| t.inner).collect()
    }

    #[test]
    fn lex_empty_str() {
        let mut lexer = Lexer::new("", 0);
        assert_eq!(lexer.lex().unwrap(), Vec::new());
    }

    #[test]
    fn lex_separators() {
        use crate::Token::*;
        let mut lexer = Lexer::new(SEPARATOR_SRC, 0);
        let tokens = unwrap_rich_tokens(lexer.lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Bang, Dollar, Percent, Ampersand, Equal, EqualEqual, Question, Tick, Comma,
                SemiColon, Period, Colon, ColonColon,
            ]
        );
    }

    #[test]
    fn spell_separators() {
        let tokens = unwrap_rich_tokens(Lexer::new(SEPARATOR_SRC, 0).lex().unwrap());
        let actual: String = tokens.iter().map(Token::spelling).collect();
        let expected: String = SEPARATOR_SRC
            .chars()
            .filter(|c| !c.is_whitespace())
            .collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_symbols() {
        use crate::Token::*;
        let tokens = unwrap_rich_tokens(Lexer::new(SYMBOL_SRC, 0).lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Sym("foo".into()),
                Plus,
                Sym("bar".into()),
                Equal,
                Sym("baz_or$not".into()),
            ]
        );
    }

    #[test]
    fn loc() {
        macro_rules! loc { ($row:literal, $col:literal) => {Loc { file_index: 0, row: $row, col: $col }}}
        let mut src = String::from(SYMBOL_SRC);
        src += "\n\n  ::\n     hi_hello";
        let tokens = Lexer::new(&src, 0).lex().unwrap();
        let locs: Vec<_> = tokens.into_iter().map(|t| (t.loc, t.len)).collect();
        assert_eq!(
            locs,
"#;
