use std::{iter::Peekable, str::Chars};

use crate::{Comment, KlexError, Loc, RichToken, Token};

/// An `Iterator` over `char`s that keeps track of the [`Location`](Loc).
#[derive(Clone, Debug)]
struct CharStream<I>
where
    I: Iterator<Item = char>,
{
    inner: Peekable<I>,
    index: usize,
    loc: Loc,
}

/// Turns an Iterator over `char`s into an `Iterator` over [`tokens`](RichToken).
#[derive(Clone, Debug)]
pub struct Lexer<I>
where
    I: Iterator<Item = char>,
{
    chars: CharStream<I>,
}

fn is_separator_char(c: char) -> bool {
    !(c.is_ascii_alphabetic() || c.is_ascii_digit() || "_$".contains(c))
}

impl<'a> Lexer<Chars<'a>> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self {
            chars: CharStream::new(src, file_index),
        }
    }
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    pub fn from_iter(src: I, file_index: usize) -> Self {
        Self {
            chars: CharStream::from_iter(src, file_index),
        }
    }
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<RichToken, KlexError>;

    fn next(&mut self) -> Option<Self::Item> {
        // this could probably be written with variadics but that's too complicated :)
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
            ($base:ident to $ex1:ident if $c1:literal or $ex2:ident if $c2:literal or $ex3:ident if $c3:literal) => {
                match self.chars.peek() {
                    Some(&$c1) => {
                        self.chars.next();
                        $ex1
                    }
                    Some(&$c2) => {
                        self.chars.next();
                        $ex2
                    }
                    Some(&$c3) => {
                        self.chars.next();
                        $ex3
                    }
                    _ => $base,
                }
            };
        }
        macro_rules! question_mark {
            ($ex:expr) => {{
                match $ex {
                    Ok(x) => x,
                    Err(e) => return Some(Err(e)),
                }
            }};
        }
        use Token::*;
        if let Some(c0) = self.chars.next_skip_ws() {
            let token_loc = self.chars.loc.clone() - 1;
            let start_index = self.chars.index - 1;
            let token = match c0 {
                '0'..='9' => self.consume_num(c0),
                #[cfg(feature = "loc_with_origin")]
                '"' => question_mark!(self.consume_string_after_quote(token_loc.clone())),
                #[cfg(not(feature = "loc_with_origin"))]
                '"' => question_mark!(self.consume_string_after_quote(token_loc)),
                '!' => Bang,
                '$' => Dollar,
                '%' => Percent,
                '&' => Ampersand,
                '=' => extend!(Equal to EqualEqual if '=' or BigArrow if '>'),
                '?' => Question,
                '\'' => question_mark!(self.consume_char_after_tick()),
                ',' => Comma,
                ';' => extend!(SemiColon to SemiSemi if ';'),
                '.' => Period,
                ':' => extend!(Colon to ColonColon if ':'),

                #[cfg(feature = "waffle_comments")]
                '#' => self.consume_waffle_comment(),

                '/' => match self.chars.next() {
                    Some('/') => self.consume_line_comment(),
                    Some('*') => question_mark!(self.consume_block_comment()),
                    Some('=') => SlashEq,
                    _ => Slash,
                },
                '*' => extend!(Aster to AsterAster if '*' or AsterEq if '='),
                '+' => extend!(Plus to PlusPlus if '+' or PlusEq if '='),
                '-' => {
                    if let Some(ch) = self.chars.peek() {
                        if ch.is_ascii_digit() {
                            self.consume_num('-')
                        } else {
                            extend!(Dash to DashDash if '-' or DashEq if '=' or Arrow if '>')
                        }
                    } else {
                        Dash
                    }
                }
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
            Some(Ok(RichToken::new(
                token,
                token_loc,
                self.chars.index - start_index,
            )))
        } else {
            None
        }
    }
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    /// Convenience function that collects the [`tokens`](RichToken) yielded by this `Lexer` into a `Vec`, bailing
    /// out on the first Error stumbled upon.
    pub fn lex(self) -> Result<Vec<RichToken>, KlexError> {
        let mut buf = Vec::new();
        for tok in self {
            match tok {
                Ok(t) => buf.push(t),
                Err(e) => return Err(e),
            }
        }
        Ok(buf)
    }

    fn consume_line_comment(&mut self) -> Token {
        let mut buf = String::new();
        for c in self.chars.by_ref() {
            if c == '\n' {
                break;
            } else {
                buf.push(c);
            }
        }
        Token::Comment(Comment::LineComment(buf))
    }

    #[cfg(feature = "waffle_comments")]
    fn consume_waffle_comment(&mut self) -> Token {
        let mut buf = String::new();
        for c in self.chars.by_ref() {
            if c == '\n' {
                break;
            } else {
                buf.push(c);
            }
        }
        Token::Comment(Comment::WaffleComment(buf))
    }

    fn consume_block_comment(&mut self) -> Result<Token, KlexError> {
        let loc = self.chars.loc.clone();
        let mut buf = String::new();
        while let Some(c) = self.chars.next() {
            if c == '*' && self.chars.peek() == Some(&'/') {
                self.chars.next();
                return Ok(Token::Comment(Comment::BlockComment(buf)));
            }
            buf.push(c);
        }
        Err(KlexError::UnterminatedBlockComment(loc))
    }

    fn consume_num(&mut self, c0: char) -> Token {
        Token::Num(self.buf_while(c0, |c| c.is_ascii_digit() || c == '.' || c == '_'))
    }

    // takes an loc so that UnterminatedStringLiteral errors are reported at the location of the
    // opening quote and not the location of the first character
    fn consume_string_after_quote(&mut self, loc: Loc) -> Result<Token, KlexError> {
        let mut buf = String::new();
        while let Some(c) = self.chars.next() {
            match c {
                '"' => return Ok(Token::Str(buf)),
                #[cfg(not(feature = "raw_strings"))]
                '\\' => match self.chars.next() {
                    Some(c @ '"') | Some(c @ '\\') => buf.push(c),
                    Some('n') => buf.push('\n'),
                    Some('t') => buf.push('\t'),
                    Some('r') => buf.push('\r'),
                    Some(_) => return Err(KlexError::InvalidEscapeSequence(self.chars.loc.clone() - 2)),
                    None => return Err(KlexError::UnterminatedStringLiteral(loc)),
                },
                #[cfg(feature = "raw_strings")]
                '\\' => match self.chars.next() {
                    Some(c @ '"') | Some(c @ '\\') => buf.push(c),
                    Some(c) => {
                        buf.push('\\');
                        buf.push(c);
                    }
                    None => return Err(KlexError::UnterminatedStringLiteral(loc)),
                },
                _ => buf.push(c),
            }
        }
        Err(KlexError::UnterminatedStringLiteral(loc))
    }
    //
    // takes an loc so that UnterminatedStringLiteral errors are reported at the location of the
    // opening quote and not the location of the first character
    fn consume_char_after_tick(&mut self) -> Result<Token, KlexError> {
        let c = match self.chars.next() {
            Some('\\') => match self.chars.next() {
                    Some(c @ '\'') | Some(c @ '\\') => c,
                    Some('n') => '\n',
                    Some('t') => '\t',
                    Some('r') => '\r',
                    Some(_) => return Err(KlexError::InvalidEscapeSequence(self.chars.loc.clone() - 1)),
                    None => return Err(KlexError::UnterminatedCharLiteral(self.chars.loc.clone() - 2)),
            }
            Some('\n') | None => return Err(KlexError::UnterminatedCharLiteral(self.chars.loc.clone() - 1)),
            Some(c) => c,
        };
        if let Some('\'') = self.chars.next() {
            Ok(Token::Chr(c))
        } else {
            Err(KlexError::UnterminatedCharLiteral(self.chars.loc.clone() - 1))
        }
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

impl<I> Iterator for CharStream<I>
where
    I: Iterator<Item = char>,
{
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let c = self.inner.next()?;
        self.index += 1;
        if c == '\n' {
            self.loc.row += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }
        Some(c)
    }
}

impl<'a> CharStream<Chars<'a>> {
    pub fn new(src: &'a str, file_index: usize) -> Self {
        Self::from_iter(src.chars(), file_index)
    }
}

impl<I> CharStream<I>
where
    I: Iterator<Item = char>,
{
    pub fn from_iter(iter: I, file_index: usize) -> Self {
        Self {
            inner: iter.peekable(),
            index: 0,
            loc: Loc::start_of_file(file_index),
        }
    }
}

impl<I> CharStream<I>
where
    I: Iterator<Item = char>,
{
    pub fn peek(&mut self) -> Option<&char> {
        self.inner.peek()
    }

    pub fn next_skip_ws(&mut self) -> Option<char> {
        self.by_ref().find(|c| !c.is_whitespace())
    }
}

#[cfg(test)]
mod tests {
    const SEPARATOR_SRC: &str = "!$% &= ==? , ; ;;.: ::";
    const SYMBOL_SRC: &str = "foo+bar =baz_or$not";
    use super::*;

    fn unwrap_rich_tokens(xs: Vec<RichToken>) -> Vec<Token> {
        xs.into_iter().map(|t| t.inner).collect()
    }

    #[test]
    fn lex_empty_str() {
        let lexer = Lexer::new("", 0);
        assert_eq!(lexer.lex().unwrap(), Vec::new());
    }

    #[test]
    fn lex_separators() {
        use crate::Token::*;
        let lexer = Lexer::new(SEPARATOR_SRC, 0);
        let tokens = unwrap_rich_tokens(lexer.lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Bang, Dollar, Percent, Ampersand, Equal, EqualEqual, Question, Comma,
                SemiColon, SemiSemi, Period, Colon, ColonColon,
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
        macro_rules! loc {
            ($row:literal, $col:literal) => {
                Loc {
                    file_index: 0,
                    row: $row,
                    col: $col,
                    #[cfg(feature = "loc_with_origin")]
                    origin: None,
                }
            };
        }
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
        let s = "\"\\\"This is a string!\\\", said klex.\"";
        let tokens = unwrap_rich_tokens(Lexer::new(s, 0).lex().unwrap());
        assert_eq!(
            tokens,
            vec![Token::Str("\"This is a string!\", said klex.".into())]
        );
    }

    #[test]
    fn lex_comments() {
        let src = "// line comment\nnext_line = yes /* inline comment */\n/* block\ncomment */";
        let tokens = unwrap_rich_tokens(Lexer::new(src, 0).lex().unwrap());
        assert_eq!(
            tokens,
            vec![
                Token::Comment(Comment::LineComment(" line comment".into())),
                Token::Sym("next_line".into()),
                Token::Equal,
                Token::Sym("yes".into()),
                Token::Comment(Comment::BlockComment(" inline comment ".into())),
                Token::Comment(Comment::BlockComment(" block\ncomment ".into())),
            ]
        )
    }
}
