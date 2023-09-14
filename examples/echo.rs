use std::io::{stdin, stdout, Write};

use klex::{KlexError, Lexer};

fn main() -> Result<(), KlexError> {
    let mut input = String::new();
    loop {
        print!(">>");
        let _ = stdout().flush();
        let _ = stdin().read_line(&mut input);
        let tokens = Lexer::new(&input, 0).lex()?;
        let tokens: Vec<_> = tokens.into_iter().map(|t| t.inner).collect();
        println!("{tokens:?}");
        input.clear();
    }
}
