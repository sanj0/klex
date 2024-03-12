use std::io::{stdin, stdout, Write};

use klex::*;

fn main() -> Result<(), KlexError> {
    let mut input = String::new();

    loop {
        print!(">>>");
        stdout().flush().expect("stdout error");
        stdin().read_line(&mut input).expect("stdin error");

        let tokens = Lexer::new(&input, 0).lex()?;
        println!("{}", tokens.iter().map(|t| t.inner.spelling()).collect::<String>());
        input.clear();
    }
}
