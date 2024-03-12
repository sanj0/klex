[![Rust](https://github.com/sanj0/klex/actions/workflows/rust.yml/badge.svg)](https://github.com/sanj0/klex/actions/workflows/rust.yml)

# klex

klex (phonetically equal to "Klecks" in German, meaning splatter) is a dumb,
simple lexer for my personal projects. It doesn't support much customizing and
tokenizes rather opinionated. That is e. g. "\_hello" is a single symbol token.
It also lexes eagerly, meaning ":::" is tokenizes as [ColonColon, Colon]

# Usage

Add `klex` as a dependency:

Cargo.toml:
```toml
[dependencies]
klex = { git = "https://www.github.com/sanj0/klex.git", branch = "main" }
```

Use `klex` to lex some code:

```rust
use klex::{Lexer, Token}
let tokens = Lexer::new("greet = \"Hello, \" + name").lex()?;
// Extract the inner token, discarding location information
let inner_tokens: Vec<_> = tokens.into_iter().map(|t| t.inner).collect();
assert_eq!(
    inner_tokens,
    vec![
        Token::Sym("greet".into()),
        Token::Equal,
        Token::Str("Hello, ".into()),
        Token::Plus,
        Token::Sym("name".into()),
    ])
```

As `Lexer` is an iterator, `RichToken`s can be streamed:

```rust
let lexer = Lexer::new("source code");
for t in lexer {
    match t {
        Ok(t) => (),// do something
        Err(e) => (),// do something
    }
}
```

Additionally, a Lexer can be instantiated with any `Iterator<Item = char>`:

```rust
let iter = ['a', '=', 'b'].into_iter();
let lexer = Lexer::from_iter(iter, 0);
for token in lexer.filter_map(|res| res.ok().map(|rt| rt.inner)) {
    println!("{}", token.spelling());
}
```

# Tokens

The Lexer returns a `Vec` of `RichToken`s which are a `Token` alongside a `Loc`
and a `len`.
The `Token` is one of the following:

- `Sym(String)`
- `Num(String)`
- `Str(String)`
- `Bang`, `Ampersand` etc; one of `[! $ % & = == ? ' , ; . : ::]`
- `Slash`, `Aster` etc; one of `[/ // /= * ** *= + ++ += - -- -= < <= > >=]`
- `LBrace`, `RBrace` etc; one of `[{ } [ ] ( )]`

The `Loc` consists of a `file_index`, a `row` and a `col`. The `len` is the
length of token in characters.

