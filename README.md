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
let token = Lexer::new("greet = \"Hello, \" + name").lex()?;
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

