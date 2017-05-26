extern crate proc_macro;

#[macro_use]
#[cfg(not(feature = "unstable"))]
extern crate synom;

use std::fmt;
use std::str::FromStr;
use std::iter::FromIterator;

#[path = "stable.rs"]
#[cfg(not(feature = "unstable"))]
mod imp;
#[path = "unstable.rs"]
#[cfg(feature = "unstable")]
mod imp;

#[derive(Clone)]
pub struct TokenStream(imp::TokenStream);

#[derive(Debug)]
pub struct LexError(imp::LexError);

impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(src: &str) -> Result<TokenStream, LexError> {
        match src.parse() {
            Ok(e) => Ok(TokenStream(e)),
            Err(e) => Err(LexError(e)),
        }
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<proc_macro::TokenStream> for TokenStream {
    fn from(inner: proc_macro::TokenStream) -> TokenStream {
        TokenStream(inner.into())
    }
}

impl From<TokenStream> for proc_macro::TokenStream {
    fn from(inner: TokenStream) -> proc_macro::TokenStream {
        inner.0.into()
    }
}

impl From<TokenTree> for TokenStream {
    fn from(tree: TokenTree) -> TokenStream {
        TokenStream(tree.into())
    }
}

impl<T: Into<TokenStream>> FromIterator<T> for TokenStream {
    fn from_iter<I: IntoIterator<Item = T>>(streams: I) -> Self {
        TokenStream(streams.into_iter().map(|t| t.into().0).collect())
    }
}

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = TokenIter;

    fn into_iter(self) -> TokenIter {
        TokenIter(self.0.into_iter())
    }
}

impl TokenStream {
    pub fn empty() -> TokenStream {
        TokenStream(imp::TokenStream::empty())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Copy, Clone)]
pub struct Span(imp::Span);

impl Default for Span {
    fn default() -> Span {
        Span(imp::Span::default())
    }
}

impl Span {
    pub fn call_site() -> Span {
        Span(imp::Span::call_site())
    }
}

#[derive(Clone)]
pub struct TokenTree {
    pub span: Span,
    pub kind: TokenKind,
}

impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        TokenStream::from(self.clone()).fmt(f)
    }
}

#[derive(Clone)]
pub enum TokenKind {
    Sequence(Delimiter, TokenStream),
    Word(Symbol),
    Op(char, OpKind),
    Literal(Literal),
}

#[derive(Copy, Clone)]
pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
    None,
}

#[derive(Copy, Clone)]
pub struct Symbol(imp::Symbol);

impl<'a> From<&'a str> for Symbol {
    fn from(string: &'a str) -> Symbol {
        Symbol(string.into())
    }
}

impl From<String> for Symbol {
    fn from(string: String) -> Symbol {
        Symbol(string[..].into())
    }
}

impl Symbol {
    fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Copy, Clone)]
pub enum OpKind {
    Alone,
    Joint,
}

#[derive(Clone)]
pub struct Literal(imp::Literal);

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Literal {
    pub fn byte_char(b: u8) -> Literal {
        Literal(imp::Literal::byte_char(b))
    }

    pub fn byte_string(s: &[u8]) -> Literal {
        Literal(imp::Literal::byte_string(s))
    }

    pub fn doccomment(s: &str) -> Literal {
        Literal(imp::Literal::doccomment(s))
    }

    pub fn float(s: &str) -> Literal {
        Literal(imp::Literal::float(s))
    }

    pub fn integer(s: &str) -> Literal {
        Literal(imp::Literal::integer(s))
    }
}

macro_rules! froms {
    ($($t:ty,)*) => {$(
        impl<'a> From<$t> for Literal {
            fn from(t: $t) -> Literal {
                Literal(t.into())
            }
        }
    )*}
}

froms! {
    u8, u16, u32, u64, usize,
    i8, i16, i32, i64, isize,
    f32, f64, char, &'a str,
}

pub struct TokenIter(imp::TokenIter);

impl Iterator for TokenIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        self.0.next()
    }
}
