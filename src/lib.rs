extern crate proc_macro;

#[macro_use]
extern crate synom;

use std::fmt;
use std::ops;
use std::str::FromStr;
use std::iter::FromIterator;

#[path = "stable.rs"]
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

impl From<TokenKind> for TokenStream {
    fn from(kind: TokenKind) -> TokenStream {
        TokenTree::from(kind).into()
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

impl From<TokenKind> for TokenTree {
    fn from(kind: TokenKind) -> TokenTree {
        TokenTree {
            span: Span::default(),
            kind: kind,
        }
    }
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

impl ops::Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &str {
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
    pub fn bytestring(s: &[u8]) -> Literal {
        Literal(imp::Literal::bytestring(s))
    }
}

macro_rules! tys {
    ($($t:ty,)*) => {$(
        impl<'a> From<$t> for Literal {
            fn from(t: $t) -> Literal {
                Literal(t.into())
            }
        }
    )*}
}

tys! {
    u8, u16, u32, u64, usize,
    i8, i16, i32, i64, isize,
    f32, f64, char, &'a str, bool,
}

pub struct TokenIter(imp::TokenIter);

impl Iterator for TokenIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        self.0.next()
    }
}
