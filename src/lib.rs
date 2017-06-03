#![cfg_attr(feature = "unstable", feature(proc_macro))]

extern crate proc_macro;

#[cfg(not(feature = "unstable"))]
extern crate unicode_xid;

use std::fmt;
use std::str::FromStr;
use std::iter::FromIterator;

#[macro_use]
#[cfg(not(feature = "unstable"))]
mod strnom;

#[path = "stable.rs"]
#[cfg(not(feature = "unstable"))]
mod imp;
#[path = "unstable.rs"]
#[cfg(feature = "unstable")]
mod imp;

#[macro_use]
mod macros;

#[derive(Clone)]
pub struct TokenStream(imp::TokenStream);

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

#[derive(Clone, Debug)]
pub struct TokenTree {
    pub span: Span,
    pub kind: TokenKind,
}

impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        TokenStream::from(self.clone()).fmt(f)
    }
}

#[derive(Clone, Debug)]
pub enum TokenKind {
    Sequence(Delimiter, TokenStream),
    Word(Symbol),
    Op(char, OpKind),
    Literal(Literal),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum OpKind {
    Alone,
    Joint,
}

#[derive(Clone)]
pub struct Literal(imp::Literal);

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

    pub fn raw_string(s: &str, pounds: usize) -> Literal {
        Literal(imp::Literal::raw_string(s, pounds))
    }

    pub fn raw_byte_string(s: &str, pounds: usize) -> Literal {
        Literal(imp::Literal::raw_byte_string(s, pounds))
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

forward_fmt!(Debug for LexError);
forward_fmt!(Debug for Literal);
forward_fmt!(Debug for Span);
forward_fmt!(Debug for Symbol);
forward_fmt!(Debug for TokenIter);
forward_fmt!(Debug for TokenStream);
forward_fmt!(Display for Literal);
forward_fmt!(Display for TokenStream);
