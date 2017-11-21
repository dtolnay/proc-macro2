//! A "shim crate" intended to multiplex the `proc_macro` API on to stable Rust.
//!
//! Procedural macros in Rust operate over the upstream
//! `proc_macro::TokenStream` type. This type currently is quite conservative
//! and exposed no internal implementation details. Nightly compilers, however,
//! contain a much richer interface. This richer interface allows fine-grained
//! inspection of the token stream which avoids stringification/re-lexing and
//! also preserves span information.
//!
//! The upcoming APIs added to `proc_macro` upstream are the foundation for
//! productive procedural macros in the ecosystem. To help prepare the ecosystem
//! for using them this crate serves to both compile on stable and nightly and
//! mirrors the API-to-be. The intention is that procedural macros which switch
//! to use this crate will be trivially able to switch to the upstream
//! `proc_macro` crate once its API stabilizes.
//!
//! In the meantime this crate also has an `unstable` Cargo feature which
//! enables it to reimplement itself with the unstable API of `proc_macro`.
//! This'll allow immediate usage of the beneficial upstream API, particularly
//! around preserving span information.

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
    type IntoIter = TokenTreeIter;

    fn into_iter(self) -> TokenTreeIter {
        TokenTreeIter(self.0.into_iter())
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

#[doc(hidden)]
impl Default for Span {
    fn default() -> Span {
        Span(imp::Span::def_site())
    }
}

impl Span {
    pub fn call_site() -> Span {
        Span(imp::Span::call_site())
    }

    pub fn def_site() -> Span {
        Span(imp::Span::def_site())
    }
}

#[derive(Clone, Debug)]
pub struct TokenTree {
    pub span: Span,
    pub kind: TokenNode,
}

impl From<TokenNode> for TokenTree {
    fn from(kind: TokenNode) -> TokenTree {
        TokenTree { span: Span::default(), kind: kind }
    }
}

impl fmt::Display for TokenTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        TokenStream::from(self.clone()).fmt(f)
    }
}

#[derive(Clone, Debug)]
pub enum TokenNode {
    Group(Delimiter, TokenStream),
    Term(Term),
    Op(char, Spacing),
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
pub struct Term(imp::Term);

impl Term {
    pub fn intern(string: &str) -> Term {
        Term(string.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Spacing {
    Alone,
    Joint,
}

#[derive(Clone)]
pub struct Literal(imp::Literal);

macro_rules! int_literals {
    ($($kind:ident,)*) => ($(
        pub fn $kind(n: $kind) -> Literal {
            Literal(n.into())
        }
    )*)
}

impl Literal {
    pub fn integer(s: i64) -> Literal {
        Literal(imp::Literal::integer(s))
    }

    int_literals! {
        u8, u16, u32, u64, usize,
        i8, i16, i32, i64, isize,
    }

    pub fn float(f: f64) -> Literal {
        Literal(imp::Literal::float(f))
    }

    pub fn f64(f: f64) -> Literal {
        Literal(f.into())
    }

    pub fn f32(f: f32) -> Literal {
        Literal(f.into())
    }

    pub fn string(string: &str) -> Literal {
        Literal(string.into())
    }

    pub fn character(ch: char) -> Literal {
        Literal(ch.into())
    }

    pub fn byte_string(s: &[u8]) -> Literal {
        Literal(imp::Literal::byte_string(s))
    }

    // =======================================================================
    // Not present upstream in proc_macro yet

    pub fn byte_char(b: u8) -> Literal {
        Literal(imp::Literal::byte_char(b))
    }

    pub fn doccomment(s: &str) -> Literal {
        Literal(imp::Literal::doccomment(s))
    }

    pub fn raw_string(s: &str, pounds: usize) -> Literal {
        Literal(imp::Literal::raw_string(s, pounds))
    }

    pub fn raw_byte_string(s: &str, pounds: usize) -> Literal {
        Literal(imp::Literal::raw_byte_string(s, pounds))
    }
}

pub struct TokenTreeIter(imp::TokenTreeIter);

impl Iterator for TokenTreeIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        self.0.next()
    }
}

forward_fmt!(Debug for LexError);
forward_fmt!(Debug for Literal);
forward_fmt!(Debug for Span);
forward_fmt!(Debug for Term);
forward_fmt!(Debug for TokenTreeIter);
forward_fmt!(Debug for TokenStream);
forward_fmt!(Display for Literal);
forward_fmt!(Display for TokenStream);
