#![allow(dead_code)]

use std::ascii;
use std::fmt;
use std::iter;
use std::str::FromStr;

use proc_macro;

use {Delimiter, Group, Op, Spacing, TokenTree};

#[derive(Clone)]
pub struct TokenStream(proc_macro::TokenStream);

pub struct LexError(proc_macro::LexError);

impl TokenStream {
    pub fn empty() -> TokenStream {
        TokenStream(proc_macro::TokenStream::empty())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(src: &str) -> Result<TokenStream, LexError> {
        Ok(TokenStream(src.parse().map_err(LexError)?))
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl From<proc_macro::TokenStream> for TokenStream {
    fn from(inner: proc_macro::TokenStream) -> TokenStream {
        TokenStream(inner)
    }
}

impl From<TokenStream> for proc_macro::TokenStream {
    fn from(inner: TokenStream) -> proc_macro::TokenStream {
        inner.0
    }
}

impl From<TokenTree> for TokenStream {
    fn from(token: TokenTree) -> TokenStream {
        let (span, kind) = match token {
            TokenTree::Group(tt) => {
                let delim = match tt.delimiter() {
                    Delimiter::Parenthesis => proc_macro::Delimiter::Parenthesis,
                    Delimiter::Bracket => proc_macro::Delimiter::Bracket,
                    Delimiter::Brace => proc_macro::Delimiter::Brace,
                    Delimiter::None => proc_macro::Delimiter::None,
                };
                let span = tt.span();
                let group = proc_macro::TokenNode::Group(delim, tt.stream.inner.0);
                (span, group)
            }
            TokenTree::Op(tt) => {
                let kind = match tt.spacing() {
                    Spacing::Joint => proc_macro::Spacing::Joint,
                    Spacing::Alone => proc_macro::Spacing::Alone,
                };
                (tt.span(), proc_macro::TokenNode::Op(tt.op(), kind))
            }
            TokenTree::Term(tt) => (tt.span(), proc_macro::TokenNode::Term(tt.inner.0)),
            TokenTree::Literal(tt) => (tt.span(), proc_macro::TokenNode::Literal(tt.inner.0)),
        };
        TokenStream(
            proc_macro::TokenTree {
                span: span.inner.0,
                kind,
            }.into(),
        )
    }
}

impl iter::FromIterator<TokenTree> for TokenStream {
    fn from_iter<I: IntoIterator<Item = TokenTree>>(streams: I) -> Self {
        let streams = streams.into_iter().map(TokenStream::from);
        TokenStream(streams.collect::<proc_macro::TokenStream>())
    }
}

impl fmt::Debug for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct TokenTreeIter(proc_macro::TokenTreeIter);

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = TokenTreeIter;

    fn into_iter(self) -> TokenTreeIter {
        TokenTreeIter(self.0.into_iter())
    }
}

impl Iterator for TokenTreeIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        let token = self.0.next()?;
        let span = ::Span::_new(Span(token.span));
        Some(match token.kind {
            proc_macro::TokenNode::Group(delim, s) => {
                let delim = match delim {
                    proc_macro::Delimiter::Parenthesis => Delimiter::Parenthesis,
                    proc_macro::Delimiter::Bracket => Delimiter::Bracket,
                    proc_macro::Delimiter::Brace => Delimiter::Brace,
                    proc_macro::Delimiter::None => Delimiter::None,
                };
                let stream = ::TokenStream::_new(TokenStream(s));
                let mut g = Group::new(delim, stream);
                g.set_span(span);
                g.into()
            }
            proc_macro::TokenNode::Op(ch, kind) => {
                let kind = match kind {
                    proc_macro::Spacing::Joint => Spacing::Joint,
                    proc_macro::Spacing::Alone => Spacing::Alone,
                };
                let mut o = Op::new(ch, kind);
                o.span = span;
                o.into()
            }
            proc_macro::TokenNode::Term(s) => ::Term::_new(Term(s), span).into(),
            proc_macro::TokenNode::Literal(l) => {
                let mut l = ::Literal::_new(Literal(l));
                l.span = span;
                l.into()
            }
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl fmt::Debug for TokenTreeIter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("TokenTreeIter").finish()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct FileName(String);

impl fmt::Display for FileName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

// NOTE: We have to generate our own filename object here because we can't wrap
// the one provided by proc_macro.
#[derive(Clone, PartialEq, Eq)]
pub struct SourceFile(proc_macro::SourceFile, FileName);

impl SourceFile {
    fn new(sf: proc_macro::SourceFile) -> Self {
        let filename = FileName(sf.path().to_string());
        SourceFile(sf, filename)
    }

    /// Get the path to this source file as a string.
    pub fn path(&self) -> &FileName {
        &self.1
    }

    pub fn is_real(&self) -> bool {
        self.0.is_real()
    }
}

impl AsRef<FileName> for SourceFile {
    fn as_ref(&self) -> &FileName {
        self.path()
    }
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Span(proc_macro::Span);

impl From<proc_macro::Span> for ::Span {
    fn from(proc_span: proc_macro::Span) -> ::Span {
        ::Span::_new(Span(proc_span))
    }
}

impl Span {
    pub fn call_site() -> Span {
        Span(proc_macro::Span::call_site())
    }

    pub fn def_site() -> Span {
        Span(proc_macro::Span::def_site())
    }

    pub fn resolved_at(&self, other: Span) -> Span {
        Span(self.0.resolved_at(other.0))
    }

    pub fn located_at(&self, other: Span) -> Span {
        Span(self.0.located_at(other.0))
    }

    pub fn unstable(self) -> proc_macro::Span {
        self.0
    }

    pub fn source_file(&self) -> SourceFile {
        SourceFile::new(self.0.source_file())
    }

    pub fn start(&self) -> LineColumn {
        let proc_macro::LineColumn { line, column } = self.0.start();
        LineColumn { line, column }
    }

    pub fn end(&self) -> LineColumn {
        let proc_macro::LineColumn { line, column } = self.0.end();
        LineColumn { line, column }
    }

    pub fn join(&self, other: Span) -> Option<Span> {
        self.0.join(other.0).map(Span)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Copy, Clone)]
pub struct Term(proc_macro::Term);

impl Term {
    pub fn intern(string: &str) -> Term {
        Term(proc_macro::Term::intern(string))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Debug for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone)]
pub struct Literal(proc_macro::Literal);

impl Literal {
    pub fn byte_char(byte: u8) -> Literal {
        match byte {
            0 => Literal(to_literal("b'\\0'")),
            b'\"' => Literal(to_literal("b'\"'")),
            n => {
                let mut escaped = "b'".to_string();
                escaped.extend(ascii::escape_default(n).map(|c| c as char));
                escaped.push('\'');
                Literal(to_literal(&escaped))
            }
        }
    }

    pub fn byte_string(bytes: &[u8]) -> Literal {
        Literal(proc_macro::Literal::byte_string(bytes))
    }

    pub fn doccomment(s: &str) -> Literal {
        Literal(to_literal(s))
    }

    pub fn float(s: f64) -> Literal {
        Literal(proc_macro::Literal::float(s))
    }

    pub fn integer(s: i64) -> Literal {
        Literal(proc_macro::Literal::integer(s.into()))
    }

    pub fn raw_string(s: &str, pounds: usize) -> Literal {
        let mut ret = format!("r");
        ret.extend((0..pounds).map(|_| "#"));
        ret.push('"');
        ret.push_str(s);
        ret.push('"');
        ret.extend((0..pounds).map(|_| "#"));
        Literal(to_literal(&ret))
    }

    pub fn raw_byte_string(s: &str, pounds: usize) -> Literal {
        let mut ret = format!("br");
        ret.extend((0..pounds).map(|_| "#"));
        ret.push('"');
        ret.push_str(s);
        ret.push('"');
        ret.extend((0..pounds).map(|_| "#"));
        Literal(to_literal(&ret))
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

fn to_literal(s: &str) -> proc_macro::Literal {
    let stream = s.parse::<proc_macro::TokenStream>().unwrap();
    match stream.into_iter().next().unwrap().kind {
        proc_macro::TokenNode::Literal(l) => l,
        _ => unreachable!(),
    }
}

macro_rules! ints {
    ($($t:ident,)*) => {$(
        impl From<$t> for Literal {
            fn from(t: $t) -> Literal {
                Literal(proc_macro::Literal::$t(t))
            }
        }
    )*}
}

ints! {
    u8, u16, u32, u64, usize,
    i8, i16, i32, i64, isize,
}

macro_rules! floats {
    ($($t:ident,)*) => {$(
        impl From<$t> for Literal {
            fn from(t: $t) -> Literal {
                Literal(proc_macro::Literal::$t(t))
            }
        }
    )*}
}

floats! {
    f32, f64,
}

impl<'a> From<&'a str> for Literal {
    fn from(t: &'a str) -> Literal {
        Literal(proc_macro::Literal::string(t))
    }
}

impl From<char> for Literal {
    fn from(t: char) -> Literal {
        Literal(proc_macro::Literal::character(t))
    }
}
