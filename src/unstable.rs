use std::ascii;
use std::fmt;
use std::iter;
use std::ops;
use std::str::FromStr;

use proc_macro;

use {TokenTree, TokenKind, Delimiter, OpKind};

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
    fn from(tree: TokenTree) -> TokenStream {
        TokenStream(proc_macro::TokenTree {
            span: (tree.span.0).0,
            kind: match tree.kind {
                TokenKind::Sequence(delim, s) => {
                    let delim = match delim {
                        Delimiter::Parenthesis => proc_macro::Delimiter::Parenthesis,
                        Delimiter::Bracket => proc_macro::Delimiter::Bracket,
                        Delimiter::Brace => proc_macro::Delimiter::Brace,
                        Delimiter::None => proc_macro::Delimiter::None,
                    };
                    proc_macro::TokenKind::Sequence(delim, (s.0).0)
                }
                TokenKind::Op(ch, kind) => {
                    let kind = match kind {
                        OpKind::Joint => proc_macro::OpKind::Joint,
                        OpKind::Alone => proc_macro::OpKind::Alone,
                    };
                    proc_macro::TokenKind::Op(ch, kind)
                }
                TokenKind::Word(s) => {
                    proc_macro::TokenKind::Word((s.0).0)
                }
                TokenKind::Literal(l) => {
                    proc_macro::TokenKind::Literal((l.0).0)
                }
            },
        }.into())
    }
}

impl iter::FromIterator<TokenStream> for TokenStream {
    fn from_iter<I: IntoIterator<Item=TokenStream>>(streams: I) -> Self {
        let streams = streams.into_iter().map(|s| s.0);
        TokenStream(streams.collect::<proc_macro::TokenStream>())
    }
}

impl fmt::Debug for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("TokenStream")
         .field("tts", &self.clone().into_iter().collect::<Vec<_>>())
         .finish()
    }
}

impl fmt::Debug for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("LexError").finish()
    }
}

pub struct TokenIter(proc_macro::TokenIter);

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = TokenIter;

    fn into_iter(self) -> TokenIter {
        TokenIter(self.0.into_iter())
    }
}

impl Iterator for TokenIter {
    type Item = TokenTree;

    fn next(&mut self) -> Option<TokenTree> {
        let token = match self.0.next() {
            Some(n) => n,
            None => return None,
        };
        Some(TokenTree {
            span: ::Span(Span(token.span)),
            kind: match token.kind {
                proc_macro::TokenKind::Sequence(delim, s) => {
                    let delim = match delim {
                        proc_macro::Delimiter::Parenthesis => Delimiter::Parenthesis,
                        proc_macro::Delimiter::Bracket => Delimiter::Bracket,
                        proc_macro::Delimiter::Brace => Delimiter::Brace,
                        proc_macro::Delimiter::None => Delimiter::None,
                    };
                    TokenKind::Sequence(delim, ::TokenStream(TokenStream(s)))
                }
                proc_macro::TokenKind::Op(ch, kind) => {
                    let kind = match kind {
                        proc_macro::OpKind::Joint => OpKind::Joint,
                        proc_macro::OpKind::Alone => OpKind::Alone,
                    };
                    TokenKind::Op(ch, kind)
                }
                proc_macro::TokenKind::Word(s) => {
                    TokenKind::Word(::Symbol(Symbol(s)))
                }
                proc_macro::TokenKind::Literal(l) => {
                    TokenKind::Literal(::Literal(Literal(l)))
                }
            },
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl fmt::Debug for TokenIter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("TokenIter").finish()
    }
}

#[derive(Copy, Clone, Default)]
pub struct Span(proc_macro::Span);

impl Span {
    pub fn call_site() -> Span {
        Span(proc_macro::Span::call_site())
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Span")
         .finish()
    }
}

#[derive(Copy, Clone)]
pub struct Symbol(proc_macro::Symbol);

impl<'a> From<&'a str> for Symbol {
    fn from(string: &'a str) -> Symbol {
        Symbol(string.into())
    }
}

impl ops::Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt(f)
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
        let mut escaped = "b\"".to_string();
        for b in bytes {
            match *b {
                b'\0' => escaped.push_str(r"\0"),
                b'\t' => escaped.push_str(r"\t"),
                b'\n' => escaped.push_str(r"\n"),
                b'\r' => escaped.push_str(r"\r"),
                b'"' => escaped.push_str("\\\""),
                b'\\' => escaped.push_str("\\\\"),
                b'\x20' ... b'\x7E' => escaped.push(*b as char),
                _ => escaped.push_str(&format!("\\x{:02X}", b)),
            }
        }
        escaped.push('"');
        Literal(to_literal(&escaped))
    }

    pub fn doccomment(s: &str) -> Literal {
        Literal(to_literal(s))
    }

    pub fn float(s: &str) -> Literal {
        Literal(to_literal(s))
    }

    pub fn integer(s: &str) -> Literal {
        Literal(to_literal(s))
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
        fmt::Display::fmt(self, f)
    }
}

fn to_literal(s: &str) -> proc_macro::Literal {
    let stream = s.parse::<proc_macro::TokenStream>().unwrap();
    match stream.into_iter().next().unwrap().kind {
        proc_macro::TokenKind::Literal(l) => l,
        _ => unreachable!(),
    }
}

macro_rules! ints {
    ($($t:ty,)*) => {$(
        impl From<$t> for Literal {
            fn from(t: $t) -> Literal {
                Literal(to_literal(&format!(concat!("{}", stringify!($t)), t)))
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
                // TODO: remove this `as f32` when fixed upstream
                Literal(proc_macro::Literal::$t(t as f32))
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
