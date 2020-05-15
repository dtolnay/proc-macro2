//! Adapted from [`nom`](https://github.com/Geal/nom).

use crate::fallback::LexError;
use std::str::{Bytes, CharIndices, Chars};
use unicode_xid::UnicodeXID;

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) struct Cursor<'a> {
    pub rest: &'a str,
    #[cfg(span_locations)]
    pub off: u32,
}

impl<'a> Cursor<'a> {
    #[cfg(not(span_locations))]
    pub fn advance(&self, amt: usize) -> Cursor<'a> {
        Cursor {
            rest: &self.rest[amt..],
        }
    }
    #[cfg(span_locations)]
    pub fn advance(&self, amt: usize) -> Cursor<'a> {
        Cursor {
            rest: &self.rest[amt..],
            off: self.off + (amt as u32),
        }
    }

    pub fn find(&self, p: char) -> Option<usize> {
        self.rest.find(p)
    }

    pub fn starts_with(&self, s: &str) -> bool {
        self.rest.starts_with(s)
    }

    pub fn is_empty(&self) -> bool {
        self.rest.is_empty()
    }

    pub fn len(&self) -> usize {
        self.rest.len()
    }

    pub fn as_bytes(&self) -> &'a [u8] {
        self.rest.as_bytes()
    }

    pub fn bytes(&self) -> Bytes<'a> {
        self.rest.bytes()
    }

    pub fn chars(&self) -> Chars<'a> {
        self.rest.chars()
    }

    pub fn char_indices(&self) -> CharIndices<'a> {
        self.rest.char_indices()
    }

    pub fn expect(&self, tag: &str) -> Result<Cursor<'a>, LexError> {
        if self.starts_with(tag) {
            Ok(self.advance(tag.len()))
        } else {
            Err(LexError)
        }
    }
}

pub(crate) type PResult<'a, O> = Result<(Cursor<'a>, O), LexError>;

pub(crate) fn whitespace(input: Cursor) -> PResult<()> {
    if input.is_empty() {
        return Err(LexError);
    }

    let bytes = input.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let s = input.advance(i);
        if bytes[i] == b'/' {
            if s.starts_with("//")
                && (!s.starts_with("///") || s.starts_with("////"))
                && !s.starts_with("//!")
            {
                if let Some(len) = s.find('\n') {
                    i += len + 1;
                    continue;
                }
                break;
            } else if s.starts_with("/**/") {
                i += 4;
                continue;
            } else if s.starts_with("/*")
                && (!s.starts_with("/**") || s.starts_with("/***"))
                && !s.starts_with("/*!")
            {
                let (_, com) = block_comment(s)?;
                i += com.len();
                continue;
            }
        }
        match bytes[i] {
            b' ' | 0x09..=0x0d => {
                i += 1;
                continue;
            }
            b if b <= 0x7f => {}
            _ => {
                let ch = s.chars().next().unwrap();
                if is_whitespace(ch) {
                    i += ch.len_utf8();
                    continue;
                }
            }
        }
        return if i > 0 { Ok((s, ())) } else { Err(LexError) };
    }
    Ok((input.advance(input.len()), ()))
}

pub(crate) fn block_comment(input: Cursor) -> PResult<&str> {
    if !input.starts_with("/*") {
        return Err(LexError);
    }

    let mut depth = 0;
    let bytes = input.as_bytes();
    let mut i = 0;
    let upper = bytes.len() - 1;
    while i < upper {
        if bytes[i] == b'/' && bytes[i + 1] == b'*' {
            depth += 1;
            i += 1; // eat '*'
        } else if bytes[i] == b'*' && bytes[i + 1] == b'/' {
            depth -= 1;
            if depth == 0 {
                return Ok((input.advance(i + 2), &input.rest[..i + 2]));
            }
            i += 1; // eat '/'
        }
        i += 1;
    }
    Err(LexError)
}

pub(crate) fn skip_whitespace(input: Cursor) -> Cursor {
    match whitespace(input) {
        Ok((rest, _)) => rest,
        Err(LexError) => input,
    }
}

fn is_whitespace(ch: char) -> bool {
    // Rust treats left-to-right mark and right-to-left mark as whitespace
    ch.is_whitespace() || ch == '\u{200e}' || ch == '\u{200f}'
}

pub(crate) fn word_break(input: Cursor) -> PResult<()> {
    match input.chars().next() {
        Some(ch) if UnicodeXID::is_xid_continue(ch) => Err(LexError),
        Some(_) | None => Ok((input, ())),
    }
}
