use crate::fallback::{
    is_ident_continue, is_ident_start, Group, LexError, Literal, Span, TokenStream,
};
use crate::{Delimiter, Punct, Spacing, TokenTree};
use std::str::{Bytes, CharIndices, Chars};
use unicode_xid::UnicodeXID;

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) struct Cursor<'a> {
    pub rest: &'a str,
    #[cfg(span_locations)]
    pub off: u32,
}

impl<'a> Cursor<'a> {
    fn advance(&self, bytes: usize) -> Cursor<'a> {
        let (_front, rest) = self.rest.split_at(bytes);
        Cursor {
            rest,
            #[cfg(span_locations)]
            off: self.off + _front.chars().count() as u32,
        }
    }

    fn starts_with(&self, s: &str) -> bool {
        self.rest.starts_with(s)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.rest.is_empty()
    }

    fn len(&self) -> usize {
        self.rest.len()
    }

    fn as_bytes(&self) -> &'a [u8] {
        self.rest.as_bytes()
    }

    fn bytes(&self) -> Bytes<'a> {
        self.rest.bytes()
    }

    fn chars(&self) -> Chars<'a> {
        self.rest.chars()
    }

    fn char_indices(&self) -> CharIndices<'a> {
        self.rest.char_indices()
    }

    fn parse(&self, tag: &str) -> Result<Cursor<'a>, LexError> {
        if self.starts_with(tag) {
            Ok(self.advance(tag.len()))
        } else {
            Err(LexError)
        }
    }
}

type PResult<'a, O> = Result<(Cursor<'a>, O), LexError>;

fn skip_whitespace(input: Cursor) -> Cursor {
    let mut s = input;

    while !s.is_empty() {
        let byte = s.as_bytes()[0];
        if byte == b'/' {
            if s.starts_with("//")
                && (!s.starts_with("///") || s.starts_with("////"))
                && !s.starts_with("//!")
            {
                let (cursor, _) = take_until_newline_or_eof(s);
                s = cursor;
                continue;
            } else if s.starts_with("/**/") {
                s = s.advance(4);
                continue;
            } else if s.starts_with("/*")
                && (!s.starts_with("/**") || s.starts_with("/***"))
                && !s.starts_with("/*!")
            {
                match block_comment(s) {
                    Ok((rest, _)) => {
                        s = rest;
                        continue;
                    }
                    Err(LexError) => return input,
                }
            }
        }
        match byte {
            b' ' | 0x09..=0x0d => {
                s = s.advance(1);
                continue;
            }
            b if b <= 0x7f => {}
            _ => {
                let ch = s.chars().next().unwrap();
                if is_whitespace(ch) {
                    s = s.advance(ch.len_utf8());
                    continue;
                }
            }
        }
        return s;
    }
    s
}

fn block_comment(input: Cursor) -> PResult<&str> {
    if !input.starts_with("/*") {
        return Err(LexError);
    }

    let mut depth = 0;
    let bytes = input.as_bytes();
    let mut iter = input.char_indices();
    let upper = bytes.len() - 1;

    while let Some((i, _)) = iter.next() {
        if i == upper {
            break;
        }
        if bytes[i] == b'/' && bytes[i + 1] == b'*' {
            depth += 1;
            // eat '*'
            let _ = iter.next();
        } else if bytes[i] == b'*' && bytes[i + 1] == b'/' {
            depth -= 1;
            if depth == 0 {
                return Ok((input.advance(i + 2), &input.rest[..i + 2]));
            }
            // eat '/'
            let _ = iter.next();
        }
    }

    Err(LexError)
}

fn is_whitespace(ch: char) -> bool {
    // Rust treats left-to-right mark and right-to-left mark as whitespace
    ch.is_whitespace() || ch == '\u{200e}' || ch == '\u{200f}'
}

fn word_break(input: Cursor) -> Result<Cursor, LexError> {
    match input.chars().next() {
        Some(ch) if UnicodeXID::is_xid_continue(ch) => Err(LexError),
        Some(_) | None => Ok(input),
    }
}

pub(crate) fn token_stream(mut input: Cursor) -> PResult<TokenStream> {
    let mut trees = Vec::new();
    loop {
        input = skip_whitespace(input);
        match doc_comment(input) {
            Ok((a, tt)) => {
                trees.extend(tt);
                input = a;
            }
            Err(_) => match token_tree(input) {
                Ok((a, tt)) => {
                    trees.push(tt);
                    input = a;
                }
                Err(_) => break,
            },
        }
    }
    Ok((input, TokenStream { inner: trees }))
}

#[cfg(not(span_locations))]
fn spanned<'a, T>(
    input: Cursor<'a>,
    f: fn(Cursor<'a>) -> PResult<'a, T>,
) -> PResult<'a, (T, crate::Span)> {
    let (a, b) = f(input)?;
    Ok((a, ((b, crate::Span::_new_stable(Span::call_site())))))
}

#[cfg(span_locations)]
fn spanned<'a, T>(
    input: Cursor<'a>,
    f: fn(Cursor<'a>) -> PResult<'a, T>,
) -> PResult<'a, (T, crate::Span)> {
    let lo = input.off;
    let (a, b) = f(input)?;
    let hi = a.off;
    let span = crate::Span::_new_stable(Span { lo, hi });
    Ok((a, (b, span)))
}

fn token_tree(input: Cursor) -> PResult<TokenTree> {
    let (rest, (mut tt, span)) = spanned(input, token_kind)?;
    tt.set_span(span);
    Ok((rest, tt))
}

fn token_kind(input: Cursor) -> PResult<TokenTree> {
    if let Ok((input, g)) = group(input) {
        Ok((input, TokenTree::Group(crate::Group::_new_stable(g))))
    } else if let Ok((input, l)) = literal(input) {
        // must be parsed before symbol
        Ok((input, TokenTree::Literal(crate::Literal::_new_stable(l))))
    } else if let Ok((input, p)) = op(input) {
        Ok((input, TokenTree::Punct(p)))
    } else {
        symbol(input)
    }
}

fn group(input: Cursor) -> PResult<Group> {
    let (delimiter, close) = if input.starts_with("(") {
        (Delimiter::Parenthesis, ")")
    } else if input.starts_with("[") {
        (Delimiter::Bracket, "]")
    } else if input.starts_with("{") {
        (Delimiter::Brace, "}")
    } else {
        return Err(LexError);
    };

    let input = input.advance(1);
    let (input, ts) = token_stream(input)?;
    let input = skip_whitespace(input);
    let input = input.parse(close)?;
    Ok((input, Group::new(delimiter, ts)))
}

fn symbol(input: Cursor) -> PResult<TokenTree> {
    let raw = input.starts_with("r#");
    let rest = input.advance((raw as usize) << 1);

    let (rest, sym) = symbol_not_raw(rest)?;

    if !raw {
        let ident = crate::Ident::new(sym, crate::Span::call_site());
        return Ok((rest, ident.into()));
    }

    if sym == "_" {
        return Err(LexError);
    }

    let ident = crate::Ident::_new_raw(sym, crate::Span::call_site());
    Ok((rest, ident.into()))
}

fn symbol_not_raw(input: Cursor) -> PResult<&str> {
    let mut chars = input.char_indices();

    match chars.next() {
        Some((_, ch)) if is_ident_start(ch) => {}
        _ => return Err(LexError),
    }

    let mut end = input.len();
    for (i, ch) in chars {
        if !is_ident_continue(ch) {
            end = i;
            break;
        }
    }

    Ok((input.advance(end), &input.rest[..end]))
}

fn literal(input: Cursor) -> PResult<Literal> {
    match literal_nocapture(input) {
        Ok(a) => {
            let end = input.len() - a.len();
            Ok((a, Literal::_new(input.rest[..end].to_string())))
        }
        Err(LexError) => Err(LexError),
    }
}

fn literal_nocapture(input: Cursor) -> Result<Cursor, LexError> {
    if let Ok(ok) = string(input) {
        Ok(ok)
    } else if let Ok(ok) = byte_string(input) {
        Ok(ok)
    } else if let Ok(ok) = byte(input) {
        Ok(ok)
    } else if let Ok(ok) = character(input) {
        Ok(ok)
    } else if let Ok(ok) = float(input) {
        Ok(ok)
    } else if let Ok(ok) = int(input) {
        Ok(ok)
    } else {
        Err(LexError)
    }
}

fn literal_suffix(input: Cursor) -> Cursor {
    match symbol_not_raw(input) {
        Ok((input, _)) => input,
        Err(LexError) => input,
    }
}

fn string(input: Cursor) -> Result<Cursor, LexError> {
    if let Ok(input) = input.parse("\"") {
        cooked_string(input)
    } else if let Ok(input) = input.parse("r") {
        raw_string(input)
    } else {
        Err(LexError)
    }
}

fn cooked_string(input: Cursor) -> Result<Cursor, LexError> {
    let mut chars = input.char_indices().peekable();

    while let Some((byte_offset, ch)) = chars.next() {
        match ch {
            '"' => {
                let input = input.advance(byte_offset + 1);
                return Ok(literal_suffix(input));
            }
            '\r' => {
                if let Some((_, '\n')) = chars.next() {
                    // ...
                } else {
                    break;
                }
            }
            '\\' => match chars.next() {
                Some((_, 'x')) => {
                    if !backslash_x_char(&mut chars) {
                        break;
                    }
                }
                Some((_, 'n')) | Some((_, 'r')) | Some((_, 't')) | Some((_, '\\'))
                | Some((_, '\'')) | Some((_, '"')) | Some((_, '0')) => {}
                Some((_, 'u')) => {
                    if !backslash_u(&mut chars) {
                        break;
                    }
                }
                Some((_, '\n')) | Some((_, '\r')) => {
                    while let Some(&(_, ch)) = chars.peek() {
                        if ch.is_whitespace() {
                            chars.next();
                        } else {
                            break;
                        }
                    }
                }
                _ => break,
            },
            _ch => {}
        }
    }
    Err(LexError)
}

fn byte_string(input: Cursor) -> Result<Cursor, LexError> {
    if let Ok(input) = input.parse("b\"") {
        cooked_byte_string(input)
    } else if let Ok(input) = input.parse("br") {
        raw_string(input)
    } else {
        Err(LexError)
    }
}

fn cooked_byte_string(mut input: Cursor) -> Result<Cursor, LexError> {
    let mut bytes = input.bytes().enumerate();
    'outer: while let Some((offset, b)) = bytes.next() {
        match b {
            b'"' => {
                let input = input.advance(offset + 1);
                return Ok(literal_suffix(input));
            }
            b'\r' => {
                if let Some((_, b'\n')) = bytes.next() {
                    // ...
                } else {
                    break;
                }
            }
            b'\\' => match bytes.next() {
                Some((_, b'x')) => {
                    if !backslash_x_byte(&mut bytes) {
                        break;
                    }
                }
                Some((_, b'n')) | Some((_, b'r')) | Some((_, b't')) | Some((_, b'\\'))
                | Some((_, b'0')) | Some((_, b'\'')) | Some((_, b'"')) => {}
                Some((newline, b'\n')) | Some((newline, b'\r')) => {
                    let rest = input.advance(newline + 1);
                    for (offset, ch) in rest.char_indices() {
                        if !ch.is_whitespace() {
                            input = rest.advance(offset);
                            bytes = input.bytes().enumerate();
                            continue 'outer;
                        }
                    }
                    break;
                }
                _ => break,
            },
            b if b < 0x80 => {}
            _ => break,
        }
    }
    Err(LexError)
}

fn raw_string(input: Cursor) -> Result<Cursor, LexError> {
    let mut chars = input.char_indices();
    let mut n = 0;
    while let Some((byte_offset, ch)) = chars.next() {
        match ch {
            '"' => {
                n = byte_offset;
                break;
            }
            '#' => {}
            _ => return Err(LexError),
        }
    }
    for (byte_offset, ch) in chars {
        match ch {
            '"' if input.rest[(byte_offset + 1)..].starts_with(&input.rest[..n]) => {
                let rest = input.advance(byte_offset + 1 + n);
                return Ok(literal_suffix(rest));
            }
            '\r' => {}
            _ => {}
        }
    }
    Err(LexError)
}

fn byte(input: Cursor) -> Result<Cursor, LexError> {
    let input = input.parse("b'")?;
    let mut bytes = input.bytes().enumerate();
    let ok = match bytes.next().map(|(_, b)| b) {
        Some(b'\\') => match bytes.next().map(|(_, b)| b) {
            Some(b'x') => backslash_x_byte(&mut bytes),
            Some(b'n') | Some(b'r') | Some(b't') | Some(b'\\') | Some(b'0') | Some(b'\'')
            | Some(b'"') => true,
            _ => false,
        },
        b => b.is_some(),
    };
    if !ok {
        return Err(LexError);
    }
    let (offset, _) = bytes.next().ok_or(LexError)?;
    if !input.chars().as_str().is_char_boundary(offset) {
        return Err(LexError);
    }
    let input = input.advance(offset).parse("'")?;
    Ok(literal_suffix(input))
}

fn character(input: Cursor) -> Result<Cursor, LexError> {
    let input = input.parse("'")?;
    let mut chars = input.char_indices();
    let ok = match chars.next().map(|(_, ch)| ch) {
        Some('\\') => match chars.next().map(|(_, ch)| ch) {
            Some('x') => backslash_x_char(&mut chars),
            Some('u') => backslash_u(&mut chars),
            Some('n') | Some('r') | Some('t') | Some('\\') | Some('0') | Some('\'') | Some('"') => {
                true
            }
            _ => false,
        },
        ch => ch.is_some(),
    };
    if !ok {
        return Err(LexError);
    }
    let (idx, _) = chars.next().ok_or(LexError)?;
    let input = input.advance(idx).parse("'")?;
    Ok(literal_suffix(input))
}

macro_rules! next_ch {
    ($chars:ident @ $pat:pat $(| $rest:pat)*) => {
        match $chars.next() {
            Some((_, ch)) => match ch {
                $pat $(| $rest)* => ch,
                _ => return false,
            },
            None => return false,
        }
    };
}

fn backslash_x_char<I>(chars: &mut I) -> bool
where
    I: Iterator<Item = (usize, char)>,
{
    next_ch!(chars @ '0'..='7');
    next_ch!(chars @ '0'..='9' | 'a'..='f' | 'A'..='F');
    true
}

fn backslash_x_byte<I>(chars: &mut I) -> bool
where
    I: Iterator<Item = (usize, u8)>,
{
    next_ch!(chars @ b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F');
    next_ch!(chars @ b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F');
    true
}

fn backslash_u<I>(chars: &mut I) -> bool
where
    I: Iterator<Item = (usize, char)>,
{
    next_ch!(chars @ '{');
    next_ch!(chars @ '0'..='9' | 'a'..='f' | 'A'..='F');
    loop {
        let c = next_ch!(chars @ '0'..='9' | 'a'..='f' | 'A'..='F' | '_' | '}');
        if c == '}' {
            return true;
        }
    }
}

fn float(input: Cursor) -> Result<Cursor, LexError> {
    let mut rest = float_digits(input)?;
    if let Some(ch) = rest.chars().next() {
        if is_ident_start(ch) {
            rest = symbol_not_raw(rest)?.0;
        }
    }
    word_break(rest)
}

fn float_digits(input: Cursor) -> Result<Cursor, LexError> {
    let mut chars = input.chars().peekable();
    match chars.next() {
        Some(ch) if ch >= '0' && ch <= '9' => {}
        _ => return Err(LexError),
    }

    let mut len = 1;
    let mut has_dot = false;
    let mut has_exp = false;
    while let Some(&ch) = chars.peek() {
        match ch {
            '0'..='9' | '_' => {
                chars.next();
                len += 1;
            }
            '.' => {
                if has_dot {
                    break;
                }
                chars.next();
                if chars
                    .peek()
                    .map(|&ch| ch == '.' || is_ident_start(ch))
                    .unwrap_or(false)
                {
                    return Err(LexError);
                }
                len += 1;
                has_dot = true;
            }
            'e' | 'E' => {
                chars.next();
                len += 1;
                has_exp = true;
                break;
            }
            _ => break,
        }
    }

    let rest = input.advance(len);
    if !(has_dot || has_exp || rest.starts_with("f32") || rest.starts_with("f64")) {
        return Err(LexError);
    }

    if has_exp {
        let mut has_exp_value = false;
        while let Some(&ch) = chars.peek() {
            match ch {
                '+' | '-' => {
                    if has_exp_value {
                        break;
                    }
                    chars.next();
                    len += 1;
                }
                '0'..='9' => {
                    chars.next();
                    len += 1;
                    has_exp_value = true;
                }
                '_' => {
                    chars.next();
                    len += 1;
                }
                _ => break,
            }
        }
        if !has_exp_value {
            return Err(LexError);
        }
    }

    Ok(input.advance(len))
}

fn int(input: Cursor) -> Result<Cursor, LexError> {
    let mut rest = digits(input)?;
    if let Some(ch) = rest.chars().next() {
        if is_ident_start(ch) {
            rest = symbol_not_raw(rest)?.0;
        }
    }
    word_break(rest)
}

fn digits(mut input: Cursor) -> Result<Cursor, LexError> {
    let base = if input.starts_with("0x") {
        input = input.advance(2);
        16
    } else if input.starts_with("0o") {
        input = input.advance(2);
        8
    } else if input.starts_with("0b") {
        input = input.advance(2);
        2
    } else {
        10
    };

    let mut len = 0;
    let mut empty = true;
    for b in input.bytes() {
        let digit = match b {
            b'0'..=b'9' => (b - b'0') as u64,
            b'a'..=b'f' => 10 + (b - b'a') as u64,
            b'A'..=b'F' => 10 + (b - b'A') as u64,
            b'_' => {
                if empty && base == 10 {
                    return Err(LexError);
                }
                len += 1;
                continue;
            }
            _ => break,
        };
        if digit >= base {
            return Err(LexError);
        }
        len += 1;
        empty = false;
    }
    if empty {
        Err(LexError)
    } else {
        Ok(input.advance(len))
    }
}

fn op(input: Cursor) -> PResult<Punct> {
    match op_char(input) {
        Ok((rest, '\'')) => {
            symbol(rest)?;
            Ok((rest, Punct::new('\'', Spacing::Joint)))
        }
        Ok((rest, ch)) => {
            let kind = match op_char(rest) {
                Ok(_) => Spacing::Joint,
                Err(LexError) => Spacing::Alone,
            };
            Ok((rest, Punct::new(ch, kind)))
        }
        Err(LexError) => Err(LexError),
    }
}

fn op_char(input: Cursor) -> PResult<char> {
    if input.starts_with("//") || input.starts_with("/*") {
        // Do not accept `/` of a comment as an op.
        return Err(LexError);
    }

    let mut chars = input.chars();
    let first = match chars.next() {
        Some(ch) => ch,
        None => {
            return Err(LexError);
        }
    };
    let recognized = "~!@#$%^&*-=+|;:,<.>/?'";
    if recognized.contains(first) {
        Ok((input.advance(first.len_utf8()), first))
    } else {
        Err(LexError)
    }
}

fn doc_comment(input: Cursor) -> PResult<Vec<TokenTree>> {
    let (rest, ((comment, inner), span)) = spanned(input, doc_comment_contents)?;
    let mut scan_for_bare_cr = comment;
    while let Some(cr) = scan_for_bare_cr.find('\r') {
        let rest = &scan_for_bare_cr[cr + 1..];
        if !rest.starts_with('\n') {
            return Err(LexError);
        }
        scan_for_bare_cr = rest;
    }

    let mut trees = Vec::new();
    trees.push(TokenTree::Punct(Punct::new('#', Spacing::Alone)));
    if inner {
        trees.push(Punct::new('!', Spacing::Alone).into());
    }
    let mut stream = vec![
        TokenTree::Ident(crate::Ident::new("doc", span)),
        TokenTree::Punct(Punct::new('=', Spacing::Alone)),
        TokenTree::Literal(crate::Literal::string(comment)),
    ];
    for tt in stream.iter_mut() {
        tt.set_span(span);
    }
    let group = Group::new(Delimiter::Bracket, stream.into_iter().collect());
    trees.push(crate::Group::_new_stable(group).into());
    for tt in trees.iter_mut() {
        tt.set_span(span);
    }
    Ok((rest, trees))
}

fn doc_comment_contents(input: Cursor) -> PResult<(&str, bool)> {
    if input.starts_with("//!") {
        let input = input.advance(3);
        let (input, s) = take_until_newline_or_eof(input);
        Ok((input, (s, true)))
    } else if input.starts_with("/*!") {
        let (input, s) = block_comment(input)?;
        Ok((input, (&s[3..s.len() - 2], true)))
    } else if input.starts_with("///") {
        let input = input.advance(3);
        if input.starts_with("/") {
            return Err(LexError);
        }
        let (input, s) = take_until_newline_or_eof(input);
        Ok((input, (s, false)))
    } else if input.starts_with("/**") && !input.advance(3).starts_with("*") {
        let (input, s) = block_comment(input)?;
        Ok((input, (&s[3..s.len() - 2], false)))
    } else {
        Err(LexError)
    }
}

fn take_until_newline_or_eof(input: Cursor) -> (Cursor, &str) {
    let mut chars = input.char_indices();

    while let Some((byte_off, ch)) = chars.next() {
        if ch == '\n' {
            return (input.advance(byte_off), &input.rest[..byte_off]);
        } else if ch == '\r' && input.rest[byte_off + 1..].starts_with('\n') {
            return (input.advance(byte_off + 1), &input.rest[..byte_off]);
        }
    }

    (input.advance(input.len()), input.rest)
}
