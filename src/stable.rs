extern crate unicode_xid;

use std::ascii;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::iter;
use std::ops;
use std::rc::Rc;
use std::str::FromStr;
use std::vec;

use proc_macro;
use self::unicode_xid::UnicodeXID;
use synom::space::{skip_whitespace, block_comment, whitespace};
use synom::{IResult};

use {TokenTree, TokenKind, Delimiter, OpKind};

#[derive(Clone)]
pub struct TokenStream {
    inner: Vec<TokenTree>,
}

#[derive(Debug)]
pub struct LexError;

impl TokenStream {
    pub fn empty() -> TokenStream {
        TokenStream { inner: Vec::new() }
    }

    pub fn is_empty(&self) -> bool {
        self.inner.len() == 0
    }
}

impl FromStr for TokenStream {
    type Err = LexError;

    fn from_str(src: &str) -> Result<TokenStream, LexError> {
        match token_stream(src) {
            IResult::Done(input, output) => {
                if skip_whitespace(input).len() != 0 {
                    Err(LexError)
                } else {
                    Ok(output)
                }
            }
            IResult::Error => Err(LexError),
        }
    }
}

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut joint = false;
        for (i, tt) in self.inner.iter().enumerate() {
            if i != 0 && !joint {
                write!(f, " ")?;
            }
            joint = false;
            match tt.kind {
                TokenKind::Sequence(delim, ref stream) => {
                    let (start, end) = match delim {
                        Delimiter::Parenthesis => ("(", ")"),
                        Delimiter::Brace => ("{", "}"),
                        Delimiter::Bracket => ("[", "]"),
                        Delimiter::None => ("", ""),
                    };
                    if stream.0.inner.len() == 0 {
                        write!(f, "{} {}", start, end)?
                    } else {
                        write!(f, "{} {} {}", start, stream, end)?
                    }
                }
                TokenKind::Word(ref sym) => write!(f, "{}", sym.as_str())?,
                TokenKind::Op(ch, ref op) => {
                    write!(f, "{}", ch)?;
                    match *op {
                        OpKind::Alone => {}
                        OpKind::Joint => joint = true,
                    }
                }
                TokenKind::Literal(ref literal) => {
                    write!(f, "{}", literal)?;
                    // handle comments
                    if (literal.0).0.starts_with("/") {
                        write!(f, "\n")?;
                    }
                }
            }
        }

        Ok(())
    }
}

impl From<proc_macro::TokenStream> for TokenStream {
    fn from(inner: proc_macro::TokenStream) -> TokenStream {
        inner.to_string().parse().expect("compiler token stream parse failed")
    }
}

impl From<TokenStream> for proc_macro::TokenStream {
    fn from(inner: TokenStream) -> proc_macro::TokenStream {
        inner.to_string().parse().expect("failed to parse to compiler tokens")
    }
}


impl From<TokenTree> for TokenStream {
    fn from(tree: TokenTree) -> TokenStream {
        TokenStream { inner: vec![tree] }
    }
}

impl iter::FromIterator<TokenStream> for TokenStream {
    fn from_iter<I: IntoIterator<Item=TokenStream>>(streams: I) -> Self {
        let mut v = Vec::new();

        for stream in streams.into_iter() {
            v.extend(stream.inner);
        }

        TokenStream { inner: v }
    }
}

pub type TokenIter = vec::IntoIter<TokenTree>;

impl IntoIterator for TokenStream {
    type Item = TokenTree;
    type IntoIter = TokenIter;

    fn into_iter(self) -> TokenIter {
        self.inner.into_iter()
    }
}

#[derive(Clone, Copy, Default)]
pub struct Span;

impl Span {
    pub fn call_site() -> Span {
        Span
    }
}

#[derive(Copy, Clone)]
pub struct Symbol(usize);

thread_local!(static SYMBOLS: RefCell<Interner> = RefCell::new(Interner::new()));

impl<'a> From<&'a str> for Symbol {
    fn from(string: &'a str) -> Symbol {
        Symbol(SYMBOLS.with(|s| s.borrow_mut().intern(string)))
    }
}

impl ops::Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &str {
        SYMBOLS.with(|interner| {
            let interner = interner.borrow();
            let s = interner.get(self.0);
            unsafe {
                &*(s as *const str)
            }
        })
    }
}

struct Interner {
    string_to_index: HashMap<MyRc, usize>,
    index_to_string: Vec<Rc<String>>,
}

#[derive(Hash, Eq, PartialEq)]
struct MyRc(Rc<String>);

impl Borrow<str> for MyRc {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl Interner {
    fn new() -> Interner {
        Interner {
            string_to_index: HashMap::new(),
            index_to_string: Vec::new(),
        }
    }

   fn intern(&mut self, s: &str) -> usize {
        if let Some(&idx) = self.string_to_index.get(s) {
            return idx
        }
        let s = Rc::new(s.to_string());
        self.index_to_string.push(s.clone());
        self.string_to_index.insert(MyRc(s), self.index_to_string.len() - 1);
        self.index_to_string.len() - 1
    }

   fn get(&self, idx: usize) -> &str {
       &self.index_to_string[idx]
   }
}

#[derive(Clone)]
pub struct Literal(String);

impl Literal {
    pub fn byte_char(byte: u8) -> Literal {
        match byte {
            0 => Literal(format!("b'\\0'")),
            b'\"' => Literal(format!("b'\"'")),
            n => {
                let mut escaped = "b'".to_string();
                escaped.extend(ascii::escape_default(n).map(|c| c as char));
                escaped.push('\'');
                Literal(escaped)
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
        Literal(escaped)
    }

    pub fn doccomment(s: &str) -> Literal {
        Literal(s.to_string())
    }

    pub fn float(s: &str) -> Literal {
        Literal(s.to_string())
    }

    pub fn integer(s: &str) -> Literal {
        Literal(s.to_string())
    }

    pub fn raw_string(s: &str, pounds: usize) -> Literal {
        let mut ret = format!("r");
        ret.extend((0..pounds).map(|_| "#"));
        ret.push('"');
        ret.push_str(s);
        ret.push('"');
        ret.extend((0..pounds).map(|_| "#"));
        Literal(ret)
    }

    pub fn raw_byte_string(s: &str, pounds: usize) -> Literal {
        let mut ret = format!("br");
        ret.extend((0..pounds).map(|_| "#"));
        ret.push('"');
        ret.push_str(s);
        ret.push('"');
        ret.extend((0..pounds).map(|_| "#"));
        Literal(ret)
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

macro_rules! ints {
    ($($t:ty,)*) => {$(
        impl From<$t> for Literal {
            fn from(t: $t) -> Literal {
                Literal(format!(concat!("{}", stringify!($t)), t))
            }
        }
    )*}
}

ints! {
    u8, u16, u32, u64, usize,
    i8, i16, i32, i64, isize,
}

macro_rules! floats {
    ($($t:ty,)*) => {$(
        impl From<$t> for Literal {
            fn from(t: $t) -> Literal {
                assert!(!t.is_nan());
                assert!(!t.is_infinite());
                Literal(format!(concat!("{}", stringify!($t)), t))
            }
        }
    )*}
}

floats! {
    f32, f64,
}

impl<'a> From<&'a str> for Literal {
    fn from(t: &'a str) -> Literal {
        let mut s = t.chars().flat_map(|c| c.escape_default()).collect::<String>();
        s.push('"');
        s.insert(0, '"');
        Literal(s)
    }
}

impl From<char> for Literal {
    fn from(t: char) -> Literal {
        Literal(format!("'{}'", t.escape_default().collect::<String>()))
    }
}

named!(token_stream -> TokenStream,
       map!(token_trees, |s: Vec<TokenTree>| TokenStream { inner: s }));

named!(token_trees -> Vec<TokenTree>, many0!(token_tree));

named!(token_tree -> TokenTree,
       map!(token_kind, |s: TokenKind| {
           TokenTree {
               span: ::Span(Span),
               kind: s,
           }
       }));

named!(token_kind -> TokenKind, alt!(
    map!(delimited, |(d, s): (Delimiter, TokenStream)| {
        TokenKind::Sequence(d, ::TokenStream(s))
    })
    |
    map!(literal, |l| TokenKind::Literal(::Literal(l))) // must be before symbol
    |
    map!(symbol, |w| TokenKind::Word(::Symbol(w)))
    |
    map!(op, |(op, kind): (char, OpKind)| {
        TokenKind::Op(op, kind)
    })
));

named!(delimited -> (Delimiter, TokenStream), alt!(
    delimited!(
        punct!("("),
        token_stream,
        punct!(")")
    ) => { |ts| (Delimiter::Parenthesis, ts) }
    |
    delimited!(
        punct!("["),
        token_stream,
        punct!("]")
    ) => { |ts| (Delimiter::Bracket, ts) }
    |
    delimited!(
        punct!("{"),
        token_stream,
        punct!("}")
    ) => { |ts| (Delimiter::Brace, ts) }
));

named!(symbol -> Symbol, alt!(
    lifetime
    |
    map!(word, Symbol::from)
));

named!(lifetime -> Symbol, preceded!(
    punct!("'"),
    alt!(
        // TODO: can we get rid of this allocation?
        map!(word, |id| Symbol::from(&format!("'{}", id)[..]))
        |
        map!(keyword!("static"), |_| Symbol::from("'static"))
    )
));

fn word(mut input: &str) -> IResult<&str, &str> {
    input = skip_whitespace(input);

    let mut chars = input.char_indices();
    match chars.next() {
        Some((_, ch)) if UnicodeXID::is_xid_start(ch) || ch == '_' => {}
        _ => return IResult::Error,
    }

    for (i, ch) in chars {
        if !UnicodeXID::is_xid_continue(ch) {
            return IResult::Done(&input[i..], &input[..i])
        }
    }

    IResult::Done("", input)
}

fn literal(input: &str) -> IResult<&str, Literal> {
    let input_no_ws = skip_whitespace(input);

    match literal_nocapture(input_no_ws) {
        IResult::Done(a, ()) => {
            let start = input.len() - input_no_ws.len();
            let len = input_no_ws.len() - a.len();
            let end = start + len;
            IResult::Done(a, Literal(input[start..end].to_string()))
        }
        IResult::Error => IResult::Error,
    }
}

named!(literal_nocapture -> (), alt!(
    string
    |
    byte_string
    |
    byte
    |
    character
    |
    float
    |
    int
    |
    boolean
    |
    doc_comment
));

named!(string -> (), alt!(
    quoted_string
    |
    preceded!(
        punct!("r"),
        raw_string
    ) => { |_| () }
));

named!(quoted_string -> (), delimited!(
    punct!("\""),
    cooked_string,
    tag!("\"")
));

fn cooked_string(input: &str) -> IResult<&str, ()> {
    let mut chars = input.char_indices().peekable();
    while let Some((byte_offset, ch)) = chars.next() {
        match ch {
            '"' => {
                return IResult::Done(&input[byte_offset..], ());
            }
            '\r' => {
                if let Some((_, '\n')) = chars.next() {
                    // ...
                } else {
                    break;
                }
            }
            '\\' => {
                match chars.next() {
                    Some((_, 'x')) => {
                        if !backslash_x_char(&mut chars) {
                            break
                        }
                    }
                    Some((_, 'n')) |
                    Some((_, 'r')) |
                    Some((_, 't')) |
                    Some((_, '\\')) |
                    Some((_, '\'')) |
                    Some((_, '"')) |
                    Some((_, '0')) => {}
                    Some((_, 'u')) => {
                        if !backslash_u(&mut chars) {
                            break
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
                }
            }
            _ch => {}
        }
    }
    IResult::Error
}

named!(byte_string -> (), alt!(
    delimited!(
        punct!("b\""),
        cooked_byte_string,
        tag!("\"")
    ) => { |_| () }
    |
    preceded!(
        punct!("br"),
        raw_string
    ) => { |_| () }
));

fn cooked_byte_string(mut input: &str) -> IResult<&str, ()> {
    let mut bytes = input.bytes().enumerate();
    'outer: while let Some((offset, b)) = bytes.next() {
        match b {
            b'"' => {
                return IResult::Done(&input[offset..], ());
            }
            b'\r' => {
                if let Some((_, b'\n')) = bytes.next() {
                    // ...
                } else {
                    break;
                }
            }
            b'\\' => {
                match bytes.next() {
                    Some((_, b'x')) => {
                        if !backslash_x_byte(&mut bytes) {
                            break
                        }
                    }
                    Some((_, b'n')) |
                    Some((_, b'r')) |
                    Some((_, b't')) |
                    Some((_, b'\\')) |
                    Some((_, b'0')) |
                    Some((_, b'\'')) |
                    Some((_, b'"'))  => {}
                    Some((newline, b'\n')) |
                    Some((newline, b'\r')) => {
                        let rest = &input[newline + 1..];
                        for (offset, ch) in rest.char_indices() {
                            if !ch.is_whitespace() {
                                input = &rest[offset..];
                                bytes = input.bytes().enumerate();
                                continue 'outer;
                            }
                        }
                        break;
                    }
                    _ => break,
                }
            }
            b if b < 0x80 => {}
            _ => break,
        }
    }
    IResult::Error
}

fn raw_string(input: &str) -> IResult<&str, ()> {
    let mut chars = input.char_indices();
    let mut n = 0;
    while let Some((byte_offset, ch)) = chars.next() {
        match ch {
            '"' => {
                n = byte_offset;
                break;
            }
            '#' => {}
            _ => return IResult::Error,
        }
    }
    for (byte_offset, ch) in chars {
        match ch {
            '"' if input[byte_offset + 1..].starts_with(&input[..n]) => {
                let rest = &input[byte_offset + 1 + n..];
                return IResult::Done(rest, ())
            }
            '\r' => {}
            _ => {}
        }
    }
    IResult::Error
}

named!(byte -> (), do_parse!(
    punct!("b") >>
    tag!("'") >>
    cooked_byte >>
    tag!("'") >>
    (())
));

fn cooked_byte(input: &str) -> IResult<&str, ()> {
    let mut bytes = input.bytes().enumerate();
    let ok = match bytes.next().map(|(_, b)| b) {
        Some(b'\\') => {
            match bytes.next().map(|(_, b)| b) {
                Some(b'x') => backslash_x_byte(&mut bytes),
                Some(b'n') |
                Some(b'r') |
                Some(b't') |
                Some(b'\\') |
                Some(b'0') |
                Some(b'\'') |
                Some(b'"') => true,
                _ => false,
            }
        }
        b => b.is_some(),
    };
    if ok {
        match bytes.next() {
            Some((offset, _)) => IResult::Done(&input[offset..], ()),
            None => IResult::Done("", ()),
        }
    } else {
        IResult::Error
    }
}

named!(character -> (), do_parse!(
    punct!("'") >>
    cooked_char >>
    tag!("'") >>
    (())
));

fn cooked_char(input: &str) -> IResult<&str, ()> {
    let mut chars = input.char_indices();
    let ok = match chars.next().map(|(_, ch)| ch) {
        Some('\\') => {
            match chars.next().map(|(_, ch)| ch) {
                Some('x') => backslash_x_char(&mut chars),
                Some('u') => backslash_u(&mut chars),
                Some('n') |
                Some('r') |
                Some('t') |
                Some('\\') |
                Some('0') |
                Some('\'') |
                Some('"') => true,
                _ => false,
            }
        }
        ch => ch.is_some(),
    };
    if ok {
        IResult::Done(chars.as_str(), ())
    } else {
        IResult::Error
    }
}

macro_rules! next_ch {
    ($chars:ident @ $pat:pat $(| $rest:pat)*) => {
        match $chars.next() {
            Some((_, ch)) => match ch {
                $pat $(| $rest)*  => ch,
                _ => return false,
            },
            None => return false
        }
    };
}

fn backslash_x_char<I>(chars: &mut I) -> bool
    where I: Iterator<Item = (usize, char)>
{
    next_ch!(chars @ '0'...'7');
    next_ch!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F');
    true
}

fn backslash_x_byte<I>(chars: &mut I) -> bool
    where I: Iterator<Item = (usize, u8)>
{
    next_ch!(chars @ b'0'...b'9' | b'a'...b'f' | b'A'...b'F');
    next_ch!(chars @ b'0'...b'9' | b'a'...b'f' | b'A'...b'F');
    true
}

fn backslash_u<I>(chars: &mut I) -> bool
    where I: Iterator<Item = (usize, char)>
{
    next_ch!(chars @ '{');
    next_ch!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F');
    let b = next_ch!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if b == '}' {
        return true
    }
    let c = next_ch!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if c == '}' {
        return true
    }
    let d = next_ch!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if d == '}' {
        return true
    }
    let e = next_ch!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if e == '}' {
        return true
    }
    let f = next_ch!(chars @ '0'...'9' | 'a'...'f' | 'A'...'F' | '}');
    if f == '}' {
        return true
    }
    next_ch!(chars @ '}');
    true
}

named!(float -> (), do_parse!(
    float_string >>
    alt!(
        tag!("f32") => { |_| () }
        |
        tag!("f64") => { |_| () }
        |
        epsilon!()
    ) >>
    (())
));

fn float_string(input: &str) -> IResult<&str, ()> {
    let mut chars = input.chars().peekable();
    match chars.next() {
        Some(ch) if ch >= '0' && ch <= '9' => {}
        _ => return IResult::Error,
    }

    let mut len = 1;
    let mut has_dot = false;
    let mut has_exp = false;
    while let Some(&ch) = chars.peek() {
        match ch {
            '0'...'9' | '_' => {
                chars.next();
                len += 1;
            }
            '.' => {
                if has_dot {
                    break;
                }
                chars.next();
                if chars.peek()
                       .map(|&ch| ch == '.' || UnicodeXID::is_xid_start(ch))
                       .unwrap_or(false) {
                    return IResult::Error;
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

    let rest = &input[len..];
    if !(has_dot || has_exp || rest.starts_with("f32") || rest.starts_with("f64")) {
        return IResult::Error;
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
                '0'...'9' => {
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
            return IResult::Error;
        }
    }

    IResult::Done(&input[len..], ())
}

named!(int -> (), do_parse!(
    digits >>
    alt!(
        tag!("isize") => { |_| () }
        |
        tag!("i8") => { |_| () }
        |
        tag!("i16") => { |_| () }
        |
        tag!("i32") => { |_| () }
        |
        tag!("i64") => { |_| () }
        |
        tag!("usize") => { |_| () }
        |
        tag!("u8") => { |_| () }
        |
        tag!("u16") => { |_| () }
        |
        tag!("u32") => { |_| () }
        |
        tag!("u64") => { |_| () }
        |
        epsilon!()
    ) >>
    (())
));

fn digits(mut input: &str) -> IResult<&str, ()> {
    let base = if input.starts_with("0x") {
        input = &input[2..];
        16
    } else if input.starts_with("0o") {
        input = &input[2..];
        8
    } else if input.starts_with("0b") {
        input = &input[2..];
        2
    } else {
        10
    };

    let mut len = 0;
    let mut empty = true;
    for b in input.bytes() {
        let digit = match b {
            b'0'...b'9' => (b - b'0') as u64,
            b'a'...b'f' => 10 + (b - b'a') as u64,
            b'A'...b'F' => 10 + (b - b'A') as u64,
            b'_' => {
                if empty && base == 10 {
                    return IResult::Error;
                }
                len += 1;
                continue;
            }
            _ => break,
        };
        if digit >= base {
            return IResult::Error;
        }
        len += 1;
        empty = false;
    }
    if empty {
        IResult::Error
    } else {
        IResult::Done(&input[len..], ())
    }
}

named!(boolean -> (), alt!(
    keyword!("true") => { |_| () }
    |
    keyword!("false") => { |_| () }
));

macro_rules! punct1 {
    ($i:expr, $punct:expr) => {
        punct1($i, $punct)
    }
}

fn punct1<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, char> {
    let input = skip_whitespace(input);
    if input.starts_with(token) {
        IResult::Done(&input[1..], token.chars().next().unwrap())
    } else {
        IResult::Error
    }
}

named!(op -> (char, OpKind), alt!(
    keyword!("_") => { |_| ('_', OpKind::Alone) }
    |
    punct1!("&&") => { |_| ('&', OpKind::Joint) }
    |
    punct1!("||") => { |_| ('|', OpKind::Joint) }
    |
    punct1!("->") => { |_| ('-', OpKind::Joint) }
    |
    punct1!("<-") => { |_| ('<', OpKind::Joint) }
    |
    punct1!("=>") => { |_| ('=', OpKind::Joint) }
    |
    punct1!("...") => { |_| ('.', OpKind::Joint) }
    |
    punct1!("..") => { |_| ('.', OpKind::Joint) }
    |
    punct!(".") => { |_| ('.', OpKind::Alone) }
    |
    punct!(",") => { |_| (',', OpKind::Alone) }
    |
    bin_op_eq
    |
    bin_op
    |
    punct1!("<=") => { |_| ('<', OpKind::Joint) }
    |
    punct1!("==") => { |_| ('=', OpKind::Joint) }
    |
    punct1!("!=") => { |_| ('!', OpKind::Joint) }
    |
    punct1!(">=") => { |_| ('>', OpKind::Joint) }
    |
    punct1!("::") => { |_| (':', OpKind::Joint) }
    |
    punct!("=") => { |_| ('=', OpKind::Alone) }
    |
    punct!("<") => { |_| ('<', OpKind::Alone) }
    |
    punct!(">") => { |_| ('>', OpKind::Alone) }
    |
    punct!("!") => { |_| ('!', OpKind::Alone) }
    |
    punct!("~") => { |_| ('~', OpKind::Alone) }
    |
    punct!("@") => { |_| ('@', OpKind::Alone) }
    |
    punct!(",") => { |_| (',', OpKind::Alone) }
    |
    punct!(";") => { |_| (';', OpKind::Alone) }
    |
    punct!(":") => { |_| (':', OpKind::Alone) }
    |
    punct!("#") => { |_| ('#', OpKind::Alone) }
    |
    punct!("$") => { |_| ('$', OpKind::Alone) }
    |
    punct!("?") => { |_| ('?', OpKind::Alone) }
));

named!(bin_op -> (char, OpKind), alt!(
    punct!("+") => { |_| ('+', OpKind::Alone) }
    |
    punct!("-") => { |_| ('-', OpKind::Alone) }
    |
    punct!("*") => { |_| ('*', OpKind::Alone) }
    |
    punct!("/") => { |_| ('/', OpKind::Alone) }
    |
    punct!("%") => { |_| ('%', OpKind::Alone) }
    |
    punct!("^") => { |_| ('^', OpKind::Alone) }
    |
    punct!("&") => { |_| ('&', OpKind::Alone) }
    |
    punct!("|") => { |_| ('|', OpKind::Alone) }
    |
    punct1!("<<") => { |_| ('<', OpKind::Joint) }
    |
    punct1!(">>") => { |_| ('>', OpKind::Joint) }
));

named!(bin_op_eq -> (char, OpKind), alt!(
    punct1!("+=") => { |_| ('+', OpKind::Joint) }
    |
    punct1!("-=") => { |_| ('-', OpKind::Joint) }
    |
    punct1!("*=") => { |_| ('*', OpKind::Joint) }
    |
    punct1!("/=") => { |_| ('/', OpKind::Joint) }
    |
    punct1!("%=") => { |_| ('%', OpKind::Joint) }
    |
    punct1!("^=") => { |_| ('^', OpKind::Joint) }
    |
    punct1!("&=") => { |_| ('&', OpKind::Joint) }
    |
    punct1!("|=") => { |_| ('|', OpKind::Joint) }
    |
    punct1!("<<=") => { |_| ('<', OpKind::Joint) }
    |
    punct1!(">>=") => { |_| ('>', OpKind::Joint) }
));

named!(doc_comment -> (), alt!(
    do_parse!(
        punct!("//!") >>
        take_until!("\n") >>
        (())
    )
    |
    do_parse!(
        option!(whitespace) >>
        peek!(tag!("/*!")) >>
        block_comment >>
        (())
    )
    |
    do_parse!(
        punct!("///") >>
        not!(tag!("/")) >>
        take_until!("\n") >>
        (())
    )
    |
    do_parse!(
        option!(whitespace) >>
        peek!(tuple!(tag!("/**"), not!(tag!("*")))) >>
        block_comment >>
        (())
    )
));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn symbols() {
        assert_eq!(&*Symbol::from("foo"), "foo");
        assert_eq!(&*Symbol::from("bar"), "bar");
    }

    #[test]
    fn literals() {
        assert_eq!(Literal::from("foo").to_string(), "\"foo\"");
        assert_eq!(Literal::from("\"").to_string(), "\"\\\"\"");
    }

    #[test]
    fn roundtrip() {
        fn roundtrip(p: &str) {
            println!("parse: {}", p);
            let s = p.parse::<TokenStream>().unwrap().to_string();
            println!("first: {}", s);
            let s2 = s.to_string().parse::<TokenStream>().unwrap().to_string();
            assert_eq!(s, s2);
        }
        roundtrip("a");
        roundtrip("<<");
        roundtrip("<<=");
        roundtrip("
            /// a
            wut
        ");
        roundtrip("
            1
            1.0
            1f32
            2f64
            1usize
            4isize
            4e10
            1_000
            1_0i32
            8u8
            9
            0
            0xffffffffffffffffffffffffffffffff
        ");
    }
}
