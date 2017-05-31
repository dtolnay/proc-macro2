//! Adapted from [`nom`](https://github.com/Geal/nom) by removing the
//! `IResult::Incomplete` variant.

use unicode_xid::UnicodeXID;

pub enum IResult<I, O> {
    /// Parsing succeeded. The first field contains the rest of the unparsed
    /// data and the second field contains the parse result.
    Done(I, O),
    /// Parsing failed.
    Error,
}

pub fn whitespace(input: &str) -> IResult<&str, ()> {
    if input.is_empty() {
        return IResult::Error;
    }

    let bytes = input.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        let s = &input[i..];
        if bytes[i] == b'/' {
            if s.starts_with("//") && (!s.starts_with("///") || s.starts_with("////")) &&
               !s.starts_with("//!") {
                if let Some(len) = s.find('\n') {
                    i += len + 1;
                    continue;
                }
                break;
            } else if s.starts_with("/*") && (!s.starts_with("/**") || s.starts_with("/***")) &&
                      !s.starts_with("/*!") {
                match block_comment(s) {
                    IResult::Done(_, com) => {
                        i += com.len();
                        continue;
                    }
                    IResult::Error => {
                        return IResult::Error;
                    }
                }
            }
        }
        match bytes[i] {
            b' ' | 0x09...0x0d => {
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
        return if i > 0 {
            IResult::Done(s, ())
        } else {
            IResult::Error
        };
    }
    IResult::Done("", ())
}

pub fn block_comment(input: &str) -> IResult<&str, &str> {
    if !input.starts_with("/*") {
        return IResult::Error;
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
                return IResult::Done(&input[i + 2..], &input[..i + 2]);
            }
            i += 1; // eat '/'
        }
        i += 1;
    }
    IResult::Error
}

pub fn skip_whitespace(input: &str) -> &str {
    match whitespace(input) {
        IResult::Done(rest, _) => rest,
        IResult::Error => input,
    }
}

fn is_whitespace(ch: char) -> bool {
    // Rust treats left-to-right mark and right-to-left mark as whitespace
    ch.is_whitespace() || ch == '\u{200e}' || ch == '\u{200f}'
}

fn word_break(input: &str) -> IResult<&str, ()> {
    match input.chars().next() {
        Some(ch) if UnicodeXID::is_xid_continue(ch) => IResult::Error,
        Some(_) | None => IResult::Done(input, ()),
    }
}

macro_rules! named {
    ($name:ident -> $o:ty, $submac:ident!( $($args:tt)* )) => {
        fn $name(i: &str) -> $crate::strnom::IResult<&str, $o> {
            $submac!(i, $($args)*)
        }
    };
}

macro_rules! alt {
    ($i:expr, $e:ident | $($rest:tt)*) => {
        alt!($i, call!($e) | $($rest)*)
    };

    ($i:expr, $subrule:ident!( $($args:tt)*) | $($rest:tt)*) => {
        match $subrule!($i, $($args)*) {
            res @ $crate::strnom::IResult::Done(_, _) => res,
            _ => alt!($i, $($rest)*)
        }
    };

    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr } | $($rest:tt)+) => {
        match $subrule!($i, $($args)*) {
            $crate::strnom::IResult::Done(i, o) => $crate::strnom::IResult::Done(i, $gen(o)),
            $crate::strnom::IResult::Error => alt!($i, $($rest)*)
        }
    };

    ($i:expr, $e:ident => { $gen:expr } | $($rest:tt)*) => {
        alt!($i, call!($e) => { $gen } | $($rest)*)
    };

    ($i:expr, $e:ident => { $gen:expr }) => {
        alt!($i, call!($e) => { $gen })
    };

    ($i:expr, $subrule:ident!( $($args:tt)* ) => { $gen:expr }) => {
        match $subrule!($i, $($args)*) {
            $crate::strnom::IResult::Done(i, o) => $crate::strnom::IResult::Done(i, $gen(o)),
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
        }
    };

    ($i:expr, $e:ident) => {
        alt!($i, call!($e))
    };

    ($i:expr, $subrule:ident!( $($args:tt)*)) => {
        $subrule!($i, $($args)*)
    };
}

macro_rules! do_parse {
    ($i:expr, ( $($rest:expr),* )) => {
        $crate::strnom::IResult::Done($i, ( $($rest),* ))
    };

    ($i:expr, $e:ident >> $($rest:tt)*) => {
        do_parse!($i, call!($e) >> $($rest)*)
    };

    ($i:expr, $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
            $crate::strnom::IResult::Done(i, _) =>
                do_parse!(i, $($rest)*),
        }
    };

    ($i:expr, $field:ident : $e:ident >> $($rest:tt)*) => {
        do_parse!($i, $field: call!($e) >> $($rest)*)
    };

    ($i:expr, $field:ident : $submac:ident!( $($args:tt)* ) >> $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
            $crate::strnom::IResult::Done(i, o) => {
                let $field = o;
                do_parse!(i, $($rest)*)
            },
        }
    };
}

macro_rules! peek {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            $crate::strnom::IResult::Done(_, o) => $crate::strnom::IResult::Done($i, o),
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
        }
    };
}

macro_rules! call {
    ($i:expr, $fun:expr $(, $args:expr)*) => {
        $fun($i $(, $args)*)
    };
}

macro_rules! option {
    ($i:expr, $f:expr) => {
        match $f($i) {
            $crate::strnom::IResult::Done(i, o) => $crate::strnom::IResult::Done(i, Some(o)),
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Done($i, None),
        }
    };
}

macro_rules! take_until {
    ($i:expr, $substr:expr) => {{
        if $substr.len() > $i.len() {
            $crate::strnom::IResult::Error
        } else {
            let substr_vec: Vec<char> = $substr.chars().collect();
            let mut window: Vec<char> = vec![];
            let mut offset = $i.len();
            let mut parsed = false;
            for (o, c) in $i.char_indices() {
                window.push(c);
                if window.len() > substr_vec.len() {
                    window.remove(0);
                }
                if window == substr_vec {
                    parsed = true;
                    window.pop();
                    let window_len: usize = window.iter()
                        .map(|x| x.len_utf8())
                        .fold(0, |x, y| x + y);
                    offset = o - window_len;
                    break;
                }
            }
            if parsed {
                $crate::strnom::IResult::Done(&$i[offset..], &$i[..offset])
            } else {
                $crate::strnom::IResult::Error
            }
        }
    }};
}

macro_rules! tuple {
    ($i:expr, $($rest:tt)*) => {
        tuple_parser!($i, (), $($rest)*)
    };
}

/// Do not use directly. Use `tuple!`.
macro_rules! tuple_parser {
    ($i:expr, ($($parsed:tt),*), $e:ident, $($rest:tt)*) => {
        tuple_parser!($i, ($($parsed),*), call!($e), $($rest)*)
    };

    ($i:expr, (), $submac:ident!( $($args:tt)* ), $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
            $crate::strnom::IResult::Done(i, o) =>
                tuple_parser!(i, (o), $($rest)*),
        }
    };

    ($i:expr, ($($parsed:tt)*), $submac:ident!( $($args:tt)* ), $($rest:tt)*) => {
        match $submac!($i, $($args)*) {
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
            $crate::strnom::IResult::Done(i, o) =>
                tuple_parser!(i, ($($parsed)* , o), $($rest)*),
        }
    };

    ($i:expr, ($($parsed:tt),*), $e:ident) => {
        tuple_parser!($i, ($($parsed),*), call!($e))
    };

    ($i:expr, (), $submac:ident!( $($args:tt)* )) => {
        $submac!($i, $($args)*)
    };

    ($i:expr, ($($parsed:expr),*), $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
            $crate::strnom::IResult::Done(i, o) => $crate::strnom::IResult::Done(i, ($($parsed),*, o))
        }
    };

    ($i:expr, ($($parsed:expr),*)) => {
        $crate::strnom::IResult::Done($i, ($($parsed),*))
    };
}

macro_rules! not {
    ($i:expr, $submac:ident!( $($args:tt)* )) => {
        match $submac!($i, $($args)*) {
            $crate::strnom::IResult::Done(_, _) => $crate::strnom::IResult::Error,
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Done($i, ()),
        }
    };
}

macro_rules! tag {
    ($i:expr, $tag:expr) => {
        if $i.starts_with($tag) {
            $crate::strnom::IResult::Done(&$i[$tag.len()..], &$i[..$tag.len()])
        } else {
            $crate::strnom::IResult::Error
        }
    };
}

macro_rules! punct {
    ($i:expr, $punct:expr) => {
        $crate::strnom::punct($i, $punct)
    };
}

/// Do not use directly. Use `punct!`.
pub fn punct<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, &'a str> {
    let input = skip_whitespace(input);
    if input.starts_with(token) {
        IResult::Done(&input[token.len()..], token)
    } else {
        IResult::Error
    }
}

macro_rules! keyword {
    ($i:expr, $keyword:expr) => {
        $crate::strnom::keyword($i, $keyword)
    };
}

/// Do not use directly. Use `keyword!`.
pub fn keyword<'a>(input: &'a str, token: &'static str) -> IResult<&'a str, &'a str> {
    match punct(input, token) {
        IResult::Done(rest, _) => {
            match word_break(rest) {
                IResult::Done(_, _) => IResult::Done(rest, token),
                IResult::Error => IResult::Error,
            }
        }
        IResult::Error => IResult::Error,
    }
}

macro_rules! epsilon {
    ($i:expr,) => {
        $crate::strnom::IResult::Done($i, ())
    };
}

macro_rules! preceded {
    ($i:expr, $submac:ident!( $($args:tt)* ), $submac2:ident!( $($args2:tt)* )) => {
        match tuple!($i, $submac!($($args)*), $submac2!($($args2)*)) {
            $crate::strnom::IResult::Done(remaining, (_, o)) => $crate::strnom::IResult::Done(remaining, o),
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
        }
    };

    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => {
        preceded!($i, $submac!($($args)*), call!($g))
    };
}

macro_rules! delimited {
    ($i:expr, $submac:ident!( $($args:tt)* ), $($rest:tt)+) => {
        match tuple_parser!($i, (), $submac!($($args)*), $($rest)*) {
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
            $crate::strnom::IResult::Done(i1, (_, o, _)) => $crate::strnom::IResult::Done(i1, o)
        }
    };
}

macro_rules! map {
    ($i:expr, $submac:ident!( $($args:tt)* ), $g:expr) => {
        match $submac!($i, $($args)*) {
            $crate::strnom::IResult::Error => $crate::strnom::IResult::Error,
            $crate::strnom::IResult::Done(i, o) => {
                $crate::strnom::IResult::Done(i, call!(o, $g))
            }
        }
    };

    ($i:expr, $f:expr, $g:expr) => {
        map!($i, call!($f), $g)
    };
}

macro_rules! many0 {
    ($i:expr, $f:expr) => {{
        let ret;
        let mut res   = ::std::vec::Vec::new();
        let mut input = $i;

        loop {
            if input.is_empty() {
                ret = $crate::strnom::IResult::Done(input, res);
                break;
            }

            match $f(input) {
                $crate::strnom::IResult::Error => {
                    ret = $crate::strnom::IResult::Done(input, res);
                    break;
                }
                $crate::strnom::IResult::Done(i, o) => {
                    // loop trip must always consume (otherwise infinite loops)
                    if i.len() == input.len() {
                        ret = $crate::strnom::IResult::Error;
                        break;
                    }

                    res.push(o);
                    input = i;
                }
            }
        }

        ret
    }};
}
