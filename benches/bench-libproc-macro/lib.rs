extern crate proc_macro;

use proc_macro::{Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use std::iter::once;
use std::time::Instant;

const N: u32 = 20000;

#[proc_macro]
pub fn bench(_input: TokenStream) -> TokenStream {
    let start = Instant::now();
    let mut string = String::new();
    for _ in 0..N {
        string += "core";
        string += ":";
        string += ":";
        string += "option";
        string += ":";
        string += ":";
        string += "Option";
        string += ":";
        string += ":";
        string += "None";
        string += ",";
    }
    string.parse::<TokenStream>().unwrap();
    eprintln!("STRING: {} millis", start.elapsed().as_millis());

    let start = Instant::now();
    let span = Span::call_site();
    let mut tokens = TokenStream::new();
    for _ in 0..N {
        // Similar to what is emitted by quote.
        tokens.extend(once(TokenTree::Ident(Ident::new("core", span))));
        tokens.extend(once(TokenTree::Punct(Punct::new(':', Spacing::Joint))));
        tokens.extend(once(TokenTree::Punct(Punct::new(':', Spacing::Alone))));
        tokens.extend(once(TokenTree::Ident(Ident::new("option", span))));
        tokens.extend(once(TokenTree::Punct(Punct::new(':', Spacing::Joint))));
        tokens.extend(once(TokenTree::Punct(Punct::new(':', Spacing::Alone))));
        tokens.extend(once(TokenTree::Ident(Ident::new("Option", span))));
        tokens.extend(once(TokenTree::Punct(Punct::new(':', Spacing::Joint))));
        tokens.extend(once(TokenTree::Punct(Punct::new(':', Spacing::Alone))));
        tokens.extend(once(TokenTree::Ident(Ident::new("None", span))));
        tokens.extend(once(TokenTree::Punct(Punct::new(',', Spacing::Joint))));
    }
    eprintln!("TOKENSTREAM: {} millis", start.elapsed().as_millis());

    TokenStream::new()
}
