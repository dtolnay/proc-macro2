use proc_macro2::{Delimiter, TokenStream, TokenTree};

#[test]
fn tricky_doc_comment() {
    let stream = "/**/".parse::<TokenStream>().unwrap();
    let tokens = stream.into_iter().collect::<Vec<_>>();
    assert!(tokens.is_empty(), "not empty -- {:?}", tokens);

    let stream = "/// doc".parse::<TokenStream>().unwrap();
    let tokens = stream.into_iter().collect::<Vec<_>>();
    assert!(tokens.len() == 2, "not length 2 -- {:?}", tokens);
    match &tokens[0] {
        TokenTree::Punct(tt) => assert_eq!(tt.as_char(), '#'),
        _ => panic!("wrong token {:?}", tokens[0]),
    }
    let mut tokens = match &tokens[1] {
        TokenTree::Group(tt) => {
            assert_eq!(tt.delimiter(), Delimiter::Bracket);
            tt.stream().into_iter()
        }
        _ => panic!("wrong token {:?}", tokens[0]),
    };

    match tokens.next().unwrap() {
        TokenTree::Ident(tt) => assert_eq!(tt.to_string(), "doc"),
        t => panic!("wrong token {:?}", t),
    }
    match tokens.next().unwrap() {
        TokenTree::Punct(tt) => assert_eq!(tt.as_char(), '='),
        t => panic!("wrong token {:?}", t),
    }
    match tokens.next().unwrap() {
        TokenTree::Literal(tt) => {
            assert_eq!(tt.to_string(), "\" doc\"");
        }
        t => panic!("wrong token {:?}", t),
    }
    assert!(tokens.next().is_none());

    let stream = "//! doc".parse::<TokenStream>().unwrap();
    let tokens = stream.into_iter().collect::<Vec<_>>();
    assert!(tokens.len() == 3, "not length 3 -- {:?}", tokens);
}

#[test]
fn incomplete_comment_no_panic() {
    let s = "/*/";
    assert!(s.parse::<TokenStream>().is_err());
}
