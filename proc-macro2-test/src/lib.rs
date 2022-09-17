#[cfg(test)]
extern crate proc_macro2 as proc_macro;

use proc_macro::TokenStream;

#[proc_macro]
pub fn demo(tokens: TokenStream) -> TokenStream {
    assert_eq!(tokens.to_string(), "pub struct S ;");
    "impl Trait for S {}".parse().unwrap()
}

#[cfg(test)]
mod tests {
    use quote::quote;

    #[test]
    fn it_works() {
        let out = crate::demo(quote! {
            pub struct S;
        });

        let expected = quote! {
            impl Trait for S {}
        };

        assert_eq!(out.to_string(), expected.to_string());
    }
}
