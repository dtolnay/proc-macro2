//! Tests how Spans from `proc_macro` are treated in `proc_macro2`.
//!
//! In order to test this interaction, the test (or parts of it) have to be executed in a procmacro
//! environment. Instead of failing a test, `cargo test` fails to compile. This is a bit unfortunate,
//! but keeps things simpler.
//! This crate has a dev-dependency on itself, making it possible to call the proc-macro from the
//! tests.

#[cfg(not(test))]
#[proc_macro_attribute]
pub fn test_that_columns_are_0_indexed(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let start = proc_macro2::Span::call_site().start();
    assert_eq!(start.column, 0);
    input
}

#[cfg(test)]
#[rustversion::nightly]
#[proc_macro2_span_test::test_that_columns_are_0_indexed]
fn _run_test_macro() {}
