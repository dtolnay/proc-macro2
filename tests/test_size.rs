extern crate proc_macro;

use std::mem;

#[test]
fn test_proc_macro_span_size() {
    assert_eq!(mem::size_of::<proc_macro::Span>(), 4);
    assert_eq!(mem::size_of::<Option<proc_macro::Span>>(), 4);
}

#[cfg_attr(span_locations, ignore)]
#[test]
fn test_proc_macro2_span_size_without_locations() {
    assert_eq!(mem::size_of::<proc_macro2::Span>(), 4);
    assert_eq!(mem::size_of::<Option<proc_macro2::Span>>(), 8);
}

#[cfg_attr(not(span_locations), ignore)]
#[test]
fn test_proc_macro2_span_size_with_locations() {
    assert_eq!(mem::size_of::<proc_macro2::Span>(), 12);
    assert_eq!(mem::size_of::<Option<proc_macro2::Span>>(), 12);
}
