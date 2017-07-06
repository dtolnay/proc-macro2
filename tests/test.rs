extern crate proc_macro2;

use proc_macro2::{Term, Literal, TokenStream};

#[test]
fn symbols() {
    assert_eq!(Term::intern("foo").as_str(), "foo");
    assert_eq!(Term::intern("bar").as_str(), "bar");
}

#[test]
fn literals() {
    assert_eq!(Literal::string("foo").to_string(), "\"foo\"");
    assert_eq!(Literal::string("\"").to_string(), "\"\\\"\"");
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
    roundtrip("'a");
    roundtrip("'static");
}

#[test]
fn fail() {
    fn fail(p: &str) {
        if p.parse::<TokenStream>().is_ok() {
            panic!("should have failed to parse: {}", p);
        }
    }
    fail("1x");
    fail("1u80");
    fail("1f320");
    fail("' static");
    fail("'mut");
}
