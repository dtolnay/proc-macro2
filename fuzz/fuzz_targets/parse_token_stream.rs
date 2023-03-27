// libfuzzer:
//
//     cargo install cargo-fuzz
//     cargo fuzz run parse_token_stream -j $(nproc) -- -max_len=200 -timeout=1
//
// afl++:
//
//     cargo install afl
//     cargo afl build --no-default-features --features afl --release
//     cargo afl fuzz -i in -o out target/release/parse_token_stream

#![cfg_attr(feature = "libfuzzer", no_main)]

use std::str;

#[cfg(not(any(
    all(feature = "libfuzzer", not(feature = "afl")),
    all(not(feature = "libfuzzer"), feature = "afl"),
)))]
fn main() {
    compile_error! {
        r#"exactly one of feature="libfuzzer" and feature="afl" must be enabled"#
    }
}

#[cfg(feature = "libfuzzer")]
libfuzzer_sys::fuzz_target!(|bytes: &[u8]| { do_fuzz(bytes) });

#[cfg(feature = "afl")]
fn main() {
    let hook = true; // turn panic into crashes
    afl::fuzz(hook, do_fuzz);
}

fn do_fuzz(bytes: &[u8]) {
    if bytes.len() < 200 {
        if let Ok(string) = str::from_utf8(bytes) {
            _ = string.parse::<proc_macro2::TokenStream>();
        }
    }
}
