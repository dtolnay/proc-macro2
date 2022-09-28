#![no_main]

use libfuzzer_sys::fuzz_target;
use std::str;

fuzz_target!(|bytes: &[u8]| {
    if bytes.len() < 200 {
        if let Ok(string) = str::from_utf8(bytes) {
            _ = string.parse::<proc_macro2::TokenStream>();
        }
    }
});
