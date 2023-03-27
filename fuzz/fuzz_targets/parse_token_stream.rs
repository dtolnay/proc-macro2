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
//
// honggfuzz:
//
//     cargo install honggfuzz
//     cargo hfuzz build --no-default-features --features honggfuzz
//     HFUZZ_RUN_ARGS="--threads $(nproc) --max_file_size 200 --timeout 1" cargo hfuzz run parse_token_stream

#![cfg_attr(feature = "libfuzzer", no_main)]

use cfg_if::cfg_if;
use std::str;

cfg_if! {
    if #[cfg(all(feature = "libfuzzer", not(feature = "afl"), not(feature = "honggfuzz")))] {
        libfuzzer_sys::fuzz_target!(|bytes: &[u8]| { do_fuzz(bytes) });
    } else if #[cfg(all(feature = "afl", not(feature = "libfuzzer"), not(feature = "honggfuzz")))] {
        fn main() {
            let hook = true; // turn panic into crashes
            afl::fuzz(hook, do_fuzz);
        }
    } else if #[cfg(all(feature = "honggfuzz", not(feature = "libfuzzer"), not(feature = "afl")))] {
        fn main() {
            loop {
                honggfuzz::fuzz(do_fuzz);
            }
        }
    } else {
        compile_error! {
            r#"exactly one of feature="libfuzzer" or feature="afl" or feature="honggfuzz" must be enabled"#
        }
        fn main() {}
    }
}

fn do_fuzz(bytes: &[u8]) {
    let ..=199 = bytes.len() else { return };
    let Ok(string) = str::from_utf8(bytes) else { return };
    let _ = string.parse::<proc_macro2::TokenStream>();
}
