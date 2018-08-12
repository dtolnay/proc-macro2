use std::env;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let target = env::var("TARGET").unwrap();

    maybe_enable_use_proc_macro(&target);
}

fn maybe_enable_use_proc_macro(target: &str) {
    // wasm targets don't have the `proc_macro` crate, disable this feature.
    if target.contains("wasm32") {
        return;
    }

    // There are currently no musl builds of the compiler, so proc_macro is
    // always missing, so disable this feature.
    if target.contains("-musl") {
        return;
    }

    // Otherwise, only enable it if our feature is actually enabled.
    if cfg!(feature = "proc-macro") {
        println!("cargo:rustc-cfg=use_proc_macro");
    }
}
