#![allow(unknown_lints)]
#![allow(unexpected_cfgs)]

use std::env;
use std::ffi::OsString;
use std::fs;
use std::io::ErrorKind;
use std::iter;
use std::path::Path;
use std::process::{self, Command, Stdio};
use std::str;

fn main() {
    let rustc = rustc_minor_version().unwrap_or(u32::MAX);

    if rustc >= 80 {
        println!("cargo:rustc-check-cfg=cfg(fuzzing)");
        println!("cargo:rustc-check-cfg=cfg(no_is_available)");
        println!("cargo:rustc-check-cfg=cfg(no_literal_byte_character)");
        println!("cargo:rustc-check-cfg=cfg(no_literal_c_string)");
        println!("cargo:rustc-check-cfg=cfg(no_source_text)");
        println!("cargo:rustc-check-cfg=cfg(proc_macro_span)");
        println!("cargo:rustc-check-cfg=cfg(proc_macro_span_file)");
        println!("cargo:rustc-check-cfg=cfg(proc_macro_span_location)");
        println!("cargo:rustc-check-cfg=cfg(procmacro2_backtrace)");
        println!("cargo:rustc-check-cfg=cfg(procmacro2_build_probe)");
        println!("cargo:rustc-check-cfg=cfg(procmacro2_nightly_testing)");
        println!("cargo:rustc-check-cfg=cfg(procmacro2_semver_exempt)");
        println!("cargo:rustc-check-cfg=cfg(randomize_layout)");
        println!("cargo:rustc-check-cfg=cfg(span_locations)");
        println!("cargo:rustc-check-cfg=cfg(super_unstable)");
        println!("cargo:rustc-check-cfg=cfg(wrap_proc_macro)");
    }

    let semver_exempt = cfg!(procmacro2_semver_exempt);
    if semver_exempt {
        // https://github.com/dtolnay/proc-macro2/issues/147
        println!("cargo:rustc-cfg=procmacro2_semver_exempt");
    }

    if semver_exempt || cfg!(feature = "span-locations") {
        // Provide methods Span::start and Span::end which give the line/column
        // location of a token. This is behind a cfg because tracking location
        // inside spans is a performance hit.
        println!("cargo:rustc-cfg=span_locations");
    }

    if rustc < 57 {
        // Do not use proc_macro::is_available() to detect whether the proc
        // macro API is available vs needs to be polyfilled. Instead, use the
        // proc macro API unconditionally and catch the panic that occurs if it
        // isn't available.
        println!("cargo:rustc-cfg=no_is_available");
    }

    if rustc < 66 {
        // Do not call libproc_macro's Span::source_text. Always return None.
        println!("cargo:rustc-cfg=no_source_text");
    }

    if rustc < 79 {
        // Do not call Literal::byte_character nor Literal::c_string. They can
        // be emulated by way of Literal::from_str.
        println!("cargo:rustc-cfg=no_literal_byte_character");
        println!("cargo:rustc-cfg=no_literal_c_string");
    }

    if !cfg!(feature = "proc-macro") {
        println!("cargo:rerun-if-changed=build.rs");
        return;
    }

    let proc_macro_span;
    let consider_rustc_bootstrap;
    if compile_probe_unstable("proc_macro_span", false) {
        // This is a nightly or dev compiler, so it supports unstable features
        // regardless of RUSTC_BOOTSTRAP. No need to rerun build script if
        // RUSTC_BOOTSTRAP is changed.
        proc_macro_span = true;
        consider_rustc_bootstrap = false;
    } else if let Some(rustc_bootstrap) = env::var_os("RUSTC_BOOTSTRAP") {
        if compile_probe_unstable("proc_macro_span", true) {
            // This is a stable or beta compiler for which the user has set
            // RUSTC_BOOTSTRAP to turn on unstable features. Rerun build script
            // if they change it.
            proc_macro_span = true;
            consider_rustc_bootstrap = true;
        } else if rustc_bootstrap == "1" {
            // This compiler does not support the proc macro Span API in the
            // form that proc-macro2 expects. No need to pay attention to
            // RUSTC_BOOTSTRAP.
            proc_macro_span = false;
            consider_rustc_bootstrap = false;
        } else {
            // This is a stable or beta compiler for which RUSTC_BOOTSTRAP is
            // set to restrict the use of unstable features by this crate.
            proc_macro_span = false;
            consider_rustc_bootstrap = true;
        }
    } else {
        // Without RUSTC_BOOTSTRAP, this compiler does not support the proc
        // macro Span API in the form that proc-macro2 expects, but try again if
        // the user turns on unstable features.
        proc_macro_span = false;
        consider_rustc_bootstrap = true;
    }

    if proc_macro_span || !semver_exempt {
        // Wrap types from libproc_macro rather than polyfilling the whole API.
        // Enabled as long as procmacro2_semver_exempt is not set, because we
        // can't emulate the unstable API without emulating everything else.
        // Also enabled unconditionally on nightly, in which case the
        // procmacro2_semver_exempt surface area is implemented by using the
        // nightly-only proc_macro API.
        println!("cargo:rustc-cfg=wrap_proc_macro");
    }

    if proc_macro_span {
        // Enable non-dummy behavior of Span::byte_range and Span::join methods
        // which requires an unstable compiler feature. Enabled when building
        // with nightly, unless `-Z allow-feature` in RUSTFLAGS disallows
        // unstable features.
        println!("cargo:rustc-cfg=proc_macro_span");
    }

    if proc_macro_span || (rustc >= 88 && compile_probe_stable("proc_macro_span_location")) {
        // Enable non-dummy behavior of Span::start and Span::end methods on
        // Rust 1.88+.
        println!("cargo:rustc-cfg=proc_macro_span_location");
    }

    if proc_macro_span || (rustc >= 88 && compile_probe_stable("proc_macro_span_file")) {
        // Enable non-dummy behavior of Span::file and Span::local_file methods
        // on Rust 1.88+.
        println!("cargo:rustc-cfg=proc_macro_span_file");
    }

    if semver_exempt && proc_macro_span {
        // Implement the semver exempt API in terms of the nightly-only
        // proc_macro API.
        println!("cargo:rustc-cfg=super_unstable");
    }

    if consider_rustc_bootstrap {
        println!("cargo:rerun-if-env-changed=RUSTC_BOOTSTRAP");
    }
}

fn compile_probe_unstable(feature: &str, rustc_bootstrap: bool) -> bool {
    // RUSTC_STAGE indicates that this crate is being compiled as a dependency
    // of a multistage rustc bootstrap. This environment uses Cargo in a highly
    // non-standard way with issues such as:
    //
    //     https://github.com/rust-lang/cargo/issues/11138
    //     https://github.com/rust-lang/rust/issues/114839
    //
    env::var_os("RUSTC_STAGE").is_none() && do_compile_probe(feature, rustc_bootstrap)
}

fn compile_probe_stable(feature: &str) -> bool {
    env::var_os("RUSTC_STAGE").is_some() || do_compile_probe(feature, true)
}

fn do_compile_probe(feature: &str, rustc_bootstrap: bool) -> bool {
    println!("cargo:rerun-if-changed=src/probe/{}.rs", feature);

    let rustc = cargo_env_var("RUSTC");
    let out_dir = cargo_env_var("OUT_DIR");
    let out_subdir = Path::new(&out_dir).join("probe");
    let probefile = Path::new("src")
        .join("probe")
        .join(feature)
        .with_extension("rs");

    if let Err(err) = fs::create_dir(&out_subdir) {
        if err.kind() != ErrorKind::AlreadyExists {
            eprintln!("Failed to create {}: {}", out_subdir.display(), err);
            process::exit(1);
        }
    }

    let rustc_wrapper = env::var_os("RUSTC_WRAPPER").filter(|wrapper| !wrapper.is_empty());
    let rustc_workspace_wrapper =
        env::var_os("RUSTC_WORKSPACE_WRAPPER").filter(|wrapper| !wrapper.is_empty());
    let mut rustc = rustc_wrapper
        .into_iter()
        .chain(rustc_workspace_wrapper)
        .chain(iter::once(rustc));
    let mut cmd = Command::new(rustc.next().unwrap());
    cmd.args(rustc);

    if !rustc_bootstrap {
        cmd.env_remove("RUSTC_BOOTSTRAP");
    }

    cmd.stderr(Stdio::null())
        .arg("--cfg=procmacro2_build_probe")
        .arg("--edition=2021")
        .arg("--crate-name=proc_macro2")
        .arg("--crate-type=lib")
        .arg("--cap-lints=allow")
        .arg("--emit=dep-info,metadata")
        .arg("--cap-lints=allow")
        .arg("--out-dir")
        .arg(&out_subdir)
        .arg(probefile);

    if let Some(target) = env::var_os("TARGET") {
        cmd.arg("--target").arg(target);
    }

    // If Cargo wants to set RUSTFLAGS, use that.
    if let Ok(rustflags) = env::var("CARGO_ENCODED_RUSTFLAGS") {
        if !rustflags.is_empty() {
            for arg in rustflags.split('\x1f') {
                cmd.arg(arg);
            }
        }
    }

    let success = match cmd.status() {
        Ok(status) => status.success(),
        Err(_) => false,
    };

    // Clean up to avoid leaving nondeterministic absolute paths in the dep-info
    // file in OUT_DIR, which causes nonreproducible builds in build systems
    // that treat the entire OUT_DIR as an artifact.
    if let Err(err) = fs::remove_dir_all(&out_subdir) {
        // libc::ENOTEMPTY
        // Some filesystems (NFSv3) have timing issues under load where '.nfs*'
        // dummy files can continue to get created for a short period after the
        // probe command completes, breaking remove_dir_all.
        // To be replaced with ErrorKind::DirectoryNotEmpty (Rust 1.83+).
        const ENOTEMPTY: i32 = 39;

        if !(err.kind() == ErrorKind::NotFound
            || (cfg!(target_os = "linux") && err.raw_os_error() == Some(ENOTEMPTY)))
        {
            eprintln!("Failed to clean up {}: {}", out_subdir.display(), err);
            process::exit(1);
        }
    }

    success
}

fn rustc_minor_version() -> Option<u32> {
    let rustc = cargo_env_var("RUSTC");
    let output = Command::new(rustc).arg("--version").output().ok()?;
    let version = str::from_utf8(&output.stdout).ok()?;
    let mut pieces = version.split('.');
    if pieces.next() != Some("rustc 1") {
        return None;
    }
    pieces.next()?.parse().ok()
}

fn cargo_env_var(key: &str) -> OsString {
    env::var_os(key).unwrap_or_else(|| {
        eprintln!(
            "Environment variable ${} is not set during execution of build script",
            key,
        );
        process::exit(1);
    })
}
