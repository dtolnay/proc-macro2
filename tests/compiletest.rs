extern crate compiletest_rs as compiletest;

fn run_mode(mode: &'static str) {
    let mut config = compiletest::default_config();
    config.mode = mode.parse().expect("invalid mode");
    config.target_rustcflags = Some("-L target/debug/deps".to_owned());
    config.src_base = format!("tests/{}", mode).into();
    compiletest::run_tests(&config);
}

#[test]
fn compile_fail() {
    run_mode("compile-fail");
}
