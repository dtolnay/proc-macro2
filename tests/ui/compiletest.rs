#[rustversion::attr(not(nightly), ignore = "requires nightly")]
#[rustversion::attr(nightly, cfg_attr(miri, ignore = "incompatible with miri"))]
#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("test-*.rs");
}
