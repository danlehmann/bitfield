#[test]
fn all_tests() {
    let t = trybuild::TestCases::new();

    // tests that pass
    t.pass("tests/basic.rs");
    t.pass("tests/with_fields.rs");

    t.compile_fail("tests/no_compile/exhaustive_bitenum.rs");
    t.compile_fail("tests/no_compile/non_exhaustive_bitenum.rs");
    t.compile_fail("tests/no_compile/invalid_field_range.rs");
    t.compile_fail("tests/no_compile/invalid_field_access.rs");
}
