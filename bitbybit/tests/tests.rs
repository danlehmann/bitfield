#[test]
fn all_tests() {
    let t = trybuild::TestCases::new();

    // tests that pass
    t.pass("tests/basic.rs");
    t.pass("tests/with_fields.rs");
    t.pass("tests/correct.rs");
    t.pass("tests/builder_value_field.rs");

    t.compile_fail("tests/no_compile/*.rs");
    t.compile_fail("tests/no_compile/invalid_bit_pos/*.rs");
}
