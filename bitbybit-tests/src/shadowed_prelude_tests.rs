//! The generated code must not depend on what the names it uses resolve to at the call site.
//! Every item, trait and macro from the prelude or `core` that the macros emit is shadowed here by
//! something incompatible, so any unqualified use in the generated code fails to compile.

#![allow(dead_code, unused_imports, unused_macros)]

use arbitrary_int::{u3, u4};

mod core {}

type Result<T> = ::core::result::Result<T, ()>;

pub enum Shadowed {
    Ok,
    Err,
}
use Shadowed::{Err, Ok};

trait Default {}

macro_rules! assert {
    ($($t:tt)*) => {
        compile_error!("generated code used an unqualified `assert!`")
    };
}
macro_rules! unreachable {
    ($($t:tt)*) => {
        compile_error!("generated code used an unqualified `unreachable!`")
    };
}
macro_rules! stringify {
    ($($t:tt)*) => {
        compile_error!("generated code used an unqualified `stringify!`")
    };
}

#[bitbybit::bitenum(u8, exhaustive = false)]
#[derive(Debug, PartialEq)]
enum NonExhaustive {
    A = 1,
    B = 2,
}

#[bitbybit::bitenum(u3, exhaustive = false)]
#[derive(Debug, PartialEq)]
enum NonExhaustiveArbitrary {
    A = 1,
    B = 2,
}

#[bitbybit::bitenum(u1, exhaustive = true)]
#[derive(Debug, PartialEq)]
enum Exhaustive {
    A = 0,
    B = 1,
}

#[bitbybit::bitfield(u32, default = 0, debug, introspect)]
struct Fields {
    #[bits(0..=3, rw)]
    nibble: u4,

    #[bits(4..=11, rw)]
    kind: Option<NonExhaustive>,

    #[bit(12, rw)]
    exhaustive: Exhaustive,

    #[bits(13..=15, rw)]
    arbitrary: Option<NonExhaustiveArbitrary>,

    #[bit(16, rw, stride = 1)]
    flags: [bool; 4],

    #[bits([20..=21, 24..=25], rw)]
    split: u4,
}

#[test]
fn bitenum_ignores_shadowed_names() {
    ::core::assert_eq!(
        NonExhaustive::new_with_raw_value(2),
        ::core::result::Result::Ok(NonExhaustive::B)
    );
    ::core::assert_eq!(
        NonExhaustive::new_with_raw_value(3),
        ::core::result::Result::Err(3)
    );
    ::core::assert_eq!(
        NonExhaustiveArbitrary::new_with_raw_value(u3::new(5)),
        ::core::result::Result::Err(u3::new(5))
    );
    ::core::assert_eq!(
        Exhaustive::new_with_raw_value(arbitrary_int::u1::new(1)),
        Exhaustive::B
    );
}

#[test]
fn bitfield_ignores_shadowed_names() {
    let fields = <Fields as ::core::default::Default>::default()
        .with_kind(NonExhaustive::A)
        .with_flags(2, true)
        .with_split(u4::new(0b1001));
    ::core::assert_eq!(fields.kind(), ::core::result::Result::Ok(NonExhaustive::A));
    ::core::assert!(fields.flags(2));
    ::core::assert_eq!(fields.split(), u4::new(0b1001));
    ::core::assert_eq!(Fields::FLAGS_COUNT, 4);
    ::core::assert_eq!(Fields::NIBBLE_BITS, 0..=3);
    ::core::assert!(::std::format!("{fields:?}").starts_with("Fields {"));
}
