use arbitrary_int::{u2, u63};
use bitbybit::bitenum;

#[test]
fn bitrange_with_enum_type_exhaustive_2() {
    #[bitenum(u2, exhaustive: true)]
    #[derive(Eq, PartialEq, Debug)]
    enum Foo {
        Zero = 0b00,
        One = 0b01,
        Two = 0b10,
        Three = 0b11,
    }

    assert_eq!(Foo::new_with_raw_value(u2::new(0)), Foo::Zero);
    assert_eq!(Foo::new_with_raw_value(u2::new(1)), Foo::One);
    assert_eq!(Foo::new_with_raw_value(u2::new(2)), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(u2::new(3)), Foo::Three);

    assert_eq!(Foo::Zero.raw_value(), u2::new(0));
    assert_eq!(Foo::One.raw_value(), u2::new(1));
}

#[test]
fn bitrange_with_enum_type_nonexhaustive_2() {
    #[bitenum(u2, exhaustive: false)]
    #[derive(Eq, PartialEq, Debug)]
    enum Foo {
        Zero = 0b00,
        One = 0b01,
        Two = 0b10,
    }

    assert_eq!(Foo::new_with_raw_value(u2::new(0)), Ok(Foo::Zero));
    assert_eq!(Foo::new_with_raw_value(u2::new(1)), Ok(Foo::One));
    assert_eq!(Foo::new_with_raw_value(u2::new(2)), Ok(Foo::Two));
    assert_eq!(Foo::new_with_raw_value(u2::new(3)), Err(3));

    assert_eq!(Foo::Zero.raw_value(), u2::new(0));
    assert_eq!(Foo::One.raw_value(), u2::new(1));
    assert_eq!(Foo::Two.raw_value(), u2::new(2));
}

#[test]
fn enum_without_other_derives() {
    #[bitenum(u2, exhaustive: true)]
    enum Foo {
        Zero = 0,
        One = 1,
        Two = 2,
        Three = 3,
    }

    assert_eq!(Foo::new_with_raw_value(u2::new(0)).raw_value().value(), 0);
    assert_eq!(Foo::new_with_raw_value(u2::new(2)).raw_value().value(), 2);

    assert_eq!(Foo::Two.raw_value().value(), 2);
    assert_eq!(Foo::Three.raw_value().value(), 3);
}

#[test]
fn enum_with_8bits() {
    #[bitenum(u8, exhaustive: false)]
    #[derive(Eq, PartialEq, Debug)]
    enum Foo {
        Zero = 0,
        One = 255,
        Two = 2,
        Three = 3,
    }

    assert_eq!(Foo::new_with_raw_value(0).unwrap(), Foo::Zero);
    assert_eq!(Foo::new_with_raw_value(255).unwrap(), Foo::One);
    assert_eq!(Foo::new_with_raw_value(2).unwrap(), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(3).unwrap(), Foo::Three);
    assert_eq!(Foo::new_with_raw_value(4), Err(4));
    assert_eq!(Foo::new_with_raw_value(254), Err(254));

    assert_eq!(Foo::Zero.raw_value(), 0);
    assert_eq!(Foo::One.raw_value(), 255);
}

#[test]
fn enum_with_16bits() {
    #[bitenum(u16, exhaustive: false)]
    #[derive(Eq, PartialEq, Debug)]
    enum Foo {
        Zero = 0,
        One = 65535,
        Two = 2,
        Three = 3,
    }

    assert_eq!(Foo::new_with_raw_value(0).unwrap(), Foo::Zero);
    assert_eq!(Foo::new_with_raw_value(65535).unwrap(), Foo::One);
    assert_eq!(Foo::new_with_raw_value(2).unwrap(), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(3).unwrap(), Foo::Three);
    assert_eq!(Foo::new_with_raw_value(4), Err(4));
    assert_eq!(Foo::new_with_raw_value(255), Err(255));

    assert_eq!(Foo::Zero.raw_value(), 0);
    assert_eq!(Foo::One.raw_value(), 65535);
}

#[test]
fn enum_with_32bits() {
    #[bitenum(u32, exhaustive: false)]
    #[derive(Eq, PartialEq, Debug)]
    enum Foo {
        Zero = 0,
        One = 0xFFFFFFFF,
        Two = 2,
        Three = 3,
    }

    assert_eq!(Foo::new_with_raw_value(0).unwrap(), Foo::Zero);
    assert_eq!(Foo::new_with_raw_value(0xFFFFFFFF).unwrap(), Foo::One);
    assert_eq!(Foo::new_with_raw_value(2).unwrap(), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(3).unwrap(), Foo::Three);
    assert_eq!(Foo::new_with_raw_value(4), Err(4));
    assert_eq!(Foo::new_with_raw_value(255), Err(255));

    assert_eq!(Foo::Zero.raw_value(), 0);
    assert_eq!(Foo::One.raw_value(), 0xFFFFFFFF);
}

#[test]
fn enum_with_63bits() {
    #[bitenum(u63, exhaustive: false)]
    #[derive(Eq, PartialEq, Debug)]
    #[repr(u64)]
    enum Foo {
        Zero = 0,
        One = 0x7FFF_FFFF_FFFF_FFFF,
        Two = 2,
        Three = 3,
    }

    assert_eq!(Foo::new_with_raw_value(u63::new(0)).unwrap(), Foo::Zero);
    assert_eq!(
        Foo::new_with_raw_value(u63::new(0x7FFF_FFFF_FFFF_FFFF)).unwrap(),
        Foo::One
    );
    assert_eq!(Foo::new_with_raw_value(u63::new(2)).unwrap(), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(u63::new(3)).unwrap(), Foo::Three);
    assert_eq!(Foo::new_with_raw_value(u63::new(4)), Err(4));
    assert_eq!(Foo::new_with_raw_value(u63::new(255)), Err(255));

    assert_eq!(Foo::Zero.raw_value(), u63::new(0));
    assert_eq!(Foo::One.raw_value(), u63::new(0x7FFF_FFFF_FFFF_FFFF));
}

#[test]
fn enum_with_64bits() {
    #[bitenum(u64, exhaustive: false)]
    #[derive(Eq, PartialEq, Debug)]
    #[repr(u64)]
    enum Foo {
        Zero = 0,
        One = 0xFFFF_FFFF_FFFF_FFFF,
        Two = 2,
        Three = 3,
    }

    assert_eq!(Foo::new_with_raw_value(0).unwrap(), Foo::Zero);
    assert_eq!(
        Foo::new_with_raw_value(0xFFFF_FFFF_FFFF_FFFF).unwrap(),
        Foo::One
    );
    assert_eq!(Foo::new_with_raw_value(2).unwrap(), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(3).unwrap(), Foo::Three);
    assert_eq!(Foo::new_with_raw_value(4), Err(4));
    assert_eq!(Foo::new_with_raw_value(255), Err(255));

    assert_eq!(Foo::Zero.raw_value(), 0);
    assert_eq!(Foo::One.raw_value(), 0xFFFF_FFFF_FFFF_FFFF);
}

#[test]
fn documentation() {
    /// This is a comment for the whole enum
    #[bitenum(u2, exhaustive: true)]
    #[derive(Eq, PartialEq, Debug)]
    enum Foo {
        /// Zero is the absence of stuff
        Zero = 0b00,

        // Double-slash shouldn't result in a comment
        One = 0b01,
        Two = 0b10,
        Three = 0b11,
    }

    assert_eq!(Foo::new_with_raw_value(u2::new(0)), Foo::Zero);
    assert_eq!(Foo::new_with_raw_value(u2::new(1)), Foo::One);
    assert_eq!(Foo::new_with_raw_value(u2::new(2)), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(u2::new(3)), Foo::Three);

    assert_eq!(Foo::Zero.raw_value(), u2::new(0));
    assert_eq!(Foo::One.raw_value(), u2::new(1));
}

/// Ensures that conditional statements in enum values are handled correctly
/// (i.e. they don't result in a compile error). We need to specify the new
/// mode exhaustive: conditional to ensure that the macro doesn't actually check
/// and simply treats it like a non-exhaustive enum.
#[test]
fn cfg_in_enum_values() {
    #[bitenum(u2, exhaustive: conditional)]
    #[derive(Eq, PartialEq, Debug)]
    enum Foo {
        /// Zero is the absence of stuff
        Zero = 0b00,

        // Double-slash shouldn't result in a comment
        #[cfg(test)]
        OneTest = 0b01,

        // Double-slash shouldn't result in a comment
        #[cfg(not(test))]
        OneNotTest = 0b01,

        #[cfg(feature = "test123")]
        Test123 = 0b10,

        #[cfg(not(feature = "test123"))]
        TestNot123 = 0b11,
    }

    assert_eq!(Foo::new_with_raw_value(u2::new(0)), Ok(Foo::Zero));
    #[cfg(test)]
    assert_eq!(Foo::new_with_raw_value(u2::new(1)), Ok(Foo::OneTest));
    #[cfg(not(test))]
    assert_eq!(Foo::new_with_raw_value(u2::new(1)), Ok(Foo::OneNotTest));

    #[cfg(feature = "test123")]
    assert_eq!(Foo::new_with_raw_value(u2::new(2)), Ok(Foo::Test123));
    #[cfg(not(feature = "test123"))]
    assert_eq!(Foo::new_with_raw_value(u2::new(2)), Err(2));

    #[cfg(not(feature = "test123"))]
    assert_eq!(Foo::new_with_raw_value(u2::new(3)), Ok(Foo::TestNot123));
    #[cfg(feature = "test123")]
    assert_eq!(Foo::new_with_raw_value(u2::new(3)), Err(3));
}

#[test]
fn enum_with_range_u8() {
    #[derive(Debug, PartialEq)]
    #[bitenum(u8, exhaustive: false)]
    enum Foo {
        Variant1 = 0x05,

        #[range(0x10..=0x3D)]
        VariantRangeHex(u8),

        Variant2 = 73,

        #[range(100..=150)]
        VariantRangeDec(u8),
    }

    assert_eq!(Foo::new_with_raw_value(0x05), Ok(Foo::Variant1));
    assert_eq!(Foo::new_with_raw_value(73), Ok(Foo::Variant2));
    assert_eq!(Foo::new_with_raw_value(0x09), Err(0x09));
    assert_eq!(Foo::new_with_raw_value(0x12), Ok(Foo::VariantRangeHex(0x12)));
    assert_eq!(Foo::new_with_raw_value(90), Err(90));
    assert_eq!(Foo::new_with_raw_value(113), Ok(Foo::VariantRangeDec(113)));
}

#[test]
fn enum_with_range_u3() {
    #[derive(Debug, PartialEq)]
    #[bitenum(u2, exhaustive: false)]
    enum Foo {
        Variant = 0x1,
        #[range(0x2..=0x3)]
        VariantRange(u2),
    }

    assert_eq!(Foo::new_with_raw_value(u2::new(0x1)), Ok(Foo::Variant));
    assert_eq!(Foo::new_with_raw_value(u2::new(0x0)), Err(0x0));
    assert_eq!(Foo::new_with_raw_value(u2::new(0x2)), Ok(Foo::VariantRange(u2::new(0x2))));
}

#[test]
fn enum_with_range_multiple() {
    #[derive(Debug, PartialEq)]
    #[bitenum(u8, exhaustive: false)]
    enum Foo {
        Variant = 0x40,

        #[range(0x20..=0x30)]
        #[range(0x50..=0x60)]
        VariantRange(u8),
    }

    assert_eq!(Foo::new_with_raw_value(0x25), Ok(Foo::VariantRange(0x25)));
    assert_eq!(Foo::new_with_raw_value(0x40), Ok(Foo::Variant));
    assert_eq!(Foo::new_with_raw_value(0x55), Ok(Foo::VariantRange(0x55)));
    assert_eq!(Foo::new_with_raw_value(0x65), Err(0x65));
}

#[test]
fn enum_with_range_catchall() {
    #[derive(Debug, PartialEq)]
    #[bitenum(u8, exhaustive: true)]
    enum Foo {
        Variant = 0x40,

        #[catchall]
        Catchall(u8),

        #[range(0x20..=0x30)]
        #[range(0x50..=0x60)]
        VariantRange(u8),
    }

    assert_eq!(Foo::new_with_raw_value(0x25), Foo::VariantRange(0x25));
    assert_eq!(Foo::new_with_raw_value(0x40), Foo::Variant);
    assert_eq!(Foo::new_with_raw_value(0x55), Foo::VariantRange(0x55));
    assert_eq!(Foo::new_with_raw_value(0x65), Foo::Catchall(0x65));
}

#[test]
fn enum_with_range_exhaustive_no_catchall() {
    #[derive(Debug, PartialEq)]
    #[bitenum(u8, exhaustive: true)]
    enum Foo {
        #[range(0x00..=0x20)]
        #[range(0x50..=0x60)]
        VariantRanges1(u8),

        #[range(0x21..=0x4F)]
        #[range(0x61..=0xFF)]
        VariantRanges2(u8),
    }

    assert_eq!(Foo::new_with_raw_value(0x10), Foo::VariantRanges1(0x10));
    assert_eq!(Foo::new_with_raw_value(0x30), Foo::VariantRanges2(0x30));
    assert_eq!(Foo::new_with_raw_value(0x50), Foo::VariantRanges1(0x50));
    assert_eq!(Foo::new_with_raw_value(0x70), Foo::VariantRanges2(0x70));
}