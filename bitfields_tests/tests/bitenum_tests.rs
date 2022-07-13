use bitfields::bitenum;
use arbitrary_int::{u2, u63};

#[test]
fn bitrange_with_enum_type_exhaustive_2() {
    #[bitenum(bits: 2, exhaustive: true)]
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
    #[bitenum(bits: 2, exhaustive: false)]
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
    #[bitenum(bits: 2, exhaustive: true)]
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
    #[bitenum(bits: 8, exhaustive: false)]
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
    #[bitenum(bits: 16, exhaustive: false)]
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
    #[bitenum(bits: 32, exhaustive: false)]
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
    #[bitenum(bits: 63, exhaustive: false)]
    #[derive(Eq, PartialEq, Debug)]
    #[repr(u64)]
    enum Foo {
        Zero = 0,
        One = 0x7FFFFFFF_FFFFFFFF,
        Two = 2,
        Three = 3,
    }

    assert_eq!(Foo::new_with_raw_value(u63::new(0)).unwrap(), Foo::Zero);
    assert_eq!(Foo::new_with_raw_value(u63::new(0x7FFFFFFF_FFFFFFFF)).unwrap(), Foo::One);
    assert_eq!(Foo::new_with_raw_value(u63::new(2)).unwrap(), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(u63::new(3)).unwrap(), Foo::Three);
    assert_eq!(Foo::new_with_raw_value(u63::new(4)), Err(4));
    assert_eq!(Foo::new_with_raw_value(u63::new(255)), Err(255));

    assert_eq!(Foo::Zero.raw_value(), u63::new(0));
    assert_eq!(Foo::One.raw_value(), u63::new(0x7FFFFFFF_FFFFFFFF));
}

#[test]
fn enum_with_64bits() {
    #[bitenum(bits: 64, exhaustive: false)]
    #[derive(Eq, PartialEq, Debug)]
    #[repr(u64)]
    enum Foo {
        Zero = 0,
        One = 0xFFFFFFFF_FFFFFFFF,
        Two = 2,
        Three = 3,
    }

    assert_eq!(Foo::new_with_raw_value(0).unwrap(), Foo::Zero);
    assert_eq!(Foo::new_with_raw_value(0xFFFFFFFF_FFFFFFFF).unwrap(), Foo::One);
    assert_eq!(Foo::new_with_raw_value(2).unwrap(), Foo::Two);
    assert_eq!(Foo::new_with_raw_value(3).unwrap(), Foo::Three);
    assert_eq!(Foo::new_with_raw_value(4), Err(4));
    assert_eq!(Foo::new_with_raw_value(255), Err(255));

    assert_eq!(Foo::Zero.raw_value(), 0);
    assert_eq!(Foo::One.raw_value(), 0xFFFFFFFF_FFFFFFFF);
}
