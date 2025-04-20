use arbitrary_int::Number;
use std::fmt::Debug;

use arbitrary_int::{u1, u12, u13, u14, u2, u24, u3, u30, u4, u48, u5, u57, u7};
use bitbybit::bitenum;
use bitbybit::bitfield;

#[test]
fn test_construction() {
    #[bitfield(u32, default = 0)]
    struct Test2 {}

    let t = Test2::DEFAULT;
    assert_eq!(0, t.raw_value);

    let t = Test2::new_with_raw_value(45);
    assert_eq!(45, t.raw_value);
}

#[test]
fn test_zero() {
    #[bitfield(u32)]
    struct TestA {}

    #[bitfield(u32, default = 0x123)]
    struct TestB {}

    assert_eq!(0, TestA::ZERO.raw_value());
    assert_eq!(0, TestB::ZERO.raw_value());
}

#[test]
fn test_getter_and_with() {
    #[bitfield(u128, default = 0)]
    struct Test2 {
        #[bits(98..=127, rw)]
        val30: u30,

        #[bits(41..=97, rw)]
        val57: u57,

        #[bits(28..=40, rw)]
        val13: u13,

        #[bits(12..=27, rw)]
        val16: u16,

        #[bits(4..=11, rw)]
        baudrate: u8,

        #[bits(0..=3, rw)]
        some_other_bits: u4,
    }

    let t = Test2::new_with_raw_value(0xAE42_315A_2134_FE06_3412_345A_2134_FE06);
    assert_eq!(u30::new(0x2B908C56), t.val30());
    assert_eq!(u57::new(0x0110_9A7F_031A_091A), t.val57());
    assert_eq!(u13::new(0x5A2), t.val13());
    assert_eq!(0x134F, t.val16());
    assert_eq!(0xE0, t.baudrate());
    assert_eq!(u4::new(0x6), t.some_other_bits());

    let t = Test2::DEFAULT
        .with_baudrate(0x12)
        .with_some_other_bits(u4::new(0x2));
    assert_eq!(0x12, t.baudrate());
    assert_eq!(u4::new(0x2), t.some_other_bits());
    assert_eq!(0x0122, t.raw_value);
}

#[test]
fn test_getter_and_with_arbitrary_uint() {
    #[bitfield(u128, default = 0)]
    struct Test2 {
        #[bits(4..=11, rw)]
        baudrate: u8,

        #[bits(0..=3, rw)]
        some_other_bits: u4,
    }

    let t = Test2::new_with_raw_value(0xFE06);
    assert_eq!(0xE0, t.baudrate());
    assert_eq!(u4::new(0x6), t.some_other_bits());

    let t = Test2::DEFAULT
        .with_baudrate(0x12)
        .with_some_other_bits(u4::new(0x2));
    assert_eq!(0x12, t.baudrate());
    assert_eq!(u4::new(0x2), t.some_other_bits());
    assert_eq!(0x0122, t.raw_value);
}

#[test]
fn test_bool() {
    #[bitfield(u16, default = 0)]
    struct Test {
        #[bit(0, rw)]
        bit0: bool,

        #[bit(1, rw)]
        bit1: bool,

        #[bits(2..=9, r)]
        n: u8,

        #[bits(10..=12, w)]
        m: u3,

        #[bits(13..=15, r)]
        o: u3,
    }

    let t = Test::DEFAULT;
    assert!(!t.bit0());
    assert!(!t.bit1());
    assert_eq!(t.raw_value, 0b00);

    let t2 = t.with_bit0(true);
    assert!(t2.bit0());
    assert!(!t2.bit1());
    assert_eq!(t2.raw_value, 0b01);

    let t3 = t.with_bit1(true);
    assert!(!t3.bit0());
    assert!(t3.bit1());
    assert_eq!(t3.raw_value, 0b10);
}

#[test]
fn test_u1() {
    #[bitfield(u16, default = 0)]
    struct Test {
        #[bit(0, rw)]
        bit0: u1,

        #[bit(1, rw)]
        bit1: u1,

        #[bits(2..=9, r)]
        n: u8,

        #[bits(10..=12, w)]
        m: u3,

        #[bits(13..=15, r)]
        o: u3,
    }

    let t = Test::DEFAULT;
    assert_eq!(t.bit0(), u1::new(0));
    assert_eq!(t.bit1(), u1::new(0));
    assert_eq!(t.raw_value, 0b00);

    let t2 = t.with_bit0(u1::new(1));
    assert_eq!(t2.bit0(), u1::new(1));
    assert_eq!(t2.bit1(), u1::new(0));
    assert_eq!(t2.raw_value, 0b01);

    let t3 = t.with_bit1(u1::new(1));
    assert_eq!(t3.bit0(), u1::new(0));
    assert_eq!(t3.bit1(), u1::new(1));
    assert_eq!(t3.raw_value, 0b10);
}

#[test]
fn signed_vs_unsigned() {
    #[bitfield(u32)]
    struct Test {
        #[bits(0..=7, rw)]
        unsigned1: u8,

        #[bits(8..=15, rw)]
        unsigned2: u8,

        #[bits(16..=23, rw)]
        signed1: i8,

        #[bits(24..=31, rw)]
        signed2: i8,
    }

    let t = Test::new_with_raw_value(0x7FFF7FFF);
    assert_eq!(t.unsigned1(), 255);
    assert_eq!(t.unsigned2(), 127);
    assert_eq!(t.signed1(), -1);
    assert_eq!(t.signed2(), 127);

    let t2 = Test::new_with_raw_value(0)
        .with_unsigned1(0x7Fu8)
        .with_unsigned2(0xFFu8)
        .with_signed1(127)
        .with_signed2(-1);
    assert_eq!(t2.raw_value, 0xFF7FFF7F);
}

#[test]
fn signed_masking8and16() {
    #[bitfield(u32)]
    struct Test {
        #[bits(24..=31, rw)]
        signed2: i8,

        #[bits(16..=23, rw)]
        signed1: i8,

        #[bits(0..=15, rw)]
        signed0: i16,
    }

    let t = Test::builder()
        .with_signed2(-5)
        .with_signed1(-100)
        .with_signed0(-32012)
        .build();
    assert_eq!(t.signed2(), -5);
    assert_eq!(t.signed1(), -100);
    assert_eq!(t.signed0(), -32012);
}

#[test]
fn signed_masking32and64() {
    #[bitfield(u128, default = 0)]
    struct Test {
        #[bits(32..=95, rw)]
        signed1: i64,

        #[bits(0..=31, rw)]
        signed0: i32,
    }

    let t = Test::builder()
        .with_signed1(-100000000)
        .with_signed0(-3500012)
        .build();
    assert_eq!(t.signed1(), -100000000);
    assert_eq!(t.signed0(), -3500012);
}

#[test]
fn signed_masking128() {
    #[bitfield(u128, default = 0)]
    struct Test {
        #[bits(0..=127, rw)]
        signed0: i128,
    }

    let t = Test::builder().with_signed0(-3500012).build();
    assert_eq!(t.signed0(), -3500012);
}

#[test]
fn signed_masking_array() {
    #[bitfield(u128, default = 0)]
    struct Test {
        #[bits(96..=127, rw)]
        signed3: i32,

        #[bits(0..=31, rw)]
        signed: [i32; 3],
    }

    let t = Test::DEFAULT
        .with_signed3(-13)
        .with_signed(0, -10)
        .with_signed(2, -12)
        .with_signed(1, -11);
    assert_eq!(t.signed(0), -10);
    assert_eq!(t.signed(1), -11);
    assert_eq!(t.signed(2), -12);
    assert_eq!(t.signed3(), -13);

    let t = Test::builder()
        .with_signed3(-13)
        .with_signed([-10, -11, -12])
        .build();
    assert_eq!(t.signed(0), -10);
    assert_eq!(t.signed(1), -11);
    assert_eq!(t.signed(2), -12);
    assert_eq!(t.signed3(), -13);
}

#[test]
fn default_value() {
    #[bitfield(u32, default = 0xDEADBEEF)]
    struct Test {}

    let t = Test::DEFAULT;
    assert_eq!(t.raw_value, 0xDEADBEEF);
}

#[test]
fn default_value_const() {
    const DEFAULT: u32 = 0xBADCAFE;
    #[bitfield(u32, default = DEFAULT)]
    struct Test {}

    let t = Test::DEFAULT;
    assert_eq!(t.raw_value, 0xBADCAFE);
}

#[test]
fn more_struct_attributes() {
    // Test that other attributes can be appended
    #[bitfield(u32)]
    #[derive(Debug)]
    struct Test {}

    // If this compiles then derive(Debug) worked
    let _ = format!("{:?}", Test::new_with_raw_value(0));
}

#[test]
fn documentation() {
    /// Test that documentation is properly applied to the output
    #[bitfield(u32, default = 0x123)]
    #[derive(Debug)]
    struct Test {
        /// This is a documented field
        #[bits(8..=15, rw)]
        field_a: u8,

        // A free standing comment - this should not become a doc comment
        #[bits(8..=15, rw)]
        undocumented: u8,

        // Another free standing comment
        /// This is the second documented field
        #[bits(0..=7, rw)]
        field_b: u8,

        // Another free standing comment
        /// Let's try multiple lines
        /// Second line
        #[bits(0..=3, rw)]
        field_c: u4,
    }
}

#[test]
fn proper_unmasking() {
    #[bitfield(u16, default = 0)]
    pub struct TestStruct {
        #[bits(0..=1, rw)]
        a: u2,

        #[bits(2..=3, rw)]
        b: u2,

        #[bits(4..=5, rw)]
        c: u2,
    }

    let s1 = TestStruct::DEFAULT
        .with_a(u2::new(0b11))
        .with_b(u2::new(0b11))
        .with_c(u2::new(0b11));

    assert_eq!(0b111111, s1.raw_value());

    let s2 = s1.with_b(u2::new(0b00));
    assert_eq!(0b110011, s2.raw_value());
}

#[test]
fn just_one_bitrange() {
    #[bitfield(u16, default = 0)]
    pub struct JustOneBitRange {
        #[bits(0..=15, rw)]
        a: i16,
    }

    let s1 = JustOneBitRange::DEFAULT.with_a(0b0111001110001111);

    assert_eq!(0b0111001110001111, s1.raw_value());
    assert_eq!(0b0111001110001111, s1.a());
}

#[test]
fn repeated_bitrange_single_bits_with_stride() {
    #[bitfield(u64, default = 0)]
    pub struct NibbleBits64 {
        #[bit(0, rw, stride = 4)]
        nibble_bit0: [bool; 16],

        #[bit(1, rw, stride = 4)]
        nibble_bit1: [bool; 16],

        #[bit(2, rw, stride = 4)]
        nibble_bit2: [bool; 16],

        #[bit(3, rw, stride = 4)]
        nibble_bit3: [bool; 16],
    }

    const VALUE: u64 = 0x1234_5678_ABCD_EFFF;
    let nibble_bits = NibbleBits64::new_with_raw_value(VALUE);
    assert_eq!(true, nibble_bits.nibble_bit0(0));
    assert_eq!(true, nibble_bits.nibble_bit0(1));
    assert_eq!(true, nibble_bits.nibble_bit0(2));
    assert_eq!(false, nibble_bits.nibble_bit0(3));
    assert_eq!(true, nibble_bits.nibble_bit0(4));
    assert_eq!(false, nibble_bits.nibble_bit0(5));
    assert_eq!(true, nibble_bits.nibble_bit0(6));
    assert_eq!(false, nibble_bits.nibble_bit0(7));

    assert_eq!(false, nibble_bits.nibble_bit0(8));
    assert_eq!(true, nibble_bits.nibble_bit0(9));
    assert_eq!(false, nibble_bits.nibble_bit0(10));
    assert_eq!(true, nibble_bits.nibble_bit0(11));
    assert_eq!(false, nibble_bits.nibble_bit0(12));
    assert_eq!(true, nibble_bits.nibble_bit0(13));
    assert_eq!(false, nibble_bits.nibble_bit0(14));
    assert_eq!(true, nibble_bits.nibble_bit0(15));
    assert_eq!(false, nibble_bits.nibble_bit1(15));
    assert_eq!(false, nibble_bits.nibble_bit2(15));
    assert_eq!(false, nibble_bits.nibble_bit3(15));

    assert_eq!(
        0x1234_5678_ABCD_EFFE,
        nibble_bits.with_nibble_bit0(0, false).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_EFEF,
        nibble_bits.with_nibble_bit0(1, false).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_EEFF,
        nibble_bits.with_nibble_bit0(2, false).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_FFFF,
        nibble_bits.with_nibble_bit0(3, true).raw_value()
    );
    assert_eq!(
        0x0234_5678_ABCD_EFFF,
        nibble_bits.with_nibble_bit0(15, false).raw_value()
    );
    assert_eq!(
        0x3234_5678_ABCD_EFFF,
        nibble_bits.with_nibble_bit1(15, true).raw_value()
    );
    assert_eq!(
        0x5234_5678_ABCD_EFFF,
        nibble_bits.with_nibble_bit2(15, true).raw_value()
    );
    assert_eq!(
        0x9234_5678_ABCD_EFFF,
        nibble_bits.with_nibble_bit3(15, true).raw_value()
    );
}

#[test]
fn repeated_bitrange_single_u1_bits_with_stride() {
    #[bitfield(u64, default = 0)]
    pub struct NibbleBits64 {
        #[bit(0, rw, stride = 4)]
        nibble_bit0: [u1; 16],

        #[bit(1, rw, stride = 4)]
        nibble_bit1: [u1; 16],

        #[bit(2, rw, stride = 4)]
        nibble_bit2: [u1; 16],

        #[bit(3, rw, stride = 4)]
        nibble_bit3: [u1; 16],
    }

    const VALUE: u64 = 0x1234_5678_ABCD_EFFF;
    let nibble_bits = NibbleBits64::new_with_raw_value(VALUE);
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(0));
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(1));
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(2));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit0(3));
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(4));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit0(5));
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(6));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit0(7));

    assert_eq!(u1::new(0), nibble_bits.nibble_bit0(8));
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(9));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit0(10));
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(11));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit0(12));
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(13));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit0(14));
    assert_eq!(u1::new(1), nibble_bits.nibble_bit0(15));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit1(15));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit2(15));
    assert_eq!(u1::new(0), nibble_bits.nibble_bit3(15));

    assert_eq!(
        0x1234_5678_ABCD_EFFE,
        nibble_bits.with_nibble_bit0(0, u1::new(0)).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_EFEF,
        nibble_bits.with_nibble_bit0(1, u1::new(0)).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_EEFF,
        nibble_bits.with_nibble_bit0(2, u1::new(0)).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_FFFF,
        nibble_bits.with_nibble_bit0(3, u1::new(1)).raw_value()
    );
    assert_eq!(
        0x0234_5678_ABCD_EFFF,
        nibble_bits.with_nibble_bit0(15, u1::new(0)).raw_value()
    );
    assert_eq!(
        0x3234_5678_ABCD_EFFF,
        nibble_bits.with_nibble_bit1(15, u1::new(1)).raw_value()
    );
    assert_eq!(
        0x5234_5678_ABCD_EFFF,
        nibble_bits.with_nibble_bit2(15, u1::new(1)).raw_value()
    );
    assert_eq!(
        0x9234_5678_ABCD_EFFF,
        nibble_bits.with_nibble_bit3(15, u1::new(1)).raw_value()
    );
}

#[test]
fn repeated_bitrange_single_bits_without_stride() {
    #[bitfield(u8, default = 0)]
    pub struct Bits8 {
        #[bit(0, rw)]
        bit: [bool; 8],
    }

    let bits8 = Bits8::new_with_raw_value(0b0110_1110);
    assert_eq!(false, bits8.bit(0));
    assert_eq!(true, bits8.bit(1));
    assert_eq!(true, bits8.bit(2));
    assert_eq!(true, bits8.bit(3));

    assert_eq!(false, bits8.bit(4));
    assert_eq!(true, bits8.bit(5));
    assert_eq!(true, bits8.bit(6));
    assert_eq!(false, bits8.bit(7));

    assert_eq!(0b0110_0110, bits8.with_bit(3, false).raw_value());
    assert_eq!(0b1110_1110, bits8.with_bit(7, true).raw_value());
}

#[test]
fn repeated_bitrange_single_u1_bits_without_stride() {
    #[bitfield(u8, default = 0)]
    pub struct Bits8 {
        #[bit(0, rw)]
        bit: [u1; 8],
    }

    let bits8 = Bits8::new_with_raw_value(0b0110_1110);
    assert_eq!(u1::new(0), bits8.bit(0));
    assert_eq!(u1::new(1), bits8.bit(1));
    assert_eq!(u1::new(1), bits8.bit(2));
    assert_eq!(u1::new(1), bits8.bit(3));

    assert_eq!(u1::new(0), bits8.bit(4));
    assert_eq!(u1::new(1), bits8.bit(5));
    assert_eq!(u1::new(1), bits8.bit(6));
    assert_eq!(u1::new(0), bits8.bit(7));

    assert_eq!(0b0110_0110, bits8.with_bit(3, u1::new(0)).raw_value());
    assert_eq!(0b1110_1110, bits8.with_bit(7, u1::new(1)).raw_value());
}

#[test]
fn repeated_bitrange_without_stride() {
    #[bitfield(u64, default = 0)]
    pub struct Nibble64 {
        #[bits(0..=3, rw)]
        nibble: [u4; 16],
    }

    const VALUE: u64 = 0x1234_5678_ABCD_EFFF;
    let nibble = Nibble64::new_with_raw_value(VALUE);

    assert_eq!(u4::new(0xF), nibble.nibble(0));
    assert_eq!(u4::new(0xF), nibble.nibble(1));
    assert_eq!(u4::new(0xF), nibble.nibble(2));
    assert_eq!(u4::new(0xE), nibble.nibble(3));
    assert_eq!(u4::new(0xD), nibble.nibble(4));
    assert_eq!(u4::new(0xC), nibble.nibble(5));

    assert_eq!(
        0x1234_5678_ABCD_EFF3,
        nibble.with_nibble(0, u4::new(0x3)).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_EF2F,
        nibble.with_nibble(1, u4::new(0x2)).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_EAFF,
        nibble.with_nibble(2, u4::new(0xA)).raw_value()
    );
    assert_eq!(
        0xE234_5678_ABCD_EFFF,
        nibble.with_nibble(15, u4::new(0xE)).raw_value()
    );
}

#[test]
fn repeated_bitrange_with_stride_equals_width() {
    #[bitfield(u64, default = 0)]
    pub struct Nibble64 {
        #[bits(0..=3, rw, stride = 4)]
        nibble: [u4; 16],
    }

    const VALUE: u64 = 0x1234_5678_ABCD_EFFF;
    let nibble = Nibble64::new_with_raw_value(VALUE);

    assert_eq!(u4::new(0xF), nibble.nibble(0));
    assert_eq!(u4::new(0xF), nibble.nibble(1));
    assert_eq!(u4::new(0xF), nibble.nibble(2));
    assert_eq!(u4::new(0xE), nibble.nibble(3));
    assert_eq!(u4::new(0xD), nibble.nibble(4));
    assert_eq!(u4::new(0xC), nibble.nibble(5));

    assert_eq!(
        0x1234_5678_ABCD_EFF3,
        nibble.with_nibble(0, u4::new(0x3)).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_EF2F,
        nibble.with_nibble(1, u4::new(0x2)).raw_value()
    );
    assert_eq!(
        0x1234_5678_ABCD_EAFF,
        nibble.with_nibble(2, u4::new(0xA)).raw_value()
    );
    assert_eq!(
        0xE234_5678_ABCD_EFFF,
        nibble.with_nibble(15, u4::new(0xE)).raw_value()
    );
}

#[test]
fn repeated_bitrange_with_stride_greater_than_width() {
    #[bitfield(u64, default = 0)]
    pub struct EvenNibble64 {
        #[bits(0..=3, rw, stride = 8)]
        even_nibble: [u4; 8],
    }

    const VALUE: u64 = 0x1234_5678_ABCD_EFFF;
    let even_nibble = EvenNibble64::new_with_raw_value(VALUE);

    assert_eq!(u4::new(0xF), even_nibble.even_nibble(0));
    assert_eq!(u4::new(0xF), even_nibble.even_nibble(1));
    assert_eq!(u4::new(0xD), even_nibble.even_nibble(2));
    assert_eq!(u4::new(0xB), even_nibble.even_nibble(3));
    assert_eq!(u4::new(0x8), even_nibble.even_nibble(4));
    assert_eq!(u4::new(0x6), even_nibble.even_nibble(5));
}

#[test]
fn repeated_bitrange_with_stride_greater_than_width_basic_type() {
    #[bitfield(u64, default = 0)]
    pub struct EvenByte64 {
        #[bits(0..=7, rw, stride = 16)]
        even_byte: [u8; 4],
    }

    const VALUE: u64 = 0x1234_5678_ABCD_EFFF;
    let even_byte = EvenByte64::new_with_raw_value(VALUE);

    assert_eq!(0xFF, even_byte.even_byte(0));
    assert_eq!(0xCD, even_byte.even_byte(1));
    assert_eq!(0x78, even_byte.even_byte(2));
    assert_eq!(0x34, even_byte.even_byte(3));
}

#[test]
fn bitfield_with_enum_exhaustive() {
    #[bitenum(u2, exhaustive = true)]
    #[derive(Eq, PartialEq, Debug)]
    pub enum ExhaustiveEnum {
        Zero = 0b00,
        One = 0b01,
        Two = 0b10,
        Three = 0b11,
    }

    #[bitfield(u64, default = 0)]
    pub struct BitfieldWithEnum {
        #[bits(0..=1, rw)]
        e1: ExhaustiveEnum,
    }

    assert_eq!(
        ExhaustiveEnum::Zero,
        BitfieldWithEnum::new_with_raw_value(0b1100).e1()
    );
    assert_eq!(
        ExhaustiveEnum::One,
        BitfieldWithEnum::new_with_raw_value(0b1101).e1()
    );
    assert_eq!(
        ExhaustiveEnum::Two,
        BitfieldWithEnum::new_with_raw_value(0b1110).e1()
    );
    assert_eq!(
        ExhaustiveEnum::Three,
        BitfieldWithEnum::new_with_raw_value(0b1111).e1()
    );

    assert_eq!(
        0b10,
        BitfieldWithEnum::DEFAULT
            .with_e1(ExhaustiveEnum::Two)
            .raw_value()
    );
    assert_eq!(
        0b11,
        BitfieldWithEnum::DEFAULT
            .with_e1(ExhaustiveEnum::Three)
            .raw_value()
    );
}

#[test]
fn bitfield_with_enum_nonexhaustive() {
    #[bitenum(u2, exhaustive = false)]
    #[derive(Eq, PartialEq, Debug)]
    pub enum NonExhaustiveEnum {
        Zero = 0b00,
        One = 0b01,
        Two = 0b10,
    }

    #[bitfield(u64, default = 0)]
    pub struct BitfieldWithEnumNonExhaustive {
        #[bits(2..=3, rw)]
        e2: Option<NonExhaustiveEnum>,
    }

    assert_eq!(
        Ok(NonExhaustiveEnum::Zero),
        BitfieldWithEnumNonExhaustive::new_with_raw_value(0b0010).e2()
    );
    assert_eq!(
        Ok(NonExhaustiveEnum::One),
        BitfieldWithEnumNonExhaustive::new_with_raw_value(0b0110).e2()
    );
    assert_eq!(
        Ok(NonExhaustiveEnum::Two),
        BitfieldWithEnumNonExhaustive::new_with_raw_value(0b1010).e2()
    );
    assert_eq!(
        Err(3),
        BitfieldWithEnumNonExhaustive::new_with_raw_value(0b1110).e2()
    );

    assert_eq!(
        0b0000,
        BitfieldWithEnumNonExhaustive::DEFAULT
            .with_e2(NonExhaustiveEnum::Zero)
            .raw_value()
    );
    assert_eq!(
        0b0100,
        BitfieldWithEnumNonExhaustive::DEFAULT
            .with_e2(NonExhaustiveEnum::One)
            .raw_value()
    );
    assert_eq!(
        0b1000,
        BitfieldWithEnumNonExhaustive::DEFAULT
            .with_e2(NonExhaustiveEnum::Two)
            .raw_value()
    );
    // No way to set to Three (by design): If an enum member doesn't exist, we shouldn't be
    // able to write it
}

#[test]
fn bitfield_with_indexed_exhaustive_enum() {
    #[bitenum(u2, exhaustive = true)]
    #[derive(Eq, PartialEq, Debug)]
    pub enum ExhaustiveEnum {
        Zero = 0b00,
        One = 0b01,
        Two = 0b10,
        Three = 0b11,
    }

    #[bitfield(u64, default = 0)]
    pub struct BitfieldWithIndexedEnums {
        #[bits(0..=1, rw)]
        exhaustive: [ExhaustiveEnum; 8],
    }

    assert_eq!(
        ExhaustiveEnum::Two,
        BitfieldWithIndexedEnums::new_with_raw_value(0b0010).exhaustive(0)
    );
    assert_eq!(
        ExhaustiveEnum::One,
        BitfieldWithIndexedEnums::new_with_raw_value(0b0110).exhaustive(1)
    );
    assert_eq!(
        ExhaustiveEnum::Zero,
        BitfieldWithIndexedEnums::new_with_raw_value(0b0110).exhaustive(2)
    );

    assert_eq!(
        0b11_01_10,
        BitfieldWithIndexedEnums::new_with_raw_value(0b01_10)
            .with_exhaustive(2, ExhaustiveEnum::Three)
            .raw_value()
    );
}

#[test]
fn bitfield_with_indexed_nonexhaustive_enum() {
    #[bitenum(u2, exhaustive = false)]
    #[derive(Eq, PartialEq, Debug)]
    pub enum NonExhaustiveEnum {
        Zero = 0b00,
        One = 0b01,
        Two = 0b10,
    }

    #[bitfield(u64, default = 0)]
    pub struct BitfieldWithIndexedEnums {
        #[bits(0..=1, rw)]
        nonexhaustive: [Option<NonExhaustiveEnum>; 8],
    }

    assert_eq!(
        Ok(NonExhaustiveEnum::Two),
        BitfieldWithIndexedEnums::new_with_raw_value(0b0010).nonexhaustive(0)
    );
    assert_eq!(
        Ok(NonExhaustiveEnum::One),
        BitfieldWithIndexedEnums::new_with_raw_value(0b0110).nonexhaustive(1)
    );
    assert_eq!(
        Ok(NonExhaustiveEnum::Zero),
        BitfieldWithIndexedEnums::new_with_raw_value(0b0110).nonexhaustive(2)
    );

    assert_eq!(
        0b10_01_10,
        BitfieldWithIndexedEnums::new_with_raw_value(0b01_10)
            .with_nonexhaustive(2, NonExhaustiveEnum::Two)
            .raw_value()
    );
}

#[test]
fn bitfield_with_u8_enum() {
    #[bitenum(u8, exhaustive = false)]
    #[derive(Eq, PartialEq, Debug)]
    pub enum NonExhaustiveEnum {
        Zero = 0b00,
        One = 0b01,
        Two = 0b10000010,
    }

    #[bitfield(u64, default = 0)]
    pub struct BitfieldWithIndexedEnums {
        #[bits(6..=13, rw)]
        val8: Option<NonExhaustiveEnum>,
    }

    assert_eq!(
        Ok(NonExhaustiveEnum::Zero),
        BitfieldWithIndexedEnums::new_with_raw_value(0b0000_0000_0000_0000).val8()
    );
    assert_eq!(
        Ok(NonExhaustiveEnum::One),
        BitfieldWithIndexedEnums::new_with_raw_value(0b0000_0000_0100_0000).val8()
    );
    assert_eq!(
        Ok(NonExhaustiveEnum::Two),
        BitfieldWithIndexedEnums::new_with_raw_value(0b0010_0000_1000_0000).val8()
    );

    assert_eq!(
        Err(0b00100000),
        BitfieldWithIndexedEnums::new_with_raw_value(0b0000_1000_0000_0000).val8()
    );
}

#[test]
fn bitfield_with_singlebit_enum() {
    #[bitenum(u1, exhaustive = true)]
    #[derive(Eq, PartialEq, Debug)]
    enum BitEnum {
        Wrong = 0,
        Correct = 1,
    }

    #[bitfield(u32, default = 0)]
    struct BitfieldWithBitEnum {
        #[bit(6, rw)]
        bit: BitEnum,

        #[bit(8, rw)]
        indexed: [BitEnum; 6],
    }

    // Regular enum
    assert_eq!(
        BitEnum::Wrong,
        BitfieldWithBitEnum::new_with_raw_value(0b0000_0000_0000_0000).bit()
    );
    assert_eq!(
        BitEnum::Correct,
        BitfieldWithBitEnum::new_with_raw_value(0b0000_0000_0100_0000).bit()
    );

    // Indexed
    assert_eq!(
        BitEnum::Correct,
        BitfieldWithBitEnum::new_with_raw_value(0b0001_0001_0000_0000).indexed(0)
    );
    assert_eq!(
        BitEnum::Wrong,
        BitfieldWithBitEnum::new_with_raw_value(0b0001_0001_0000_0000).indexed(1)
    );
    assert_eq!(
        BitEnum::Wrong,
        BitfieldWithBitEnum::new_with_raw_value(0b0001_0001_0000_0000).indexed(2)
    );
    assert_eq!(
        BitEnum::Wrong,
        BitfieldWithBitEnum::new_with_raw_value(0b0001_0001_0000_0000).indexed(3)
    );
    assert_eq!(
        BitEnum::Correct,
        BitfieldWithBitEnum::new_with_raw_value(0b0001_0001_0000_0000).indexed(4)
    );
}

#[test]
fn with_derive_default() {
    // Test that derive(Default) can be specified
    #[bitfield(u32)]
    #[derive(Default)]
    struct Test {}

    // If this compiles then derive(Debug) worked
    let _ = Test::default();
}

#[test]
fn default_value_automatically_implements_default() {
    // Early versions didn't automatically implement Default - ensure that this still compiles
    // (though we emit a compiler warning)
    #[bitfield(u32, default = 2)]
    #[derive(Eq, PartialEq, Debug)]
    struct Test {}

    // If this compiles then derive(Debug) worked
    let a = Test::default();
    let b = Test::DEFAULT;
    assert_eq!(a, b);
}

#[test]
fn new_can_still_be_called() {
    #[bitfield(u32, default = 567)]
    #[derive(Eq, PartialEq, Debug)]
    struct Test {}

    #[allow(deprecated)]
    let old_new = Test::new();
    assert_eq!(old_new.raw_value(), 567);
}

#[test]
#[should_panic]
fn array_out_of_bounds_read() {
    #[bitfield(u64, default = 0)]
    pub struct OutOfBoundsTests {
        #[bit(0, rw, stride = 4)]
        nibble_bit0: [u1; 15],
    }

    let a = OutOfBoundsTests::DEFAULT;
    let _ = a.nibble_bit0(15);
}

#[test]
#[should_panic]
fn array_out_of_bounds_write() {
    #[bitfield(u64, default = 0)]
    pub struct OutOfBoundsTests {
        #[bit(0, rw, stride = 4)]
        nibble_bit0: [u1; 15],
    }

    let a = OutOfBoundsTests::DEFAULT;
    let _ = a.with_nibble_bit0(15, u1::new(0));
}

#[allow(non_camel_case_types)]
#[test]
fn reserved_identifiers() {
    #[bitenum(u1, exhaustive = true)]
    #[derive(PartialEq, Eq, Debug)]
    enum BitEnum {
        r#priv = 0,
        r#enum = 1,
    }

    #[bitfield(u32, default = 0)]
    #[derive(PartialEq, Eq, Debug)]
    struct BitfieldWithBitEnum {
        #[bit(6, rw)]
        r#priv: bool,

        #[bit(8, rw)]
        r#enum: [BitEnum; 6],
    }

    let field = BitfieldWithBitEnum::DEFAULT;
    assert_eq!(field.r#priv(), false);
    assert_eq!(
        field.with_priv(true),
        BitfieldWithBitEnum::new_with_raw_value(64)
    );

    assert_eq!(field.r#enum(0), BitEnum::r#priv);
    assert_eq!(
        field.with_enum(5, BitEnum::r#enum),
        BitfieldWithBitEnum::new_with_raw_value(8192)
    );
}

#[test]
fn new_with_construction1() {
    #[bitfield(u128, default = 0)]
    struct Test2 {
        #[bits(98..=127, rw)]
        val30: u30,

        #[bits(41..=97, rw)]
        val57: u57,

        #[bits(28..=40, rw)]
        val13: u13,

        #[bits(12..=27, rw)]
        val16: u16,

        #[bits(4..=11, rw)]
        baudrate: u8,

        #[bits(0..=3, rw)]
        some_other_bits: u4,
    }

    let t = Test2::builder()
        .with_val30(u30::new(1234))
        .with_val57(u57::new(876543))
        .with_val13(u13::new(5324))
        .with_val16(0x8FEF)
        .with_baudrate(0x12)
        .with_some_other_bits(u4::new(0x2))
        .build();
    assert_eq!(0x12, t.baudrate());
    assert_eq!(u4::new(0x2), t.some_other_bits());
    assert_eq!(u30::new(1234), t.val30());
}

#[test]
fn new_with_construction2() {
    #[bitfield(u128, default = 0)]
    struct Test2 {
        #[bits(0..=127, rw)]
        val128: u128,
    }

    let t = Test2::builder()
        .with_val128(0xFEDC_BA98_7654_3210_0123_4567_89AB_CDEF)
        .build();
    assert_eq!(0xFEDC_BA98_7654_3210_0123_4567_89AB_CDEF, t.val128());
}

#[test]
fn new_with_construction3() {
    #[bitfield(u64, default = 0)]
    struct Test2 {
        #[bits(0..=63, rw)]
        val64: u64,
    }

    let t = Test2::builder().with_val64(0xFEDC_BA98_7654_3210).build();
    assert_eq!(0xFEDC_BA98_7654_3210, t.val64());
}

#[test]
fn new_with_construction4() {
    #[bitfield(u32, default = 0x8000_0000)]
    struct Test2 {
        #[bits(12..=27, w)]
        c: u16,

        #[bits(8..=11, w)]
        b: u4,

        #[bits(0..=7, rw)]
        a: u8,
    }

    let t = Test2::builder()
        .with_c(0x8001)
        .with_b(u4::new(0b1011))
        .with_a(251)
        .build();
    assert_eq!(0x88001bfb, t.raw_value());
}

#[test]
fn new_with_construction5() {
    #[bitfield(u8, default = 0)]
    struct Test2 {
        #[bits(0..=7, rw)]
        val8: u8,
    }

    let t = Test2::builder().with_val8(0x83).build();
    assert_eq!(0x83, t.raw_value());
}

#[test]
fn new_with_construction_array1() {
    #[bitfield(u32, default = 0)]
    struct BytesInU32 {
        #[bits(0..=7, rw)]
        bytes: [u8; 4],
    }

    let t = BytesInU32::builder()
        .with_bytes([0x01, 0x23, 0x45, 0x67])
        .build();
    assert_eq!(0x67_45_23_01, t.raw_value());
}

#[test]
fn new_with_construction_array2() {
    #[bitfield(u128, default = 0)]
    struct WordsInU128 {
        #[bits(0..=15, w)]
        words: [u16; 8],
    }

    let t = WordsInU128::builder()
        .with_words([0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEFFE])
        .build();
    assert_eq!(0xEFFE_00CD_00AB_0089_0067_0045_0023_0001, t.raw_value());
}

#[test]
fn new_with_construction_array3() {
    #[bitfield(u32, default = 0)]
    struct Nibbles {
        #[bits(0..=3, w, stride = 8)]
        even_nibbles: [u4; 4],

        #[bits(4..=7, w, stride = 8)]
        odd_nibbles: [u4; 4],
    }

    let t = Nibbles::builder()
        .with_even_nibbles([u4::new(0), u4::new(1), u4::new(2), u4::new(3)])
        .with_odd_nibbles([u4::new(4), u4::new(5), u4::new(6), u4::new(7)])
        .build();
    assert_eq!(0x73625140, t.raw_value());
}

#[test]
fn without_default_complete() {
    // If no default value is specified, we can still do builder() syntax if the bitfield is complete.
    // Try this using a mix of arrays and a simple value

    #[bitfield(u32)]
    struct Nibbles {
        #[bits(0..=3, w, stride = 8)]
        even_nibbles: [u4; 4],

        #[bits(4..=7, w, stride = 8)]
        odd_nibbles: [u4; 3],

        #[bits(28..=31, rw)]
        last_odd: u4,
    }

    let t = Nibbles::builder()
        .with_even_nibbles([u4::new(0), u4::new(1), u4::new(2), u4::new(3)])
        .with_odd_nibbles([u4::new(4), u4::new(5), u4::new(6)])
        .with_last_odd(u4::new(7))
        .build();
    assert_eq!(0x73625140, t.raw_value());
}

#[test]
fn underlying_type_is_arbitrary_array_complete() {
    #[bitfield(u24)]
    struct Nibbles {
        #[bits(0..=7, rw)]
        bytes: [u8; 3],
    }

    assert_eq!(
        Nibbles::new_with_raw_value(u24::new(123)).raw_value(),
        u24::new(123)
    );
    assert_eq!(
        Nibbles::new_with_raw_value(u24::new(0xAABBCC)).bytes(0),
        0xCC
    );
    assert_eq!(
        Nibbles::new_with_raw_value(u24::new(0xAABBCC)).bytes(1),
        0xBB
    );
    assert_eq!(
        Nibbles::new_with_raw_value(u24::new(0xAABBCC)).bytes(2),
        0xAA
    );
    assert_eq!(
        Nibbles::new_with_raw_value(u24::new(0xAABBCC))
            .with_bytes(0, 0xDD)
            .raw_value(),
        u24::new(0xAABBDD)
    );
}

#[test]
fn underlying_type_is_arbitrary_default() {
    #[bitfield(u14, default = 0x567)]
    struct Nibbles {
        /// The nibbles
        #[bits(0..=3, rw)]
        first_nibble: u4,
    }

    assert_eq!(Nibbles::DEFAULT.raw_value(), u14::new(0x567));
    assert_eq!(
        Nibbles::new_with_raw_value(u14::new(123)).raw_value(),
        u14::new(123)
    );
    assert_eq!(
        Nibbles::new_with_raw_value(u14::new(0x127)).first_nibble(),
        u4::new(0x7)
    );
    assert_eq!(
        Nibbles::builder()
            .with_first_nibble(u4::new(0x9))
            .build()
            .raw_value(),
        u14::new(0x569)
    );
}

#[test]
fn test_noncontiguous_ranges() {
    // On RISC-V, some encodings have non-contiguous bitranges
    // These are two examples of two
    #[bitfield(u32)]
    struct S {
        #[bits([7..=11, 25..=31], rw)]
        imm: u12,
    }

    #[bitfield(u32)]
    struct B {
        #[bits([8..=11, 25..=30, 7..=7, 31], rw)]
        imm: u12,
    }

    pub const fn s_imm(value: u32) -> u12 {
        const MASK5: u32 = u5::MASK as u32;
        const MASK7: u32 = u7::MASK as u32;
        let imm_0_4 = (value >> 7) & MASK5;
        let imm_5_11 = (value >> 25) & MASK7;
        let imm = imm_0_4 | (imm_5_11 << 5);
        u12::extract_u32(imm, 0)
    }

    pub const fn b_imm(value: u32) -> u12 {
        let imm_0_3 = (value >> 8) & 0b1111;
        let imm_10 = (value >> 7) & 0b1;
        let imm_4_9 = (value >> 25) & 0b111111;
        let imm_11 = value >> 31;

        u12::extract_u32(
            imm_0_3 | (imm_4_9 << 4) | (imm_10 << 10) | (imm_11 << 11),
            0,
        )
    }

    for x in [
        0, 0xFFFFFFFF, 0x11111111, 0x22222222, 0x12345678, 0x98765432,
    ] {
        assert_eq!(S::new_with_raw_value(x).imm(), s_imm(x));
        assert_eq!(B::new_with_raw_value(x).imm(), b_imm(x));
    }
}

#[test]
fn test_noncontiguous_ranges_bitswap8() {
    // Make a really inefficient bitswapper
    #[bitfield(u8)]
    struct A {
        #[bits([7, 6, 5, 4, 3, 2, 1, 0], rw)]
        reversed: u8,
    }

    for x in 0..=255 {
        // Reading
        assert_eq!(A::new_with_raw_value(x).reversed(), x.reverse_bits());

        // Writing
        assert_eq!(
            A::new_with_raw_value(0).with_reversed(x).raw_value(),
            x.reverse_bits()
        );
    }
}

#[test]
fn test_noncontiguous_ranges_bitswap7() {
    #[bitfield(u8)]
    struct A8 {
        #[bits([6, 5, 4, 3, 2, 1, 0], rw)]
        reversed: u7,
    }

    #[bitfield(u7)]
    struct A7 {
        #[bits([6, 5, 4, 3, 2, 1, 0], rw)]
        reversed: u7,
    }

    for x in 0..=127 {
        // Reading
        let x7 = u7::new(x);
        assert_eq!(A8::new_with_raw_value(x).reversed(), x7.reverse_bits());
        assert_eq!(A7::new_with_raw_value(x7).reversed(), x7.reverse_bits());

        // Writing
        assert_eq!(
            A8::new_with_raw_value(0).with_reversed(x7).raw_value(),
            x7.reverse_bits().value()
        );
        assert_eq!(
            A7::new_with_raw_value(u7::new(0))
                .with_reversed(x7)
                .raw_value(),
            x7.reverse_bits()
        );
    }
}

#[test]
fn test_noncontiguous_ranges_byteswap() {
    // Same outer type as inner type
    #[bitfield(u32, default = 0)]
    struct A32 {
        #[bits([24..=31, 16..=23, 8..=15, 0..=7], rw)]
        byteswapped: u32,
    }

    #[bitfield(u24, default = 0)]
    struct A24 {
        #[bits([16..=23, 8..=15, 0..=7], rw)]
        byteswapped: u24,
    }

    // Arbitrary inside regular int, but with same base data type
    #[bitfield(u32, default = 0)]
    struct A32_24 {
        #[bits([16..=23, 8..=15, 0..=7], rw)]
        byteswapped: u24,
    }

    // Mixing arbitrary int and regular int where the base types don't match
    #[bitfield(u64, default = 0)]
    struct A64_24 {
        #[bits([16..=23, 8..=15, 0..=7], rw)]
        byteswapped: u24,
    }

    #[bitfield(u24, default = 0)]
    struct A24_16 {
        #[bits([8..=15, 0..=7], rw)]
        byteswapped: u16,
    }

    // Mixing different regular ints
    #[bitfield(u32, default = 0)]
    struct A32_16 {
        #[bits([8..=15, 0..=7], rw)]
        byteswapped: u16,
    }

    for x in [0, 0x11223344, 0x12345678] {
        let x24 = u24::extract_u32(x, 0);
        assert_eq!(A32::new_with_raw_value(x).byteswapped(), x.swap_bytes());
        assert_eq!(A32::DEFAULT.with_byteswapped(x).raw_value(), x.swap_bytes());

        assert_eq!(A24::new_with_raw_value(x24).byteswapped(), x24.swap_bytes());
        assert_eq!(
            A24::DEFAULT.with_byteswapped(x24).raw_value(),
            x24.swap_bytes()
        );

        assert_eq!(
            A32_24::new_with_raw_value(x24.value()).byteswapped(),
            x24.swap_bytes()
        );
        assert_eq!(
            A32_24::DEFAULT.with_byteswapped(x24).raw_value(),
            x24.swap_bytes().value()
        );

        assert_eq!(
            A64_24::new_with_raw_value(x24.value() as u64).byteswapped(),
            x24.swap_bytes()
        );
        assert_eq!(
            A64_24::DEFAULT.with_byteswapped(x24).raw_value(),
            x24.swap_bytes().value() as u64
        );

        assert_eq!(
            A24_16::new_with_raw_value(x24).byteswapped(),
            (x as u16).swap_bytes()
        );
        assert_eq!(
            A24_16::DEFAULT.with_byteswapped(x as u16).raw_value(),
            u24::new((x as u16).swap_bytes() as u32)
        );

        assert_eq!(
            A32_16::new_with_raw_value(x).byteswapped(),
            (x as u16).swap_bytes()
        );

        assert_eq!(
            A32_16::DEFAULT.with_byteswapped(x as u16).raw_value(),
            (x as u16).swap_bytes() as u32
        );
    }
}

#[test]
fn test_noncontiguous_ranges_array_arbitrary_in_regular() {
    #[bitfield(u32)]
    struct A32_4 {
        #[bits([0, 2, 4, 6], rw, stride = 8)]
        even: [u4; 4],
    }

    let a = A32_4::new_with_raw_value(0b11001001_01010101_10101010_11111111);
    assert_eq!(a.even(0), u4::new(0b1111));
    assert_eq!(a.even(1), u4::new(0b0000));
    assert_eq!(a.even(2), u4::new(0b1111));
    assert_eq!(a.even(3), u4::new(0b1001));

    assert_eq!(
        a.with_even(0, u4::new(0b0000)).raw_value(),
        0b11001001_01010101_10101010_10101010
    );
    assert_eq!(
        a.with_even(1, u4::new(0b1001)).raw_value(),
        0b11001001_01010101_11101011_11111111
    );
    assert_eq!(
        a.with_even(2, u4::new(0b1001)).raw_value(),
        0b11001001_01000001_10101010_11111111
    );
    assert_eq!(
        a.with_even(3, u4::new(0b0011)).raw_value(),
        0b10001101_01010101_10101010_11111111
    );
}

#[test]
fn test_noncontiguous_ranges_array_regular_in_regular() {
    #[bitfield(u32)]
    struct A32_8 {
        #[bits([0, 2, 4, 6, 8, 10, 12, 14], rw, stride = 16)]
        even: [u8; 2],
    }

    let a = A32_8::new_with_raw_value(0b11001001_01010101_10101010_11111111);
    assert_eq!(a.even(0), 0b00001111);
    assert_eq!(a.even(1), 0b10011111);

    assert_eq!(
        a.with_even(0, 0b10010000).raw_value(),
        0b11001001_01010101_11101011_10101010
    );
    assert_eq!(
        a.with_even(1, 0b00111001).raw_value(),
        0b10001101_01000001_10101010_11111111
    );
}

#[test]
fn test_noncontiguous_ranges_array_regular_in_arbitrary() {
    #[bitfield(u48)]
    struct A48_8 {
        #[bits([0, 2, 4, 6, 8, 10, 12, 14], rw, stride = 16)]
        even: [u8; 3],
    }

    let a = A48_8::new_with_raw_value(u48::new(
        0b11110000_11100010_11001001_01010101_10101010_11111111,
    ));
    assert_eq!(a.even(0), 0b00001111);
    assert_eq!(a.even(1), 0b10011111);
    assert_eq!(a.even(2), 0b11001000);

    assert_eq!(
        a.with_even(0, 0b10010000).raw_value(),
        u48::new(0b11110000_11100010_11001001_01010101_11101011_10101010)
    );
    assert_eq!(
        a.with_even(1, 0b00111001).raw_value(),
        u48::new(0b11110000_11100010_10001101_01000001_10101010_11111111)
    );
    assert_eq!(
        a.with_even(2, 0b00111001).raw_value(),
        u48::new(0b10100101_11100011_11001001_01010101_10101010_11111111)
    );
}

/// Use arrays and non-contiguous together
#[test]
fn test_noncontiguous_ranges_array_with_interleaving_and_builder() {
    #[bitfield(u8)]
    struct A8 {
        #[bits([0, 2, 4, 6], rw, stride = 1)]
        every_other_bit: [u4; 2],
    }
    assert_eq!(
        A8::new_with_raw_value(0b01101101).every_other_bit(0),
        u4::new(0b1011)
    );
    assert_eq!(
        A8::new_with_raw_value(0b01101101).every_other_bit(1),
        u4::new(0b0110)
    );

    assert_eq!(
        A8::builder()
            .with_every_other_bit([u4::new(0b0110), u4::new(0b1100)])
            .build()
            .raw_value(),
        0b10110100
    );
}

#[test]
fn test_getter_and_setter() {
    #[bitfield(u128, default = 0)]
    struct Test2 {
        #[bits(98..=127, rw)]
        val30: u30,

        #[bits(41..=97, rw)]
        val57: u57,

        #[bits(28..=40, rw)]
        val13: u13,

        #[bits(12..=27, rw)]
        val16: u16,

        #[bits(4..=11, rw)]
        baudrate: u8,

        #[bits(0..=3, rw)]
        some_other_bits: u4,
    }

    let mut t = Test2::new_with_raw_value(0xAE42_315A_2134_FE06_3412_345A_2134_FE06);
    assert_eq!(u30::new(0x2B908C56), t.val30());
    assert_eq!(u57::new(0x0110_9A7F_031A_091A), t.val57());
    assert_eq!(u13::new(0x5A2), t.val13());
    assert_eq!(0x134F, t.val16());
    assert_eq!(0xE0, t.baudrate());
    assert_eq!(u4::new(0x6), t.some_other_bits());

    t.set_baudrate(0x12);
    t.set_some_other_bits(u4::new(0x2));
    assert_eq!(0x12, t.baudrate());
    assert_eq!(u4::new(0x2), t.some_other_bits());
    assert_eq!(0xAE42_315A_2134_FE06_3412_345A_2134_F122, t.raw_value);
}

#[test]
fn test_getter_and_setter_arbitrary_uint() {
    #[bitfield(u128, default = 0)]
    struct Test2 {
        #[bits(4..=11, rw)]
        baudrate: u8,

        #[bits(0..=3, rw)]
        some_other_bits: u4,
    }

    let mut t = Test2::new_with_raw_value(0xFE06);
    assert_eq!(0xE0, t.baudrate());
    assert_eq!(u4::new(0x6), t.some_other_bits());

    t.set_baudrate(0x12);
    t.set_some_other_bits(u4::new(0x2));
    assert_eq!(0x12, t.baudrate());
    assert_eq!(u4::new(0x2), t.some_other_bits());
    assert_eq!(0xF122, t.raw_value);
}

#[test]
fn test_fully_qualified_paths() {
    mod inner {
        use super::*;
        #[bitenum(u2, exhaustive = true)]
        #[derive(Eq, PartialEq, Debug)]
        pub enum TestEnum {
            Var0 = 0b00,
            Var1 = 0b01,
            Var2 = 0b10,
            Var3 = 0b11,
        }

        #[bitenum(u2, exhaustive = false)]
        #[derive(Eq, PartialEq, Debug)]
        pub enum TestEnum2 {
            Var0 = 0b00,
            Var1 = 0b01,
            Var2 = 0b10,
        }
    }
    #[bitfield(u16, default = 0)]
    struct Test {
        #[bits(8..=15, rw)]
        baudrate: u8,
        #[bits(6..=7, rw)]
        exhaustive_enum: inner::TestEnum,
        #[bits(4..=5, rw)]
        non_exhaustive_enum: Option<inner::TestEnum2>,
        #[bits(0..=3, rw)]
        some_other_bits: arbitrary_int::u4,
    }
    let t = Test::new_with_raw_value(0x1894);
    assert_eq!(t.baudrate(), 0x18);
    assert_eq!(t.some_other_bits().as_u8(), 0x4);
    assert_eq!(t.exhaustive_enum(), inner::TestEnum::Var2);
    assert_eq!(t.non_exhaustive_enum(), Ok(inner::TestEnum2::Var1));
}

#[test]
fn test_debug_impl() {
    #[bitfield(u16, debug)]
    struct Test {
        #[bits(8..=15, rw)]
        upper: u8,

        #[bits(0..=7, rw)]
        lower: u8,
    }
    let test = Test::new_with_raw_value(0x1F2F);
    let display_str = format!("{:?}", test);
    assert_eq!(display_str, "Test { upper: 31, lower: 47 }");
}

#[test]
fn introspection() {
    #[bitfield(u32, introspect)]
    struct Bitfield {
        #[bits(0..=7)]
        a: u8,
        #[bits(8..=15)]
        b: [u8; 2],
        #[bits([24..=25, 30..=31])]
        c: u4,
        #[bit(29)]
        d: bool,
        #[bit(30)]
        r#ref: [bool; 2],
    }

    // <NAME>_BITS exposes the "bits" value(s):
    assert_eq!(Bitfield::A_BITS, 0..=7);
    assert_eq!(Bitfield::B_BITS, 8..=15);
    assert_eq!(Bitfield::C_BITS, [24..=25, 30..=31]);
    assert_eq!(Bitfield::D_BITS, 29..=29);
    assert_eq!(Bitfield::REF_BITS, 30..=30);

    // Arrays also have <NAME>_COUNT and <NAME>_STRIDE
    assert_eq!(Bitfield::B_COUNT, 2);
    assert_eq!(Bitfield::B_STRIDE, 8);
    assert_eq!(Bitfield::REF_COUNT, 2);
    assert_eq!(Bitfield::REF_STRIDE, 1);

    // <name>_mask() returns a mask for the field
    assert_eq!(Bitfield::a_mask(), 0x000000FF);
    assert_eq!(Bitfield::b_mask(0), 0x0000FF00);
    assert_eq!(Bitfield::b_mask(1), 0x00FF0000);
    assert_eq!(Bitfield::c_mask(), 0xC3000000);
    assert_eq!(Bitfield::d_mask(), 0x20000000);
    assert_eq!(Bitfield::ref_mask(0), 0x40000000);
    assert_eq!(Bitfield::ref_mask(1), 0x80000000);
}

fn test_defmt_impl_fields() {
    #[bitfield(u16, defmt_fields)]
    struct Test {
        #[bits(8..=15, rw)]
        upper: u8,

        #[bits(0..=7, rw)]
        lower: u8,
    }
    let test = Test::new_with_raw_value(0x1F2F);
    defmt_impl_check(&test);
}

#[test]
fn test_defmt_impl_bitfields() {
    #[bitfield(u16, defmt_bitfields)]
    struct Test {
        #[bits(8..=15, rw)]
        upper: u8,

        #[bits(0..=7, rw)]
        lower: u8,
    }
    let test = Test::new_with_raw_value(0x1F2F);
    defmt_impl_check(&test);
    // I'd like to test the actual printout/impl, but I do not know how to do this on a host PC
    // yet..
}

#[test]
#[cfg(feature = "defmt")]
fn test_defmt_impl_bitfield_feature_gated() {
    #[bitfield(u16, defmt_bitfields(feature = "defmt"))]
    struct Test {
        #[bits(8..=15, rw)]
        upper: u8,

        #[bits(0..=7, rw)]
        lower: u8,
    }
    let test = Test::new_with_raw_value(0x1F2F);
    defmt_impl_check(&test);
    // I'd like to test the actual printout/impl, but I do not know how to do this on a host PC
    // yet..
}

pub fn defmt_impl_check<T: defmt::Format>(_: &T) {}
