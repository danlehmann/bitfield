use arbitrary_int::*;
use bitbybit::{bitenum, bitfield};

#[bitenum(u2, exhaustive = true)]
#[derive(PartialEq, Debug)]
pub enum BitEnumTest {
    A = 0,
    B = 1,
    C = 2, 
    D = 3
}


#[bitfield(u32, introspect, forbid_overlaps, default = use_field_defaults)]
pub struct WithDefaultFieldValues {
    #[bit(0, rw, default=true)]
    test_bool_true: bool, 
    #[bit(1, rw, default=false)]
    test_bool_false: bool, 
    #[bit(2, rw, default=1)]
    test_u1: u1,
    #[bits(3..=6, rw, default=10)] 
    test_arbitrary_int: u4,
    #[bits(7..=8, rw, default= BitEnumTest::B)] 
    test_enum: BitEnumTest, 
    #[bits(9..=16, rw, default=0xFF)] 
    test_native_int: u8, 
    #[bits(17..=31, rw, default=-50)] 
    test_signed_int: i15, 
}

#[bitfield(u32, introspect, default = 0xFFFF_0000)] // the previous system takes over
pub struct LegacyDefault {
    #[bits(0..=15, rw, default = 4)]
    non_zero: u16,
    #[bits(16..=31, rw)]
    zero: u16,
}


pub fn main() {
    // Testing the generated constant
    assert_eq!(WithDefaultFieldValues::TEST_BOOL_TRUE_DEFAULT, true);
    assert_eq!(WithDefaultFieldValues::TEST_BOOL_FALSE_DEFAULT, false);
    assert_eq!(WithDefaultFieldValues::TEST_U1_DEFAULT, u1::new(1));
    assert_eq!(WithDefaultFieldValues::TEST_ARBITRARY_INT_DEFAULT, u4::new(10));
    assert_eq!(WithDefaultFieldValues::TEST_ENUM_DEFAULT, BitEnumTest::B);
    assert_eq!(WithDefaultFieldValues::TEST_NATIVE_INT_DEFAULT, 0xFF);
    assert_eq!(WithDefaultFieldValues::TEST_SIGNED_INT_DEFAULT, i15::new(-50));

    // Test the generated default value
    let reg = WithDefaultFieldValues::default();
    assert_eq!(reg.test_bool_true(), true);
    assert_eq!(reg.test_bool_false(), false);
    assert_eq!(reg.test_u1(), u1::new(1));
    assert_eq!(reg.test_arbitrary_int(), u4::new(10));
    assert_eq!(reg.test_enum(), BitEnumTest::B);
    assert_eq!(reg.test_native_int(), 0xFF);
    assert_eq!(reg.test_signed_int(), i15::new(-50));
    
    // Still can access the previous ZERO const
    let reg = WithDefaultFieldValues::ZERO;
    assert_eq!(reg.test_bool_true(), false);
    assert_eq!(reg.test_bool_false(), false);
    assert_eq!(reg.test_u1(), u1::new(0));
    assert_eq!(reg.test_arbitrary_int(), u4::new(0));
    assert_eq!(reg.test_enum(), BitEnumTest::A);
    assert_eq!(reg.test_native_int(), 00);
    assert_eq!(reg.test_signed_int(), i15::new(0));

    // Old system can still be used
    let reg: LegacyDefault = LegacyDefault::default() ;
    assert_eq!(reg.non_zero(), 0x0000);
    assert_eq!(reg.zero(), 0xFFFF);

}
