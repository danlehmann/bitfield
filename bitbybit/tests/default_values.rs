use arbitrary_int::*;
use bitbybit::bitfield;

#[bitfield(u32, introspect, forbid_overlaps, default = use_field_defaults)]
pub struct WithDefaultFieldValues {
    #[bits(0..=2, rw, default = 4)] // <- decimal ok
    val3: u3,
    //#[bits(2..=16, rw, default = 0xFFFF)] //<- Will raise an error
    #[bits(3..=16, rw, default = 0xBEF)] // Accepts 0x and 0h (as base16 indicator)
    val2: u14,
    #[bits(17..=23, rw, default = 0d42)] // Accepts 0d (as base10 indicator)
    val1: u7,
    #[bits(24..=31, rw, default = -1)] // Can parse negative values
    val0: i8,
}

#[bitfield(u32, introspect, default = 0)] // the previous system takes over
pub struct LegacyDefault {
    #[bits(0..=2, rw, default = 4)]
    val3: u3,
    #[bits(3..=16, rw, default = 0xBEF)]
    val2: u14,
    #[bits(17..=23, rw, default = 0d42)]
    val1: u7,
    #[bits(24..=31, rw, default = -1)]
    val0: i8,
}

#[bitfield(u32, introspect, forbid_overlaps, default = use_field_defaults)]
pub struct WithoutWriteAcces {
    #[bits(0..=2, rw, default = 4)]
    val3: u3,
    #[bits(3..=16, r, default = 0xBEF)]
    val2: u14,
    #[bits(17..=23, w, default = 0d42)]
    val1: u7,
    #[bits(24..=31, default = -1)]
    val0: i8,
}


pub fn main() {
    let reg = WithDefaultFieldValues::default();
    assert_eq!(reg.val3(), u3::new(4));
    assert_eq!(reg.val2(), u14::new(0xBEF));
    assert_eq!(reg.val1(), u7::new(42));
    assert_eq!(reg.val0(), -1);

    let reg = WithDefaultFieldValues::new_with_raw_value(0);
    assert_eq!(reg.val3(), u3::new(0));
    assert_eq!(reg.val2(), u14::new(0));
    assert_eq!(reg.val1(), u7::new(0));
    assert_eq!(reg.val0(), 0);

    let reg2 = LegacyDefault::default();
    assert_ne!(reg2.val3(), LegacyDefault::VAL3_DEFAULT);
    assert_ne!(reg2.val2(), LegacyDefault::VAL2_DEFAULT);
    assert_ne!(reg2.val1(), LegacyDefault::VAL1_DEFAULT);
    assert_ne!(reg2.val0(), LegacyDefault::VAL0_DEFAULT);
    assert_eq!(reg2.raw_value(), LegacyDefault::ZERO.raw_value());

    let reg = WithoutWriteAcces::default();
    assert_eq!(reg.val3(), u3::new(4));
    assert_eq!(reg.val2(), u14::new(0xBEF));
    //assert_eq!(reg.val1(), u7::new(42)); 
    //assert_eq!(reg.val0(), -1);
    assert_eq!(reg.raw_value(), WithoutWriteAcces::DEFAULT_RAW_VALUE);
}
