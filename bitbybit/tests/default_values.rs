use arbitrary_int::u4;
use bitbybit::bitfield;

#[bitfield(u32, debug, introspect)]
pub struct BitfieldU32 {
    #[bits(28..=31, rw, default = 0xF)]
    val3: u4,
    #[bits(24..=27, rw, default = 0d10)]
    val2: u4,
    #[bits(16..=23, rw, default = 1)]
    val1: u8,
    #[bits(0..=15, rw, default = 53)]
    val0: u16,
}

pub fn main() {
    let test = 0x00 ;
    let bitfield = BitfieldU32::new_with_raw_value(0x0);
    assert_eq!(bitfield.val0(), 0);
    assert_eq!(bitfield.val1(), 0);
    assert_eq!(bitfield.val2(), u4::new(0));
    assert_eq!(bitfield.val3(), u4::new(0));
    assert_eq!(bitfield.raw_value(), 0);

    let bitfield_with_values = BitfieldU32::new_with_raw_value(0x1234_5678);

    assert_eq!(bitfield_with_values.val0(), 0x5678);
    assert_eq!(bitfield_with_values.val1(), 0x34);
    assert_eq!(bitfield_with_values.val2(), u4::new(0x2));
    assert_eq!(bitfield_with_values.val3(), u4::new(0x1));
    assert_eq!(bitfield_with_values.raw_value(), 0x1234_5678);
}
