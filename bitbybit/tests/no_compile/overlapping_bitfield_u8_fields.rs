use bitbybit::bitfield;
use arbitrary_int::u4;

#[bitfield(u32, default = 0, forbid_overlaps)]
struct Test {
    #[bits(7..=14, rw)]
    overlapping_bits: u8,
    #[bits(0..=7, rw)]
    lower_bits: u8,
}
#[bitfield(u16)]
struct TestAllowed {
    #[bits(12..=15, rw)]
    bit_upper: u4,
    #[bits(2..=9, rw)]
    middle_bits: u8,
    #[bits(0..=7, rw)]
    lower_bits: u8,
}

fn main() {
    // correct usage
    TestAllowed::new_with_raw_value(0x1F1F);
    TestAllowed::builder().with_bit_upper(u4::new(0)).with_lower_bits(0).build();
    TestAllowed::builder().with_middle_bits(0).with_bit_upper(u4::new(0)).build();
    // can't set overlapping fields at the same time
    TestAllowed::builder().with_lower_bits(0).with_middle_bits(0).with_bit_upper(u4::new(0)).build();
    TestAllowed::builder().with_middle_bits(0).with_lower_bits(0).with_bit_upper(u4::new(0)).build();
    // missing mandatory fields
    TestAllowed::builder().with_bit_upper(u4::new(0)).build();
    TestAllowed::builder().with_middle_bits(0).build();
}
