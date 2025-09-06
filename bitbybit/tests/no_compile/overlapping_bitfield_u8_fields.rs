use bitbybit::bitfield;

#[bitfield(u32, default = 0, forbid_overlaps)]
struct Test {
    #[bits(7..=14, rw)]
    overlapping_bits: u8,
    #[bits(0..=7, rw)]
    lower_bits: u8,
}

fn main() {}
