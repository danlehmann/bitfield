use bitbybit::bitfield;

#[bitfield(u32, default = 0, forbid_overlaps)]
struct Test {
    #[bit(0, rw)]
    overlapping_bit: bool,
    #[bit(0, rw)]
    bit: bool
}

fn main() {}
