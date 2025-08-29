use bitbybit::bitfield;

#[bitfield(u32, default = 0)]
struct Test {
    #[bits(0..=8, rw)]
    invalid_range: u8,
}

fn main() {}
