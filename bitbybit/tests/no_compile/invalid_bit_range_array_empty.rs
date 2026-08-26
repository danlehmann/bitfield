use bitbybit::bitfield;

#[bitfield(u16, default = 0)]
struct Test {
    // A doubled comma produces an empty array element, which is rejected.
    #[bits([0..=1,, 4..=5], rw)]
    field: u8,
}

fn main() {}
