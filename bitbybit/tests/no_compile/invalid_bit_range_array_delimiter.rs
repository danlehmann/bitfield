use bitbybit::bitfield;

#[bitfield(u16, default = 0)]
struct Test {
    // A parenthesized range is not a bit-range array; it must be given in
    // square brackets.
    #[bits((0..=1), rw)]
    field: u8,
}

fn main() {}
