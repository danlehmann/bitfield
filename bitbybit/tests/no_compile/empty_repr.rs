use bitbybit::bitfield;

// An empty repr would silently drop the default repr(C), so it is rejected.
#[bitfield(u32, repr())]
struct Test {
    #[bits(0..=31, rw)]
    field: u32,
}

fn main() {}
