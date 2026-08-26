use bitbybit::bitenum;

#[bitenum(u8, exhaustive = true)]
struct NotAnEnum {
    value: u8,
}

fn main() {
    // The macro should still emit the original item so that it stays usable
    // even though the attribute is invalid. If it were dropped, this reference
    // would surface an additional "cannot find type" error.
    let _size = core::mem::size_of::<NotAnEnum>();
}
