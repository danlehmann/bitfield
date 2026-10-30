use arbitrary_int::prelude::*;
use bitbybit::bitfield;

#[bitfield(u32)]
struct SignedWithUnsignedUnderlyingType {
    #[bits(0..=6, rw)]
    a: Int<u32, 7>,
}

#[bitfield(u32)]
struct UnsignedWithSignedUnderlyingType {
    #[bits(0..=6, rw)]
    a: UInt<i32, 7>,
}

#[bitfield(u32)]
struct TooManyBitsForUnderlyingType {
    #[bits(0..=8, rw)]
    a: UInt<u8, 9>,
}

#[bitfield(u32)]
struct ZeroBits {
    #[bits(0..=8, rw)]
    a: UInt<u16, 0>,
}

#[bitfield(u32)]
struct MismatchedBitCount {
    #[bits(0..=8, rw)]
    a: UInt<u32, 8>,
}

fn main() {}
