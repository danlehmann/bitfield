use bitbybit::bitenum;

#[bitenum(u8, exhaustive: true)]
enum Foo {
    #[range(0x00..=0x20)]
    #[range(0x50..=0x60)]
    VariantRanges1(u8),

    #[range(0x21..=0x4F)]
    #[range(0x61..=0xFF)]
    VariantRanges2(u8),

    #[catchall]
    CatchallVariant(u8),
}

fn main() {}