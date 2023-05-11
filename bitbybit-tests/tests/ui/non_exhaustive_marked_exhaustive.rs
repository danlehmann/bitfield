use bitbybit::bitenum;

#[bitenum(u8, exhaustive: true)]
enum Foo {
    #[range(0x00..=0x20)]
    #[range(0x50..=0x60)]
    VariantRanges1(u8),
}

fn main() {}