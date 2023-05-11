use bitbybit::bitenum;

#[bitenum(u8, exhaustive: true)]
enum Foo {
    #[range(0x00..=0x20)]
    VariantRange1(u8),

    #[range(0x20..=0x300)]
    VariantRange2(u8),
}

fn main() {}