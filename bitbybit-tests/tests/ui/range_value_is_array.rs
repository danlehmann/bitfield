use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[range([0x00..=0x20, 0x40..=0x50])]
    VariantRange1(u8),
}

fn main() {}