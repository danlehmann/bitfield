use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[ranges(0x00..=0x20)]
    VariantRange1(u8),
}

fn main() {}