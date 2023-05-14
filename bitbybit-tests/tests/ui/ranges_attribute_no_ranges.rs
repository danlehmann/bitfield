use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[ranges()]
    VariantRange1(u8),

    Variant(u8) = 0x10,
}

fn main() {}