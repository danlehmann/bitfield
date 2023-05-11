use bitbybit::bitenum;

#[bitenum(u8, exhaustive: true)]
enum Foo {
    Variant1 = 0x00,

    #[catchall]
    CatchallVariant1(u8),

    #[catchall]
    CatchallVariant2(u8),
}

fn main() {}