use bitbybit::bitenum;

#[bitenum(u8, exhaustive: true)]
enum Foo {
    #[ranges(0x00..=0x20, 0x50..=0x60)]
    RangeVariant1(u8),

    #[ranges(0x21..=0x4F, 0x61..=0xFF)]
    RangeVariant2(u8),

    #[catchall]
    CatchallVariant(u8),
}

fn main() {}