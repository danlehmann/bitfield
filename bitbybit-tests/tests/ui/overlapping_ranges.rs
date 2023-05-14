use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[ranges(0x00..=0x20)]
    RangeVariant1(u8),

    #[ranges(0x20..=0x30)]
    RangeVariant2(u8),
}

fn main() {}