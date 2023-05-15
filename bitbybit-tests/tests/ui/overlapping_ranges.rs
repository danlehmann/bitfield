use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[range(0x00..=0x20)]
    RangeVariant1(u8),

    #[range(0x20..=0x30)]
    RangeVariant2(u8),
}

fn main() {}