use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[range(0x00..=0x20)]
    #[range(0x40..=0x60)]
    RangeVariant1(u8),
}

fn main() {}