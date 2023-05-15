use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[range(1..=10)]
    RangeVariant(u16),
}

fn main() {}