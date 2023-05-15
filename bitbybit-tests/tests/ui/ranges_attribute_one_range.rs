use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[ranges(1..=5)]
    RangeVariant1(u8),

    Variant(u8) = 20,
}

fn main() {}