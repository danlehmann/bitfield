use bitbybit::bitenum;

#[bitenum(u8, exhaustive: false)]
enum Foo {
    #[range(1..=5, 7..=10)]
    RangeVariant1(u8),

    Variant(u8) = 20,
}

fn main() {}