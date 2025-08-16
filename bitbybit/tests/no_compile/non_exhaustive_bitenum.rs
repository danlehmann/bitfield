
use bitbybit::bitenum;

#[bitenum(u2, exhaustive = true)]
pub enum NonExhaustive {
    A = 0b00,
    B = 0b01,
}

fn main() {}
