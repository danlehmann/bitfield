#![no_std]
#![no_main]

use arbitrary_int::u5;
use defmt_semihosting as _;
use qemu_tests as _;

#[bitbybit::bitenum(u2, exhaustive = true)]
#[derive(defmt::Format)]
pub enum TestEnum {
    A = 0b00,
    B = 0b01,
    C = 0b10,
    D = 0b11,
}
#[bitbybit::bitfield(u32, default = 0x0, defmt_bitfields)]
struct TestBitfields {
    #[bits(11..=15, rw)]
    number: u5,

    #[bit(10, rw)]
    boolean: bool,

    #[bits(5..=6, rw)]
    enumeration: TestEnum,
}

#[bitbybit::bitfield(u32, default = 0x0, defmt_fields)]
struct TestFields {
    #[bits(11..=15, rw)]
    number: u5,

    #[bit(10, rw)]
    boolean: bool,

    #[bits(5..=6, rw)]
    enumeration: TestEnum,
}

#[cortex_m_rt::entry]
fn main() -> ! {
    defmt::println!("Hello, world!");
    let bitfield_register = TestBitfields::builder()
        .with_number(u5::new(16))
        .with_boolean(true)
        .with_enumeration(TestEnum::A)
        .build();

    let fields_register = TestFields::builder()
        .with_number(u5::new(12))
        .with_boolean(true)
        .with_enumeration(TestEnum::C)
        .build();
    defmt::info!("Bitfields: {}", bitfield_register);
    defmt::info!("Fields: {}", fields_register);
    loop {
        cortex_m_semihosting::debug::exit(cortex_m_semihosting::debug::EXIT_SUCCESS);
    }
}

// End of file
