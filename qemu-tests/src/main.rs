#![no_std]
#![no_main]

use arbitrary_int::{u2, u4, u5};
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

#[bitbybit::bitfield(u4, default = 0x0, defmt_fields)]
pub struct SubBitfield {
    #[bit(3, rw)]
    boolean_1: bool,
    #[bits(1..=2, rw)]
    number_0: u2,
    #[bit(0, rw)]
    boolean_0: bool,
}

#[bitbybit::bitfield(u4, default = 0x0, defmt_bitfields)]
pub struct FieldWithArbitraryIntBaseType {
    #[bit(3, rw)]
    boolean_1: bool,
    #[bits(1..=2, rw)]
    number_0: u2,
    #[bit(0, rw)]
    boolean_0: bool,
}

#[bitbybit::bitfield(u32, default = 0x0, defmt_bitfields)]
struct TestBitfields {
    #[bits(11..=15, rw)]
    number: u5,

    #[bit(10, rw)]
    boolean: bool,

    // Write-only fields are allowed. Fot bitfields, they are also printed.
    #[bit(9, w)]
    control_bit: bool,

    #[bits(5..=6, rw)]
    enumeration: TestEnum,
}

#[bitbybit::bitfield(u32, default = 0x0, defmt_fields)]
struct TestFields {
    #[bits(11..=15, rw)]
    number: u5,

    #[bit(10, rw)]
    boolean: bool,

    // Write-only fields are allowed. Fot fields, they are skipped.
    #[bit(9, w)]
    control_bit: bool,

    #[bits(5..=6, rw)]
    enumeration: TestEnum,
}

#[bitbybit::bitfield(u32, default = 0x0, defmt_fields)]
struct TestNestedField {
    #[bits(12..=15, rw)]
    field_3: SubBitfield,
    #[bits(8..=11, rw)]
    field_2: SubBitfield,
    #[bits(4..=7, rw)]
    field_1: SubBitfield,
    #[bits(0..=3, rw)]
    field_0: SubBitfield,
}

#[bitbybit::bitfield(u32, default = 0x0, defmt_bitfields)]
struct MultiRangeBitfield {
    #[bits(4..=7, rw)]
    field_in_between: u4,
    #[bits([0..=3, 8..=11], rw)]
    split_field: u8,
}

#[cortex_m_rt::entry]
fn main() -> ! {
    defmt::println!("bitbybit defmt test");
    let bitfield_register = TestBitfields::builder()
        .with_number(u5::new(16))
        .with_boolean(true)
        .with_control_bit(true)
        .with_enumeration(TestEnum::A)
        .build();

    let fields_register = TestFields::builder()
        .with_number(u5::new(12))
        .with_boolean(true)
        .with_control_bit(true)
        .with_enumeration(TestEnum::C)
        .build();

    let multi_range_bitfield = MultiRangeBitfield::builder()
        .with_field_in_between(u4::new(0b1010))
        .with_split_field(0b11001010)
        .build();

    let nested_field = TestNestedField::builder()
        .with_field_3(
            SubBitfield::builder()
                .with_boolean_1(true)
                .with_number_0(u2::new(0x0))
                .with_boolean_0(false)
                .build(),
        )
        .with_field_2(
            SubBitfield::builder()
                .with_boolean_1(false)
                .with_number_0(u2::new(0b10))
                .with_boolean_0(true)
                .build(),
        )
        .with_field_1(
            SubBitfield::builder()
                .with_boolean_1(true)
                .with_number_0(u2::new(0b11))
                .with_boolean_0(true)
                .build(),
        )
        .with_field_0(
            SubBitfield::builder()
                .with_boolean_1(false)
                .with_number_0(u2::new(0x0))
                .with_boolean_0(false)
                .build(),
        )
        .build();

    let arb_int_field = FieldWithArbitraryIntBaseType::builder()
        .with_boolean_1(true)
        .with_number_0(u2::new(0b01))
        .with_boolean_0(false)
        .build();

    defmt::info!("Bitfields: {}", bitfield_register);
    defmt::info!("Fields: {}", fields_register);
    defmt::info!("MultiRangeBitfield: {}", multi_range_bitfield);
    defmt::info!("NestedField: {}", nested_field);
    defmt::info!("FieldWithArbIntBase: {}", arb_int_field);
    loop {
        cortex_m_semihosting::debug::exit(cortex_m_semihosting::debug::EXIT_SUCCESS);
    }
}

// End of file
