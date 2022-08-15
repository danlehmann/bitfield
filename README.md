# bit-fields
This crate provides bit fields and enums that are backed by bits.

## How to use

A bit field is created similar to a regular Rust struct. Annotations define the layout of the structure. As an example, the following bitfield
implements the [GICD_TYPER](https://developer.arm.com/documentation/ihi0048/b/Programmers--Model/Distributor-register-descriptions/Interrupt-Controller-Type-Register--GICD-TYPER) register
that is used in the ARM interrupt controller:

```
#[bitfield(u32)]
struct GICD_TYPER {
    #[bits(11..=15, r)]
    lspi: u5,

    #[bit(10, r)]
    security_extn: bool,

    #[bits(5..=7, r)]
    cpu_number: u3,

    #[bits(0..=4, r)]
    itlines_number: u5,
}
```

It has the following parts:
- #[bitfield(u32)] specifies that this is a bitfield in which u32 is the underlying data type. This means that all the bits inside of the bitfield
have to fit within 32 bits. u8, u16, u32, u64 and u128 are supported as underlying data types.
- Each field is annotated with the range of bits that are used by the field. The data type must match the number of bits: A range of 0..=8 with u8 would cause a compile error, as u9 is the data type that matches 0..=8.
- bool fields are declared as "bit", all other fields as "bits"
- Fields are declared as "r" for read-only, "w" for write-only or "rw" as read/write. In the example above, all fields are read-only as this specific register is only used to read values.
