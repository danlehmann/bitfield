# bit-fields
This crate provides bit fields and enums that are backed by bits.

## Basic declaration

A bit field is created similar to a regular Rust struct. Annotations define the layout of the structure. As an example, consider the following definition, which specifies a bit field:

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
- Valid data types for fields are the basic types u8, u16, u32, u64, u128, bool as well as enums (see below) or types like u1, u2, u3 from [arbitrary-int](https://crates.io/crates/arbitrary-int)
- Fields are declared as "r" for read-only, "w" for write-only or "rw" as read/write. In the example above, all fields are read-only as this specific register is only used to read values.

## Enumerations
Very often, fields aren't just numbers but really enums. This is supported by first defining a bitenum and then using that inside of a bitfield:

```
#[bitenum(bits: 2, exhaustive: false)]
#[derive(Eq, PartialEq, Debug)]
pub enum NonExhaustiveEnum {
    Zero = 0b00,
    One = 0b01,
    Two = 0b10,
}

#[bitenum(bits: 2, exhaustive: true)]
#[derive(Eq, PartialEq, Debug)]
enum ExhaustiveEnum {
    Zero = 0b00,
    One = 0b01,
    Two = 0b10,
    Three = 0b11,
}

#[bitfield(u64, default: 0)]
struct BitfieldWithEnum {
    #[bits(2..=3, rw)]
    e2: Option<NonExhaustiveEnum>,

    #[bits(0..=1, rw)]
    e1: ExhaustiveEnum,
}
```

- The bitenum macro turns an enum into an enum that can be used within bitfields. The "bits" argument specifies, how many the enum needs. Any mismatch would cause a compiler error.
- The exhaustive argument specifies whether every possible bit combination is contained within the enum. The example above has both an exhaustive and a non-exhaustive enum. Notice how the non-exhaustive enum has to be wrapped in an Option to account for the case of e2 not being one of the defined enum values.

## Arrays

Sometimes, bits inside of bitfields are repeated. To support this, this crate allows specifying bitwise arrays. For example, the following struct gives read/write access to each individual nibble (hex character) of a u64:

```
#[bitfield(u64, default: 0)]
struct Nibble64 {
     #[bits(0..=3, rw)]
     nibble: [u4; 16],
}
```

Arrays can also have a stride. This is useful in the case of multiple smaller values repeating. For example, the following definition provides access to each bit of each nibble:

```
#[bitfield(u64, default: 0)]
struct NibbleBits64 {
    #[bit(0, rw, stride: 4)]
    nibble_bit0: [bool; 16],

    #[bit(1, rw, stride: 4)]
    nibble_bit1: [bool; 16],

    #[bit(2, rw, stride: 4)]
    nibble_bit2: [bool; 16],

    #[bit(3, rw, stride: 4)]
    nibble_bit3: [bool; 16],
}
```

## Usage
Eventhough bitfields feel somewhat like structs, they are internally implemented as simple data types like u32. Therefore, they provide an immutable interface: Instead of changing the value of a field, any change operation will return a new bitfield with that field modified.

```
let a = NibbleBits64::new_with_raw_value(0x12345678_ABCDEFFF);
// Read a value
assert_eq!(u4::new(0xE), a.nibble(3));
// Change a value
let b = a.with_nibble(0, u4::new(0x3))
assert_eq!(0x12345678_ABCDEFF3, nibble.raw_value());
```
