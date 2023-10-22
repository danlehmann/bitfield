# Changelog

## bitbybit 1.3.0

### Changed

- Switched default attribute argument syntax from field type to assignment type (colon field style is still allowed, but might be deprecated in the future):
```rs
#[bitenum(u2, exhaustive = true)]
enum ExhaustiveEnum {
    Zero = 0b00,
    One = 0b01,
    Two = 0b10,
    Three = 0b11,
}

#[bitfield(u64, default = 0)]
struct BitfieldWithEnum {
    #[bits(2..=3, rw)]
    e2: Option<NonExhaustiveEnum>,

    #[bits(0..=1, rw)]
    e1: ExhaustiveEnum,
}
```

## bitbybit 1.2.2

### Added

- Bitfields can support any arbitrary-int as a base-data-type, not just built-ins. For example, this is now supported:
```rs
#[bitfield(u12)]
struct Bitfield {
  // bits...
}
```

### Changed

### Fixed

- Multi-line doc-comments on fields are now fully put into the resulting accessors (previously, just the last line was)
- Masking of signed fields setters is now correct


## bitbybit 1.2.1

### Added

- Experimental new `builder()`...`build()` syntax, which allows setting all values without the risk of forgetting any. Requires opt-in via new `experimental_builder_syntax` feature
- Bump to [arbitrary-int](https://crates.io/crates/arbitrary-int) version 1.2.6

### Changed

### Fixed

- Accessors for array fields now assert that the index is within the size of the array.
- Most usage errors are now associated correctly to the line where they happen, instead of at the top of the declaration.


## bitbybit 1.2.0

### Added

### Changed

- `new()` has caused some confusion - it's a harmless way to create a default. In practice, this wasn't really clear and people thought the function might read e.g. from hardware. `new()` is now deprecated. `default()` (or `DEFAULT` in const contexts) take its place.

### Fixed

- Reserved identifiers like `r#enum` or `r#priv` can now be used for field names