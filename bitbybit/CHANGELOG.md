# Changelog

## bitbybit 1.2.1

### Added

### Changed

### Fixed

- Accessors for array fields now assert that the index is within the size of the array.


## bitbybit 1.2.0

### Added

### Changed

- `new()` has caused some confusion - it's a harmless way to create a default. In practice, this wasn't really clear and people thought the function might read e.g. from hardware. `new()` is now deprecated. `default()` (or `DEFAULT` in const contexts) take its place.

### Fixed

- Reserved identifiers like `r#enum` or `r#priv` can now be used for field names