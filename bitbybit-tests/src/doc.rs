//! This ensures that callers can compile even if they specify missing_docs
#![deny(missing_docs)]

use bitbybit::bitfield;

/// We put this here to verify the missing_docs above
#[bitfield(u32, default = 0)]
pub struct DocCheck {
    /// A field
    #[bits(2..=17, rw)]
    a: u16,
}
