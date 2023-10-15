use proc_macro::TokenStream;

mod bitenum;
mod bitfield;

/// Defines a bitfield: `#[bitfield(<base-data-type>, default = 0)]`
/// `<base-data-type>` is a data type like [`u32`] which is used to represent all the bits of the bitfield.
/// `default` is an optional default when the bitfield is created
#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    bitfield::bitfield(args, input)
}

/// Defines a bitenum: `#[bitenum(<base-data-type>, exhaustive = true)]`
/// `<base-data-type>` is a data type like [`u2`] or [`u8`]
/// which is used the actually store the value of the bitfield.
/// `exhaustive` specifies whether the bitenum includes all possible value of the
/// given base data type (for example, a bitenum over [`u2`] with 4 values is exhaustive)
///
/// [`u2`]: arbitrary_int::u2
#[proc_macro_attribute]
pub fn bitenum(args: TokenStream, input: TokenStream) -> TokenStream {
    bitenum::bitenum(args, input)
}
