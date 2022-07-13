use proc_macro::TokenStream;

mod bitenum;
mod bitfield;

/// Defines a bitfield: #[bitfield(<base-data-type>, default: 0)]
/// <base-data-type> is a data type like u32 which is used to represent all the bits of the bitfield.
/// default is an optional default when the bitfield is created
#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    bitfield::bitfield(args, input)
}

#[proc_macro_attribute]
pub fn bitenum(args: TokenStream, input: TokenStream) -> TokenStream {
    bitenum::bitenum(args, input)
}
