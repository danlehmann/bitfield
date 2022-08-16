use proc_macro::TokenStream;
use std::str::FromStr;

use proc_macro2::TokenTree;
use quote::{quote, ToTokens};
use syn::{Data, DeriveInput, Expr, Ident};
use syn::__private::TokenStream2;

pub fn bitenum(args: TokenStream, input: TokenStream) -> TokenStream {
    let args: Vec<_> = proc_macro2::TokenStream::from(args).into_iter().collect();

    let mut bits: Option<usize> = None;
    let mut exhaustive_value: Option<TokenStream2> = None;

    enum ArgumentType {
        Exhaustive,
    }
    let mut next_expected: Option<ArgumentType> = None;

    fn handle_next_expected(next_expected: &Option<ArgumentType>, default_value: &mut Option<TokenStream2>, token_stream: TokenStream2) {
        match next_expected {
            None => panic!("enum_raw_value!: Seen {}, but didn't expect anything. Example of valid syntax: #[enum_raw_value(u3, exhaustive: false)]", token_stream.to_string()),
            Some(ArgumentType::Exhaustive) => {
                *default_value = Some(token_stream);
            }
        }
    }
    for i in 0..args.len() {
        match &args[i] {
            TokenTree::Punct(p) => {
                match p.to_string().as_str() {
                    "," => next_expected = None,
                    ":" => {}
                    _ => panic!("enum_raw_value!: Expected ',' or ':' in argument list. Seen '{}'", p.to_string()),
                }
            }
            TokenTree::Ident(sym) => {
                if next_expected.is_some() {
                    // We might end up here if we refer to a constant, like 'exhaustive: SOME_CONSTANT'
                    handle_next_expected(&next_expected, &mut exhaustive_value, sym.to_token_stream());
                } else {
                    match sym.to_string().as_str() {
                        "exhaustive" => {
                            if exhaustive_value.is_some() {
                                panic!("enum_raw_value!: exhaustive must only be specified at most once");
                            }
                            next_expected = Some(ArgumentType::Exhaustive)
                        }
                        s => {
                            // See if this is a base datatype like u3
                            let size = if s.starts_with("u") {
                                let num = usize::from_str(s.split_at(1).1);
                                if let Ok(num) = num {
                                    if num <= 64 { Some(num) } else { None }
                                } else {
                                    None
                                }
                            } else {
                                None
                            };

                            match size {
                                Some(size) => bits = Some(size),
                                None => panic!("enum_raw_value!: Unexpected argument {}. Supported: u1, u2, u3, .., u64 and 'exhaustive'", sym.to_string()),
                            }
                        },
                    }
                }
            }
            TokenTree::Literal(literal) => {
                // We end up here if we see a literal, like 'exhaustive: true'
                let default_value = match next_expected {
                    None => { panic!() }
                    Some(ArgumentType::Exhaustive) => {
                        &mut exhaustive_value
                    }
                };
                handle_next_expected(&next_expected, default_value, literal.to_token_stream());
            }
            _ => {
                panic!("enum_raw_value!: Unexpected token. Example of valid syntax: #[enum_raw_value(u32, exhaustive: true)]")
            }
        }
    }

    let (bit_count, base_data_type, bounded_data_type, result_constructor, bounded_getter) =
        match bits {
            Some(bit_count) => {
                match bit_count {
                    b if b < 8 => (b, quote! { u8 }, quote! { arbitrary_int::UInt::<u8, #b> }, quote! { arbitrary_int::UInt::<u8, #b>::new }, quote! { .value() }),
                    b if b == 8 => (b, quote! { u8 }, quote! { u8 }, quote! { }, quote! { }),
                    b if b < 16 => (b, quote! { u16 }, quote! { arbitrary_int::UInt::<u16, #b> }, quote! { arbitrary_int::UInt::<u16, #b>::new }, quote! { .value() }),
                    b if b == 16 => (b, quote! { u16 }, quote! { u16 }, quote! { }, quote! { }),
                    b if b < 32 => (b, quote! { u32 }, quote! { arbitrary_int::UInt::<u32, #b> }, quote! { arbitrary_int::UInt::<u32, #b>::new }, quote! { .value() }),
                    b if b == 32 => (b, quote! { u32 }, quote! { u32 }, quote! { }, quote! { }),
                    b if b < 64 => (b, quote! { u64 }, quote! { arbitrary_int::UInt::<u64, #b> }, quote! { arbitrary_int::UInt::<u64, #b>::new }, quote! { .value() }),
                    b if b == 64 => (b, quote! { u64 }, quote! { u64 }, quote! { }, quote! { }),
                    _ => panic!("enum_raw_value!: Unhandled bits. Supported up to u64"),
                }
            }
            None => panic!("enum_raw_value!: datatype argument needed, for example #[enum_raw_value(u4, exhaustive: true)"),
        };



    let is_exhaustive = exhaustive_value.map(|x| x.to_string() == "true").unwrap_or(false);

    let input = syn::parse_macro_input!(input as DeriveInput);
    let enum_name = input.ident;
    let enum_vis = input.vis;
    let enum_attrs = input.attrs;

    let variants = match input.data {
        Data::Enum(enum_data) => enum_data.variants,
        _ => panic!("enum_raw_value!: Must be used on enum"),
    };
    let emitted_variants: Vec<(&Expr, u128, &Ident)> = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        let discriminant = (&variant.discriminant).as_ref().expect(format!("enum_raw_value!: Variant '{}' needs to have a value", variant_name).as_str());
        // Discriminant.0 is the equals sign. 1 is the value
        let value = &discriminant.1;
        let string_value = value.to_token_stream().to_string().replace("_", "");

        // Determine the integer value itself. While we don't need it further down (for now),
        // this ensures that only constants are being used; due to the way how new_with_raw_value()
        // is written, some expressions would cause compilation issues (e.g. those that refer to other
        // enum values).
        let int_value = if string_value.starts_with("0x") {
            u128::from_str_radix(&string_value[2..], 16)
        } else if string_value.starts_with("0b") {
            u128::from_str_radix(&string_value[2..], 2)
        } else if string_value.starts_with("0o") {
            u128::from_str_radix(&string_value[2..], 8)
        } else {
            u128::from_str_radix(&string_value, 10)
        }.expect(format!("enum_raw_value!: Error parsing '{}' as integer. Supported: hexadecimal, octal, binary and decimal unsigned integers, but not expressions", string_value).as_str());

        if int_value >= (1u128 << bit_count) {
            panic!("enum_raw_value!: Value {} exceeds the given number of bits", variant_name);
        }

        (value, int_value, variant_name)
    }).collect();

    // We tested the numeric values for out-of-bounds above. As enum values are unique integers,
    // we can now reason about the number of variants: If variants == 2^bits then we have to be exhaustive
    // (and if not, we can't be).
    let possible_maximum_variants = 1u128 << bit_count;
    if is_exhaustive {
        if emitted_variants.len() != possible_maximum_variants as usize {
            panic!("enum_raw_value!: Enum is marked as exhaustive, but it is missing variants")
        }
    } else {
        if emitted_variants.len() == possible_maximum_variants as usize {
            panic!("enum_raw_value!: Enum is exhaustive, but not marked accordingly. Add 'exhaustive: true'")
        }
    }

    // There are two ways to turn an int into an enum values:
    // - match cases against every single integer
    // - exclude unhandled integers, followed by transmute (unsafe)
    // For now, we'll go with the first option. If we find that the compiler generates bad code,
    // we can switch to the second option (as we required all values to be literals, so we can
    // analyse used vs unused ranges)

    let case_values: Vec<TokenStream2> = emitted_variants.iter().map(|(expression, _int_value, name)| {
        if is_exhaustive {
            quote! {
                #expression => Self::#name,
            }
        } else {
            quote! {
                #expression => Ok(Self::#name),
            }
        }
    }).collect();

    let constructor_function = if is_exhaustive {
        let panic_string = format!("{}: Unhandled value", enum_name);
        quote!(
            pub const fn new_with_raw_value(value: #bounded_data_type) -> Self {
                match value #bounded_getter {
                    #( #case_values )*
                    _ => panic!(#panic_string)
                }
            }
        )
    } else {
        quote!(
            pub const fn new_with_raw_value(value: #bounded_data_type) -> Result<Self, #base_data_type> {
                match value #bounded_getter {
                    #( #case_values )*
                    _ => Err(value #bounded_getter)
                }
            }
        )
    };

    let expanded = quote! {
        #[derive(Copy, Clone)]
        #( #enum_attrs )*
        #enum_vis enum #enum_name {
            #variants
        }

        impl #enum_name {
            pub const fn raw_value(self) -> #bounded_data_type { #result_constructor(self as #base_data_type) }

            #constructor_function
        }
    };
    //println!("Expanded: {}", expanded.to_string());
    TokenStream::from(expanded)
}