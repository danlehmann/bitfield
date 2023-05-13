use proc_macro::TokenStream;
use syn::punctuated::Punctuated;
use syn::token::{Comma, Or};
use std::ops::RangeInclusive;
use std::str::FromStr;

use proc_macro2::{TokenTree};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::__private::TokenStream2;
use syn::{Attribute, Data, DeriveInput, Expr, Ident, ExprRange, Variant, Lit, RangeLimits};

const CUSTOM_VARIANT_ATTRIBUTES: [&'static str; 3] = [
    "range",
    "ranges",
    "catchall",
];

struct BitenumVariant<'a> {
    variant: &'a Variant,
    emit_discriminant: bool,
}

impl ToTokens for BitenumVariant<'_> {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        // Remove custom attributes, but still emit any we don't recognize
        let non_elided_attrs = self.variant.attrs.iter().filter(|attr| {
            if let Some(ident) = attr.path().get_ident() {
                !CUSTOM_VARIANT_ATTRIBUTES.contains(&ident.to_string().as_str())
            } else {
                true
            }
        });
        tokens.append_all(non_elided_attrs);
        self.variant.ident.to_tokens(tokens);
        self.variant.fields.to_tokens(tokens);
        if self.emit_discriminant {
            if let Some((eq_token, disc)) = &self.variant.discriminant {
                eq_token.to_tokens(tokens);
                disc.to_tokens(tokens);
            }
        }
    }
}

pub fn bitenum(args: TokenStream, input: TokenStream) -> TokenStream {
    let args: Vec<_> = proc_macro2::TokenStream::from(args).into_iter().collect();

    let mut bits: Option<usize> = None;
    let mut exhaustive_value: Option<TokenStream2> = None;

    enum ArgumentType {
        Exhaustive,
    }
    let mut next_expected: Option<ArgumentType> = None;

    fn handle_next_expected(
        next_expected: &Option<ArgumentType>,
        default_value: &mut Option<TokenStream2>,
        token_stream: TokenStream2,
    ) {
        match next_expected {
            None => panic!("bitenum!: Seen {}, but didn't expect anything. Example of valid syntax: #[bitenum(u3, exhaustive: false)]", token_stream),
            Some(ArgumentType::Exhaustive) => {
                *default_value = Some(token_stream);
            }
        }
    }
    for arg in args {
        match arg {
            TokenTree::Punct(p) => match p.as_char() {
                ',' => next_expected = None,
                ':' => {}
                _ => panic!(
                    "bitenum!: Expected ',' or ':' in argument list. Seen '{}'",
                    p
                ),
            },
            TokenTree::Ident(sym) => {
                if next_expected.is_some() {
                    // We might end up here if we refer to a constant, like 'exhaustive: SOME_CONSTANT'
                    handle_next_expected(
                        &next_expected,
                        &mut exhaustive_value,
                        sym.to_token_stream(),
                    );
                } else {
                    match sym.to_string().as_str() {
                        "exhaustive" => {
                            if exhaustive_value.is_some() {
                                panic!("bitenum!: exhaustive must only be specified at most once");
                            }
                            next_expected = Some(ArgumentType::Exhaustive)
                        }
                        s => {
                            // See if this is a base datatype like u3
                            let size = if s.starts_with('u') {
                                let num = usize::from_str(s.split_at(1).1);
                                if let Ok(num) = num {
                                    if num <= 64 {
                                        Some(num)
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            } else {
                                None
                            };

                            match size {
                                Some(size) => bits = Some(size),
                                None => panic!("bitenum!: Unexpected argument {}. Supported: u1, u2, u3, .., u64 and 'exhaustive'", sym),
                            }
                        }
                    }
                }
            }
            TokenTree::Literal(literal) => {
                // We end up here if we see a literal, like 'exhaustive: true'
                let default_value = match next_expected {
                    None => {
                        panic!()
                    }
                    Some(ArgumentType::Exhaustive) => &mut exhaustive_value,
                };
                handle_next_expected(&next_expected, default_value, literal.to_token_stream());
            }
            _ => {
                panic!("bitenum!: Unexpected token. Example of valid syntax: #[bitenum(u32, exhaustive: true)]")
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
                    _ => panic!("bitenum!: Unhandled bits. Supported up to u64"),
                }
            }
            None => panic!("bitenum!: datatype argument needed, for example #[bitenum(u4, exhaustive: true)"),
        };

    #[derive(PartialEq, Eq)]
    enum Exhaustiveness {
        True,
        False,
        Conditional,
    }

    let exhaustiveness = exhaustive_value
        .map(|x| match x.to_string().as_str() {
            "true" => Exhaustiveness::True,
            "false" => Exhaustiveness::False,
            "conditional" => Exhaustiveness::Conditional,
            _ => panic!("bitenum!: \"exhaustive\" must be \"true\", \"false\" or \"conditional\""),
        })
        .unwrap_or(Exhaustiveness::False);

    let input = syn::parse_macro_input!(input as DeriveInput);
    let enum_name = input.ident;
    let enum_vis = input.vis;
    let enum_attrs = input.attrs;

    let variants = match input.data {
        Data::Enum(enum_data) => enum_data.variants,
        _ => panic!("bitenum!: Must be used on enum"),
    };
    let mut uses_conditional = false;

    enum VariantValue<'a> {
        SingleValue(&'a Expr, u128),
        Ranges(Vec<(ExprRange, RangeInclusive<u128>)>),
        Catchall,
    }

    let mut has_catchall = false;
    let mut has_ranges = false;
    let max_value = (1u128 << bit_count) - 1;

    let mut emitted_variants: Vec<(VariantValue, &Ident, Vec<Attribute>)> = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        let value = {
            let range_attrs = variant.attrs
                .iter()
                .filter(|attr| { attr.path().is_ident("range") || attr.path().is_ident("ranges") });
            let range_attr_count = range_attrs.clone().count();

            let catchall_attrs = variant.attrs
                .iter()
                .filter(|attr| { attr.path().is_ident("catchall") });
            let catchall_attr_count = catchall_attrs.clone().count();

            if range_attr_count > 0 && catchall_attr_count > 0 {
                panic!("bitenum!: variant cannot have both range and catchall attributes");
            }

            if catchall_attr_count > 1 {
                panic!("bitenum!: catchall attribute must only be specified once");
            }

            if range_attr_count > 1 {
                panic!("bitenum!: range/ranges attribute must only be specified once");
            }

            if catchall_attr_count > 0 {
                if has_catchall {
                    panic!("bitenum!: only one variant can be designated as the catchall");
                }

                has_catchall = true;
                VariantValue::Catchall
            } else if range_attr_count > 0 {
                has_ranges = true;
                let range_attr = range_attrs.clone().nth(0).unwrap();
                let attr_args: Expr = range_attr.parse_args().expect("bitenum: failed to parse range attribute");

                let expr_ranges = if range_attr.path().is_ident("ranges") {
                    match attr_args {
                        Expr::Array(t) => t.elems.iter().map(|expr| match expr {
                            Expr::Range(e) => e.clone(),
                            _ => panic!("bitenum!: ranges elements are not all valid ranges"),
                        }).collect::<Vec<ExprRange>>(),
                        _ => panic!("bitenum!: ranges attribute argument must be an array of ranges"),
                    }
                } else {
                    match attr_args {
                        Expr::Range(e) => [e].to_vec(),
                        _ => panic!("bitenum!: range attribute argument must be a range"),
                    }
                };

                let range_info = expr_ranges.iter().map(|expr_range| {
                    let start = match expr_range.clone().start.expect("bitenum: range must have start and end bounds").as_ref() {
                        Expr::Lit(expr_lit) => match &expr_lit.lit {
                            Lit::Int(lit_int) => {
                                lit_int.base10_parse::<u128>().expect("bitenum: failed to parse range start")
                            },
                            _ => panic!("bitenum!: invalid range start"),
                        },
                        _ => panic!("bitenum!: invalid range start"),
                    };
                    let end = match expr_range.clone().end.expect("bitenum: range must have start and end bounds").as_ref() {
                        Expr::Lit(expr_lit) => match &expr_lit.lit {
                            Lit::Int(lit_int) => {
                                lit_int.base10_parse::<u128>().expect("bitenum: failed to parse range end")
                            },
                            _ => panic!("bitenum!: invalid range start"),
                        },
                        _ => panic!("bitenum!: invalid range end"),
                    };
                    match expr_range.clone().limits {
                        RangeLimits::HalfOpen(_) => panic!("bitenum!: range must be inclusive"),
                        _ => (),
                    };

                    if start > max_value {
                        panic!("bitenum!: range start for {variant_name} exceeds the given number of bits")
                    }

                    if end > max_value {
                        panic!("bitenum!: range end for {variant_name} exceeds the given number of bits")
                    }

                    (expr_range.clone(), start..=end)
                }).collect();
                VariantValue::Ranges(range_info)
            } else {
                let discriminant = variant.discriminant.as_ref().unwrap_or_else(|| panic!("bitenum!: Variant '{}' needs to have a value", variant_name));
                // Discriminant.0 is the equals sign. 1 is the value
                let value = &discriminant.1;
                let string_value = value.to_token_stream().to_string().replace('_', "");

                // Determine the integer value itself. This ensures that only constants are being
                // used; due to the way how new_with_raw_value() is written, some expressions would
                // cause compilation issues (e.g. those that refer to other enum values).
                let int_value = if let Some(stripped) = string_value.strip_prefix("0x") {
                    u128::from_str_radix(stripped, 16)
                } else if let Some(stripped) = string_value.strip_prefix("0b") {
                    u128::from_str_radix(stripped, 2)
                } else if let Some(stripped) = string_value.strip_prefix("0o") {
                    u128::from_str_radix(stripped, 8)
                } else {
                    string_value.parse::<u128>()
                }.unwrap_or_else(|_| panic!("bitenum!: Error parsing '{}' as integer. Supported: hexadecimal, octal, binary and decimal unsigned integers, but not expressions", string_value));

                if int_value > max_value {
                    panic!("bitenum!: Value {} exceeds the given number of bits", variant_name);
                }

                VariantValue::SingleValue(value, int_value)
            }
        };

        let mut cfg_attributes = Vec::new();

        for attr in &variant.attrs {
            if attr.path().is_ident("cfg") {
                cfg_attributes.push(attr.clone());
                uses_conditional = true;
            }
        }

        (value, variant_name, cfg_attributes)
    }).collect();

    // Put the catchall last
    emitted_variants.sort_by_key(|(value, _, _)| match value {
        VariantValue::SingleValue(_, _) => 0,
        VariantValue::Ranges(_) => 0,
        VariantValue::Catchall => 10,
    });

    if uses_conditional {
        if exhaustiveness != Exhaustiveness::Conditional {
            panic!("bitenum!: If any values are marked as conditional (using the cfg attribute), the enum must be marked as 'exhaustive: conditional'");
        }
    } else {
        if exhaustiveness == Exhaustiveness::Conditional {
            panic!("bitenum!: No values are conditionally compiled using cfg, so the enum must not be marked as conditional. Change to 'exhaustive: true' or 'exhaustive: false'");
        }
    }

    if has_catchall && exhaustiveness == Exhaustiveness::False {
        panic!("bitenum!: if there's a catch-all variant, enum is necessarily exhaustive");
    }

    let return_is_result = if exhaustiveness == Exhaustiveness::Conditional {
        true
    } else {
        let mut ranges: Vec<RangeInclusive<u128>> = Vec::new();
        for (value, _, _) in &emitted_variants {
            match value {
                VariantValue::SingleValue(_, v) => ranges.push(*v..=*v),
                VariantValue::Ranges(range_info) => {
                    for (_, range) in range_info {
                        ranges.push(range.clone());
                    }
                }
                VariantValue::Catchall => (),
            }
        }
        ranges.sort_by_key(|r| *r.start());

        let mut has_holes = false;
        let mut last_end: Option<u128> = None;
        for range in ranges {
            if let Some(last_end) = last_end {
                if *range.start() != last_end + 1 {
                    has_holes = true;
                }
                if *range.start() <= last_end {
                    panic!("bitenum!: one or more value in range 0x{:02x}..=0x{:02x} is covered by multiple variants", range.start(), last_end);
                }
            } else {
                has_holes = *range.start() > 0;
            }
            last_end = Some(*range.end());
        }

        let covers_all_values = !has_holes && last_end == Some(max_value);

        match exhaustiveness {
            Exhaustiveness::True => {
                if !has_catchall && !covers_all_values {
                    panic!("bitenum!: Enum is marked as exhaustive, but it is missing variants")
                }
                if has_catchall && covers_all_values {
                    panic!("bitenum!: Enum has a catchall variant, but all possible values are already covered")
                }
                false
            }
            Exhaustiveness::False => {
                if covers_all_values {
                    panic!("bitenum!: Enum is exhaustive, but not marked accordingly. Add 'exhaustive: true'")
                }
                true
            }
            Exhaustiveness::Conditional => {
                // Shouldn't get here because we shouldn't have done an exhaustiveness check
                panic!("bitenum!: internal error");
            }
        }
    };

    // There are two ways to turn an int into an enum values:
    // - match cases against every single integer
    // - exclude unhandled integers, followed by transmute (unsafe)
    // For now, we'll go with the first option. If we find that the compiler generates bad code,
    // we can switch to the second option (as we required all values to be literals, so we can
    // analyse used vs unused ranges)

    let case_values: Vec<TokenStream2> = emitted_variants
        .iter()
        .map(|(value, name, cfg_attributes)| {
            let (case_expression, case_value_expression) = match value {
                VariantValue::SingleValue(expression, _) => (expression.to_token_stream(), quote! { Self::#name }),
                VariantValue::Ranges(range_info) => {
                    let case_expression = range_info.iter().map(|(expr_range, _)| expr_range).collect::<Punctuated<&ExprRange, Or>>();
                    (case_expression.to_token_stream(), quote! { Self::#name(value) })
                },
                VariantValue::Catchall => (quote! { _ }, quote! { Self::#name(value) }),
            };
            if return_is_result {
                quote! {
                    #( #cfg_attributes )*
                    #case_expression => Ok(#case_value_expression),
                }
            } else {
                quote! {
                    #( #cfg_attributes )*
                    #case_expression => #case_value_expression,
                }
            }
        })
        .collect();

    let constructor_function = if return_is_result {
        quote!(
            /// Creates a new instance of this bitfield with the given raw value, or
            /// Err(value) if the value does not exist in the enum.
            pub const fn new_with_raw_value(value: #bounded_data_type) -> Result<Self, #base_data_type> {
                match value #bounded_getter {
                    #( #case_values )*
                    _ => Err(value #bounded_getter)
                }
            }
        )
    } else {
        let panic_string = format!("{}: Unhandled value", enum_name);
        quote!(
            /// Creates a new instance of this bitfield with the given raw value.
            ///
            /// As the enum is exhaustive, this function will always return a valid result
            pub const fn new_with_raw_value(value: #bounded_data_type) -> Self {
                match value #bounded_getter {
                    #( #case_values )*
                    _ => panic!(#panic_string)
                }
            }
        )
    };

    // A primitive enum just has variants that each have a single value. For this kind of
    // enum, the `as` keyword can be used to cast an instance to an integer.
    let is_primitive = !has_ranges && !has_catchall;

    let wrapped_variants = variants
        .iter()
        .map(|variant| {
            BitenumVariant {
                variant,
                // If a non-primitive enum has any variants with discriminants, `#[repr]` must be
                // specified. We shouldn't emit that because user code may already specify it. Work
                // around this by not emitting the discriminants for non-primitive bitenums. This
                // shouldn't make a practical because discriminants only affect casting using `as`,
                // which already doesn't work for non-primitive enums.
                emit_discriminant: is_primitive
            }
        })
        .collect::<Punctuated<BitenumVariant, Comma>>();

    let raw_value_function = if is_primitive {
        quote! {
            /// Returns the underlying raw value of this bitfield
            pub const fn raw_value(self) -> #bounded_data_type { #result_constructor(self as #base_data_type) }
        }
    } else {
        let raw_value_cases: Punctuated<TokenStream2, Comma> = emitted_variants.iter().map(|(value, name, cfg_attributes)| {
            match value {
                VariantValue::SingleValue(expression, _) => quote! {
                    #( #cfg_attributes )*
                    Self::#name => #result_constructor(#expression)
                },
                VariantValue::Ranges(_) | VariantValue::Catchall => quote! {
                    #( #cfg_attributes )*
                    Self::#name(v) => v
                },
            }
        })
        .collect();

        quote! {
            /// Returns the underlying raw value of this bitfield
            pub const fn raw_value(self) -> #bounded_data_type {
                match self {
                    #raw_value_cases
                }
            }
        }
    };

    let expanded = quote! {
        #[derive(Copy, Clone)]
        #( #enum_attrs )*
        #enum_vis enum #enum_name {
            #wrapped_variants
        }

        impl #enum_name {
            #raw_value_function

            #constructor_function
        }
    };
    //println!("Expanded: {}", expanded.to_string());
    TokenStream::from(expanded)
}
