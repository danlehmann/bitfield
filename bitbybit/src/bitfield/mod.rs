mod codegen;
mod parsing;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro2::{Ident, TokenTree};
use quote::{quote, ToTokens};
use std::ops::Range;
use std::str::FromStr;
use syn::{Attribute, Data, DeriveInput, Type};

/// In the code below, bools are considered to have 0 bits. This lets us distinguish them
/// from u1
const BITCOUNT_BOOL: usize = 0;

/// Returns true if the number can be expressed by a regular data type like u8 or u32.
/// 0 is special as it means bool (technically should be 1, but we use that for u1)
const fn is_int_size_regular_type(size: usize) -> bool {
    size == BITCOUNT_BOOL || size == 8 || size == 16 || size == 32 || size == 64 || size == 128
}

fn parse_arbitrary_int_type(s: &str) -> Result<usize, ()> {
    if !s.starts_with('u') || s.len() < 2 {
        return Err(());
    }

    let size = usize::from_str(s.split_at(1).1);
    match size {
        Ok(size) => {
            if size >= 1 && size < 128 && !is_int_size_regular_type(size) {
                Ok(size)
            } else {
                Err(())
            }
        }
        Err(_) => Err(()),
    }
}

// If a convert_type is given, that will be the final getter/setter type. If not, it is the base type
enum CustomType {
    No,
    Yes(Type),
}

#[derive(Copy, Clone)]
struct BaseDataSize {
    /// The size of the raw_value field, e.g. u32
    internal: usize,

    /// The size exposed via raw_value() and new_with_raw_value(), e.g. u24
    exposed: usize,
}

impl BaseDataSize {
    const fn new(size: usize) -> Self {
        let built_in_size = if size <= 8 {
            8
        } else if size <= 16 {
            16
        } else if size <= 32 {
            32
        } else if size <= 64 {
            64
        } else {
            128
        };
        assert!(size <= built_in_size);
        Self {
            internal: built_in_size,
            exposed: size,
        }
    }
}

pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let args: Vec<_> = proc_macro2::TokenStream::from(args).into_iter().collect();

    if args.is_empty() {
        panic!(
            "bitfield! No arguments given, but need at least base data type (e.g. 'bitfield(u32)')"
        );
    }

    // Parse arguments: the first argument is required and has the base data type. Further arguments are
    // optional and are key:value pairs
    let base_data_type = &args[0];
    let mut default_value: Option<TokenStream2> = None;

    enum ArgumentType {
        Default,
    }
    let mut next_expected: Option<ArgumentType> = None;

    fn handle_next_expected(
        next_expected: &Option<ArgumentType>,
        default_value: &mut Option<TokenStream2>,
        token_stream: TokenStream2,
    ) {
        match next_expected {
            None => panic!("bitfield!: Unexpected token {}. Example of valid syntax: #[bitfield(u32, default = 0)]", token_stream),
            Some(ArgumentType::Default) => {
                *default_value = Some(token_stream);
            }
        }
    }
    for arg in args.iter().skip(1) {
        match arg {
            TokenTree::Punct(p) => match p.as_char() {
                ',' => next_expected = None,
                '=' | ':' => (),
                _ => panic!(
                    "bitfield!: Expected ',', '=' or ':' in argument list. Saw '{}'",
                    p
                ),
            },
            TokenTree::Ident(sym) => {
                if next_expected.is_some() {
                    // We might end up here if we refer to a constant, like 'default = SOME_CONSTANT'
                    handle_next_expected(&next_expected, &mut default_value, sym.to_token_stream());
                } else {
                    match sym.to_string().as_str() {
                        "default" => {
                            if default_value.is_some() {
                                panic!("bitfield!: default must only be specified at most once");
                            }
                            next_expected = Some(ArgumentType::Default)
                        }
                        _ => panic!(
                            "bitfield!: Unexpected argument {}. Supported: 'default'",
                            sym
                        ),
                    }
                }
            }
            TokenTree::Literal(literal) => {
                // We end up here if we see a literal, like 'default = 0x1234'
                handle_next_expected(
                    &next_expected,
                    &mut default_value,
                    literal.to_token_stream(),
                );
            }
            t => panic!("bitfield!: Unexpected token {}. Example of valid syntax: #[bitfield(u32, default = 0)]", t),
        }
    }

    // If an arbitrary-int is specified as a base-type, we only use that when exposing it
    // (e.g. through raw_value() and for bounds-checks). The actual raw_value field will be the next
    // larger integer field
    let base_data_size = match base_data_type.to_string().as_str() {
        "u8" => BaseDataSize::new(8),
        "u16" => BaseDataSize::new(16),
        "u32" => BaseDataSize::new(32),
        "u64" => BaseDataSize::new(64),
        "u128" => BaseDataSize::new(128),
        s if parse_arbitrary_int_type(s).is_ok() => {
            BaseDataSize::new(parse_arbitrary_int_type(s).unwrap())
        }
        _ => {
            return syn::Error::new_spanned(
                &base_data_type,
                format!("bitfield!: Supported values for base data type are u8, u16, u32, u64, u128. {} is invalid", base_data_type.to_string().as_str()),
            ).to_compile_error().into();
        }
    };
    let internal_base_data_type =
        syn::parse_str::<Type>(format!("u{}", base_data_size.internal).as_str())
            .unwrap_or_else(|_| panic!("bitfield!: Error parsing internal base data type"));

    let input = syn::parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let struct_vis = input.vis;
    let struct_attrs = input.attrs;

    let fields = match input.data {
        Data::Struct(struct_data) => struct_data.fields,
        _ => panic!("bitfield!: Must be used on struct"),
    };

    let field_definitions = match parsing::parse(&fields, base_data_size) {
        Ok(definitions) => definitions,
        Err(token_stream) => return token_stream.into(),
    };
    let accessors = codegen::generate(&field_definitions, base_data_size, &internal_base_data_type);

    let (default_constructor, default_trait) = if let Some(default_value) = default_value.clone() {
        let constructor = {
            let comment = format!("An instance that uses the default value {}", default_value);
            let deprecated_warning = format!(
                "Use {}::Default (or {}::DEFAULT in const context) instead",
                struct_name, struct_name
            );

            let default_raw_value = if base_data_size.exposed == base_data_size.internal {
                quote! { const DEFAULT_RAW_VALUE: #base_data_type = #default_value; }
            } else {
                quote! { const DEFAULT_RAW_VALUE: #base_data_type = #base_data_type::new(#default_value); }
            };
            quote! {
                #default_raw_value

                #[doc = #comment]
                #[inline]
                pub const DEFAULT: Self = Self::new_with_raw_value(Self::DEFAULT_RAW_VALUE);

                #[deprecated(note = #deprecated_warning)]
                pub const fn new() -> Self {
                    Self::DEFAULT
                }
            }
        };

        let default_trait = quote! {
            impl Default for #struct_name {
                fn default() -> Self {
                    Self::DEFAULT
                }
            }
        };

        (constructor, default_trait)
    } else {
        (quote! {}, quote! {})
    };

    let (new_with_constructor, new_with_builder_chain) = codegen::make_builder(
        &struct_name,
        default_value.is_some(),
        &struct_vis,
        &internal_base_data_type,
        base_data_type,
        base_data_size,
        &field_definitions,
    );

    let raw_value_unwrap = if base_data_size.exposed == base_data_size.internal {
        quote! { value }
    } else {
        quote! { value.value() }
    };

    let raw_value_wrap = if base_data_size.exposed == base_data_size.internal {
        quote! { self.raw_value }
    } else {
        // We use extract as that - unlike new() - never panics. This macro already guarantees that
        // the upper bits can't be set.
        let extract =
            syn::parse_str::<Type>(format!("extract_u{}", base_data_size.internal).as_str())
                .unwrap_or_else(|_| panic!("bitfield!: Error parsing one literal"));
        quote! { #base_data_type::#extract(self.raw_value, 0) }
    };

    let expanded = quote! {
        #[derive(Copy, Clone)]
        #[repr(C)]
        #( #struct_attrs )*
        #struct_vis struct #struct_name {
            raw_value: #internal_base_data_type,
        }

        impl #struct_name {
            #default_constructor
            /// Returns the underlying raw value of this bitfield
            #[inline]
            pub const fn raw_value(&self) -> #base_data_type { #raw_value_wrap }

            /// Creates a new instance of this bitfield with the given raw value.
            ///
            /// No checks are performed on the value, so it is possible to set bits that don't have any
            /// accessors specified.
            #[inline]
            pub const fn new_with_raw_value(value: #base_data_type) -> #struct_name { #struct_name { raw_value: #raw_value_unwrap } }

            #new_with_constructor

            #( #accessors )*
        }
        #default_trait
        #( #new_with_builder_chain )*
    };
    //println!("Expanded: {}", expanded.to_string());
    TokenStream::from(expanded)
}

fn setter_name(field_name: &Ident) -> Ident {
    // The field might have started with r#. If so, it was likely used for a keyword. This can be dropped here
    let field_name_without_prefix = {
        let s = field_name.to_string();
        if s.starts_with("r#") {
            s[2..].to_string()
        } else {
            s
        }
    };

    syn::parse_str::<Ident>(format!("with_{}", field_name_without_prefix).as_str())
        .unwrap_or_else(|_| panic!("bitfield!: Error creating setter name"))
}

struct FieldDefinition {
    field_name: Ident,
    ranges: Vec<Range<usize>>,
    unsigned_field_type: Option<Type>,
    array: Option<(usize, usize)>,
    field_type_size: usize,
    getter_type: Option<Type>,
    setter_type: Option<Type>,
    field_type_size_from_data_type: Option<usize>,
    /// If non-null: (count, stride)
    use_regular_int: bool,
    primitive_type: TokenStream2,
    custom_type: CustomType,
    doc_comment: Vec<Attribute>,
}
