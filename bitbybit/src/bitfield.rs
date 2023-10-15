use proc_macro::TokenStream;
use std::ops::{Deref, Range};
use std::str::FromStr;

use proc_macro2::{Ident, TokenTree};
use quote::{quote, ToTokens};
use syn::__private::TokenStream2;
use syn::{Attribute, Data, DeriveInput, Field, GenericArgument, PathArguments, Type, Visibility};

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
                    // We might end up here if we refer to a constant, like 'default: SOME_CONSTANT'
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
                // We end up here if we see a literal, like 'default: 0x1234'
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

    let one = syn::parse_str::<syn::LitInt>(format!("1u{}", base_data_size.internal).as_str())
        .unwrap_or_else(|_| panic!("bitfield!: Error parsing one literal"));

    let input = syn::parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let struct_vis = input.vis;
    let struct_attrs = input.attrs;

    let fields = match input.data {
        Data::Struct(struct_data) => struct_data.fields,
        _ => panic!("bitfield!: Must be used on struct"),
    };

    let mut field_definitions = Vec::with_capacity(fields.len());

    for field in fields {
        match parse_field(base_data_size.internal, &field) {
            Ok(def) => field_definitions.push(def),
            Err(ts) => return ts.into(),
        }
    }

    let accessors: Vec<TokenStream2> = field_definitions.iter().map(|field_definition| {
        let field_name = &field_definition.field_name;
        let lowest_bit = field_definition.lowest_bit;
        let primitive_type = &field_definition.primitive_type;
        let doc_comment = &field_definition.doc_comment;
        let number_of_bits = field_definition.number_of_bits;
        let getter =
            if let Some(getter_type) = field_definition.getter_type.as_ref() {
                let extracted_bits = if field_definition.use_regular_int {
                    // Extract standard type (u8, u16 etc)
                    if let Some(array) = field_definition.array {
                        let indexed_stride = array.1;
                        if field_definition.field_type_size == BITCOUNT_BOOL {
                            quote! { (self.raw_value & (#one << (#lowest_bit + index * #indexed_stride))) != 0 }
                        } else {
                            quote! { ((self.raw_value >> (#lowest_bit + index * #indexed_stride)) & ((#one << #number_of_bits) - #one)) as #primitive_type }
                        }
                    } else if field_definition.field_type_size == BITCOUNT_BOOL {
                        quote! { (self.raw_value & (#one << #lowest_bit)) != 0 }
                    } else if field_definition.number_of_bits == base_data_size.internal {
                        // If the field is the whole size of the bitfield, we can't apply a mask
                        // as that would overflow. However, we don't need to
                        assert_eq!(field_definition.lowest_bit, 0);
                        quote! { self.raw_value as #primitive_type }
                    } else {
                        quote! {
                            ((self.raw_value >> #lowest_bit) & ((#one << #number_of_bits) - #one)) as #primitive_type
                        }
                    }
                } else {
                    // Extract arbitrary int (e.g. u7), using one of the extract methods
                    let custom_type = TokenStream2::from_str(format!("arbitrary_int::u{}", field_definition.number_of_bits).as_str()).unwrap();
                    let extract = TokenStream2::from_str(format!("extract_u{}", base_data_size.internal).as_str()).unwrap();
                    if let Some(array) = field_definition.array {
                        let indexed_stride = array.1;
                        quote! {
                            #custom_type::#extract(self.raw_value, #lowest_bit + index * #indexed_stride)
                        }
                    } else {
                        quote! {
                            #custom_type::#extract(self.raw_value, #lowest_bit)
                        }
                    }
                };

                let converted = match &field_definition.custom_type {
                    CustomType::No => extracted_bits,
                    CustomType::Yes(convert_type) => {
                        quote! {
                            let extracted_bits = #extracted_bits;
                            #convert_type::new_with_raw_value(extracted_bits)
                        }
                    }
                };

                if let Some(array) = field_definition.array {
                    let indexed_count = array.0;
                    quote! {
                        #(#doc_comment)*
                        #[inline]
                        pub const fn #field_name(&self, index: usize) -> #getter_type {
                            assert!(index < #indexed_count);
                            #converted
                        }
                    }
                } else {
                    quote! {
                        #(#doc_comment)*
                        #[inline]
                        pub const fn #field_name(&self) -> #getter_type {
                            #converted
                        }
                    }
                }
            } else {
                quote! {}
            };

        let setter = if let Some(setter_type) = field_definition.setter_type.as_ref() {
            let argument_converted =
                match field_definition.custom_type {
                    CustomType::No => {
                        if field_definition.use_regular_int {
                            // For signed types, we first have to convert to the unsigned type. Then up the base type
                            // (e.g. i16 would go: field_value as u16 as u64)
                            if let Some(unsigned_field_type) = &field_definition.unsigned_field_type {
                                quote! { field_value as #unsigned_field_type }
                            } else {
                                quote! { field_value }
                            }
                        } else {
                            // Once signed arbitrary-ints (e.g. i7) are a thing, we'll need to pay special attention to sign extension here
                            quote! { field_value.value() }
                        }
                    }
                    CustomType::Yes(_) => {
                        // Once signed bitenum or bitfield-base-data-types are a thing, we'll need to pay special attention to sign extension here
                        if field_definition.use_regular_int {
                            quote! { field_value.raw_value() }
                        } else {
                            quote! { field_value.raw_value().value() }
                        }
                    }
                };

            let new_raw_value = if let Some(array) = field_definition.array {
                let indexed_stride = array.1;
                // bool?
                if field_definition.field_type_size_from_data_type == Some(BITCOUNT_BOOL) {
                    quote! {
                        {
                            let effective_index = #lowest_bit + index * #indexed_stride;
                            if #argument_converted { self.raw_value | (#one << effective_index) } else { self.raw_value & !(#one << effective_index) }
                        }
                    }
                } else {
                    quote! {
                        {
                            let effective_index = #lowest_bit + index * #indexed_stride;
                            (self.raw_value & !(((#one << #number_of_bits) - #one) << effective_index)) | ((#argument_converted as #internal_base_data_type) << effective_index)
                        }
                    }
                }
            } else if field_definition.field_type_size_from_data_type == Some(BITCOUNT_BOOL) {
                quote! {
                    if #argument_converted { self.raw_value | (#one << #lowest_bit) } else { self.raw_value & !(#one << #lowest_bit) }
                }
            } else if field_definition.number_of_bits == base_data_size.internal {
                // If the field is the whole size of the bitfield, we can't apply a mask
                // as that would overflow. However, we don't need to
                assert_eq!(field_definition.lowest_bit, 0);
                quote! { #argument_converted as #internal_base_data_type }
            } else {
                quote! { (self.raw_value & !(((#one << #number_of_bits) - #one) << #lowest_bit)) | ((#argument_converted as #internal_base_data_type) << #lowest_bit) }
            };

            let setter_name = setter_name(field_name);

            if let Some(array) = field_definition.array {
                let indexed_count = array.0;
                quote! {
                    #(#doc_comment)*
                    #[inline]
                    pub const fn #setter_name(&self, index: usize, field_value: #setter_type) -> Self {
                        assert!(index < #indexed_count);
                        Self {
                            raw_value: #new_raw_value
                        }
                    }
                }
            } else {
                quote! {
                    #(#doc_comment)*
                    #[inline]
                    pub const fn #setter_name(&self, field_value: #setter_type) -> Self {
                        Self {
                            raw_value: #new_raw_value
                        }
                    }
                }
            }
        } else {
            quote! {}
        };

        quote! {
            #getter
            #setter
        }
    }).collect();

    let (default_constructor, default_trait) = if let Some(default_value) = default_value.clone() {
        (
            {
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
            },
            quote! {
                impl Default for #struct_name {
                    fn default() -> Self {
                        Self::DEFAULT
                    }
                }
            },
        )
    } else {
        (quote! {}, quote! {})
    };

    let (new_with_constructor, new_with_builder_chain) = make_new_with_constructor(
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
    // println!("Expanded: {}", expanded.to_string());
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

fn make_new_with_constructor(
    struct_name: &Ident,
    has_default: bool,
    struct_vis: &Visibility,
    internal_base_data_type: &Type,
    base_data_type: &TokenTree,
    base_data_size: BaseDataSize,
    field_definitions: &[FieldDefinition],
) -> (TokenStream2, Vec<TokenStream2>) {
    if !cfg!(feature = "experimental_builder_syntax") {
        return (quote! {}, Vec::new());
    }

    let builder_struct_name =
        syn::parse_str::<Ident>(format!("Partial{}", struct_name).as_str()).unwrap();

    let mut running_mask = 0u128;
    let mut running_mask_token_tree = syn::parse_str::<TokenTree>("0x0").unwrap();
    let mut new_with_builder_chain: Vec<TokenStream2> =
        Vec::with_capacity(field_definitions.len() + 2);

    new_with_builder_chain.push(quote! {
       #struct_vis struct #builder_struct_name<const MASK: #internal_base_data_type>(#struct_name);
    });

    for field_definition in field_definitions {
        if let Some(setter_type) = field_definition.setter_type.as_ref() {
            let field_name = &field_definition.field_name;
            let setter_name = setter_name(field_name);

            let (field_mask, value_transform, argument_type) = if let Some(array) =
                field_definition.array
            {
                // For arrays, we'll generate this code:
                // self.0
                //   .with_a(0, value[0])
                //   .with_a(1, value[1])
                //   .with_a(2, value[2])

                let array_count = array.0;
                let mut mask = 0;
                let mut array_setters = Vec::with_capacity(array_count);
                for i in 0..array_count {
                    mask |= ((1u128 << field_definition.number_of_bits) - 1)
                        << (field_definition.lowest_bit + i * array.1);

                    array_setters.push(quote! { .#setter_name(#i, value[#i]) });
                }
                let value_transform = quote!(self.0 #( #array_setters )*);
                let array_type = quote! { [#setter_type; #array_count] };

                (mask, value_transform, array_type)
            } else {
                let mask = if field_definition.number_of_bits == 128 {
                    u128::MAX
                } else {
                    ((1u128 << field_definition.number_of_bits) - 1) << field_definition.lowest_bit
                };

                (
                    mask,
                    quote! { self.0.#setter_name(value)},
                    quote! { #setter_type },
                )
            };
            let previous_mask = running_mask;
            let previous_mask_token_tree = running_mask_token_tree;

            if (previous_mask & field_mask) != 0 {
                // Some fields are writable through multiple fields. This is not supported, so don't provide the constructor
                return (quote! {}, Vec::new());
            }

            running_mask = previous_mask | field_mask;
            running_mask_token_tree =
                syn::parse_str::<TokenTree>(format!("{:#x}", running_mask).as_str()).unwrap();
            new_with_builder_chain.push(quote! {
                impl #builder_struct_name<#previous_mask_token_tree> {
                    pub const fn #setter_name(&self, value: #argument_type) -> #builder_struct_name<#running_mask_token_tree> {
                        #builder_struct_name(#value_transform)
                    }
                }
            });
        }
    }

    // The type has to either be complete OR it has to have a default value. Otherwise we can't do constructor syntax
    if (running_mask.count_ones() as usize != base_data_size.exposed) && !has_default {
        return (quote! {}, Vec::new());
    }

    new_with_builder_chain.push(quote! {
        impl #builder_struct_name<#running_mask_token_tree> {
            pub const fn build(&self) -> #struct_name {
                self.0
            }
        }
    });

    let default = if has_default {
        quote! { #builder_struct_name(#struct_name::DEFAULT) }
    } else {
        if base_data_size.exposed == base_data_size.internal {
            quote! { #builder_struct_name(#struct_name::new_with_raw_value(0)) }
        } else {
            quote! {
                const ZERO: #base_data_type = #base_data_type::new(0);
                #builder_struct_name(#struct_name::new_with_raw_value(ZERO))
            }
        }
    };
    let result_new_with_constructor = quote! {
        /// Creates a builder for this bitfield which ensures that all writable fields are initialized
        pub const fn builder() -> #builder_struct_name<0> {
            #default
        }
    };
    (result_new_with_constructor, new_with_builder_chain)
}

struct FieldDefinition {
    field_name: Ident,
    lowest_bit: usize,
    number_of_bits: usize,
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

fn parse_field(base_data_size: usize, field: &Field) -> Result<FieldDefinition, TokenStream2> {
    let field_name = field.ident.as_ref().unwrap();

    let (ty, indexed_count) = {
        match &field.ty {
            Type::Array(ty) => {
                let length = (&ty.len).into_token_stream().to_string();

                (
                    ty.elem.deref(),
                    Some(
                        length
                            .parse::<usize>()
                            .unwrap_or_else(|_| panic!("{} is not a valid number", length)),
                    ),
                )
            }
            _ => (&field.ty, None),
        }
    };
    let (field_type_size_from_data_type, field_type_is_signed) = match ty {
        Type::Path(path) => {
            match path.to_token_stream().to_string().as_str() {
                "bool" => (Some(BITCOUNT_BOOL), false),
                "u8" => (Some(8), false),
                "i8" => (Some(8), true),
                "u16" => (Some(16), false),
                "i16" => (Some(16), true),
                "u32" => (Some(32), false),
                "i32" => (Some(32), true),
                "u64" => (Some(64), false),
                "i64" => (Some(64), true),
                "u128" => (Some(128), false),
                "i128" => (Some(128), true),
                s if parse_arbitrary_int_type(s).is_ok() => (Some(parse_arbitrary_int_type(s).unwrap()), false),
                _ => (None, false), // Enum type - size is the the number of bits
            }
        }
        _ => panic!("bitfield!: Field type {} not valid. bool, u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, arbitrary int (e.g. u1, u3, u62). Their arrays are also supported", ty.into_token_stream()),
    };

    let unsigned_field_type = if field_type_is_signed {
        Some(
            syn::parse_str::<Type>(
                format!("u{}", field_type_size_from_data_type.unwrap()).as_str(),
            )
            .unwrap_or_else(|_| panic!("bitfield!: Error parsing unsigned_field_type")),
        )
    } else {
        None
    };

    let mut range: Option<Range<usize>> = None;
    let mut provide_getter = false;
    let mut provide_setter = false;
    let mut indexed_stride: Option<usize> = None;

    let mut doc_comment: Vec<Attribute> = Vec::new();

    for attr in &field.attrs {
        let attr_name = &attr
            .path()
            .segments
            .first()
            .unwrap_or_else(|| panic!("bitfield!: Invalid path"))
            .ident;
        match attr_name.to_string().as_str() {
            start @ ("bits" | "bit") => {
                let is_range = start == "bits";
                let range_span = attr
                    .meta
                    .require_list()
                    .unwrap()
                    .tokens
                    .clone()
                    .into_iter()
                    .take_while(|t| &t.to_string() != ",")
                    .collect::<proc_macro2::TokenStream>();

                if range.is_some() {
                    return Err(syn::Error::new_spanned(
                        &attr,
                        "bitfield!: Only one 'bit' or 'bits' is supported per field",
                    )
                    .to_compile_error());
                }
                // Get the token_string, which is "bit(...) or bits()". Then get the arguments inside the parentheses.
                let token_string = attr.meta.to_token_stream().to_string();
                assert!(token_string.starts_with("bit"));
                let attr_token_string = if token_string.starts_with("bits") {
                    token_string.trim_start_matches("bits").trim()
                } else {
                    token_string.trim_start_matches("bit").trim()
                };

                if &attr_token_string[..1] != "(" {
                    return Err(syn::Error::new_spanned(
                        &attr.meta,
                        format!("bitfield!: Expected '(' after '{}'", start),
                    )
                    .to_compile_error());
                }
                if &attr_token_string[attr_token_string.len() - 1..] != ")" {
                    return Err(syn::Error::new_spanned(
                        &attr.meta,
                        format!("bitfield!: Expected ')' to close '{}'", start),
                    )
                    .to_compile_error());
                }
                let arguments_string = &attr_token_string[1..attr_token_string.len() - 1];
                let arguments: Vec<&str> = arguments_string.split(',').map(|s| s.trim()).collect();

                if arguments.len() < 2 {
                    if is_range {
                        return Err(syn::Error::new_spanned(
                            &range_span,
                            "bitfield!: Expected inclusive bit-range and read/write specifier, e.g. bits(1..=8, rw). Supported read/write specifiers: rw, w, r."
                        ).to_compile_error());
                    } else {
                        return Err(syn::Error::new_spanned(
                            &range_span,
                            "bitfield!: Expected bit index and read/write specifier, e.g. bit(5, rw). Supported read/write specifiers: rw, w, r."
                        ).to_compile_error());
                    }
                }

                // *** Parse first argument:
                //   inclusive range like "6..=10" if attr_name = "bits"
                //   single bit "6" if attr_name = "bit"
                if is_range {
                    let range_elements: Vec<&str> =
                        arguments[0].split("..").map(|s| s.trim()).collect();
                    if range_elements.len() != 2 {
                        return Err(syn::Error::new_spanned(
                            &range_span,
                            "bitfield!: Expected inclusive range, e.g. bits(1..=8, rw)'",
                        )
                        .to_compile_error());
                    }
                    let start = range_elements[0].parse::<usize>()
                        .map_err(|_| if range_elements[0] == "" {
                            syn::Error::new_spanned(
                                &range_span,
                                "bitfield!: Expected valid integer, but found an empty string"
                            ).to_compile_error()
                        } else {
                            syn::Error::new_spanned(
                                &range_span,
                                format!("bitfield!: Expected valid range, e.g. 1..=8 but '{}' is not a number", range_elements[0])
                            ).to_compile_error()
                        })?;
                    let rest = range_elements[1];
                    if &rest[0..1] != "=" {
                        return Err(syn::Error::new_spanned(
                            &range_span,
                            "bitfield!: Expected inclusive range, e.g. bits(1..=8, rw)'",
                        )
                        .to_compile_error());
                    }
                    let end_string = rest[1..].trim();

                    let end = end_string.parse::<usize>()
                        .map_err(|_| if end_string == "" {
                            syn::Error::new_spanned(
                                &range_span,
                                "bitfield!: Expected valid integer, but found an empty string"
                            ).to_compile_error()
                        } else {
                            syn::Error::new_spanned(
                                &range_span,
                                format!("bitfield!: Expected valid range, e.g. 1..=8 but '{}' is not a number", end_string)
                            ).to_compile_error()
                        })?;
                    if start > end {
                        return Err(syn::Error::new_spanned(
                            &range_span,
                            format!(
                                "bitfield!: In Range {}..={}, start is not <= end",
                                start, end
                            ),
                        )
                        .to_compile_error());
                    }
                    if start >= base_data_size {
                        return Err(syn::Error::new_spanned(
                            &range_span,
                            format!("bitfield!: In Range {}..={}, start is out of range, as the base type has {} bits", start, end, base_data_size)
                        ).to_compile_error());
                    }
                    if end >= base_data_size {
                        return Err(syn::Error::new_spanned(
                            &range_span,
                            format!("bitfield!: In Range {}..={}, end is out of range, as the base type has {} bits", start, end, base_data_size)
                        ).to_compile_error());
                    }
                    if start == end {
                        return Err(syn::Error::new_spanned(
                            &attr.meta.require_list().unwrap().tokens,
                            format!("bitfield!: In Range {start}..={end}, start is equal to end. Use the syntax 'bit({start})' instead", start = start, end = end)
                        ).to_compile_error());
                    }
                    range = Some(Range {
                        start,
                        end: end + 1,
                    });
                } else {
                    let bit_index_string = arguments[0];
                    let bit_index = bit_index_string.parse::<usize>()
                        .map_err(|e| if arguments[0] == "" {
                            syn::Error::new_spanned(
                                &attr.meta.require_list().unwrap().tokens,
                                "bitfield!: Expected valid integer, but found an empty string"
                            ).to_compile_error()
                        } else {
                            syn::Error::new_spanned(
                                &range_span,
                                format!("bitfield!: Expected valid bit index, e.g. bit(8) but '{}' is not a number: {}", bit_index_string, e)
                            ).to_compile_error()
                        })?;
                    if bit_index >= base_data_size {
                        return Err(syn::Error::new_spanned(
                            &range_span,
                            format!("bitfield!: Bit index {} is out of range", bit_index),
                        )
                        .to_compile_error());
                    }
                    range = Some(Range {
                        start: bit_index,
                        end: bit_index + 1,
                    });
                }

                // *** Parse second argument: we expect either "r", "w" or "rw"
                match arguments[1] {
                    "rw" => {
                        provide_getter = true;
                        provide_setter = true;
                    }
                    "r" => {
                        provide_getter = true;
                        provide_setter = false;
                    }
                    "w" => {
                        provide_getter = false;
                        provide_setter = true;
                    }
                    "" => return Err(syn::Error::new_spanned(
                        &attr.meta.require_list().unwrap().tokens,
                        format!("bitfield!: No read/write specifier. Expected 'r', 'w', or 'rw'")
                        ).to_compile_error()),
                    _ => return Err(
                        syn::Error::new_spanned(
                        attr.meta.require_list().unwrap().tokens
                            .clone()
                            .into_iter()
                            .skip_while(|t| &t.to_string() != ",")
                            .skip(1)
                            .collect::<proc_macro2::TokenStream>(),
                        format!("bitfield!: Unhandled read/write specifier {}. Expected 'r', 'w', or 'rw'", arguments[1])
                        ).to_compile_error())
                }

                // *** Parse additional named arguments (at the moment just stride: X)
                for argument in arguments.iter().skip(2) {
                    let argument_elements: Vec<&str> = argument
                        .split(|c| c == '=' || c == ':')
                        .map(|s| s.trim())
                        .collect();
                    if argument_elements.len() != 2 {
                        return Err(syn::Error::new_spanned(
                                &attr.meta,
                                format!("bitfield!: Named arguments have to be in the form of 'argument = value'. Seen: {:?}", argument_elements)
                            ).to_compile_error());
                    }
                    match argument_elements[0] {
                        "stride" => {
                            if indexed_count.is_none() {
                                return Err(syn::Error::new_spanned(
                                        &attr.meta,
                                        "bitfield!: stride is only supported for indexed properties. Use array type (e.g. [u8; 8]) to indicate"
                                    ).to_compile_error());
                            }
                            indexed_stride = match argument_elements[1].parse() {
                                Ok(x) => Some(x),
                                Err(_) => {
                                    return Err(syn::Error::new_spanned(
                                        &attr.meta,
                                        format!(
                                            "bitfield!: Stride {} is not a number",
                                            argument_elements[1]
                                        ),
                                    )
                                    .to_compile_error());
                                }
                            }
                        }
                        _ => {
                            return Err(syn::Error::new_spanned(
                                &attr.meta,
                                format!(
                                    "bitfield!: Unhandled named argument '{}'. Supported: 'stride'",
                                    argument_elements[0]
                                ),
                            )
                            .to_compile_error());
                        }
                    }
                }
            }
            "doc" => {
                // inline documentation. pass through to both getter and setter
                doc_comment.push(attr.clone());
            }
            _ => {
                return Err(syn::Error::new_spanned(
                        &attr_name,
                        format!("bitfield!: Unhandled attribute '{}'. Only supported attributes are 'bit' or 'bits'", attr_name)
                    ).to_compile_error());
            }
        }
    }

    let (lowest_bit, number_of_bits) = match range {
        Some(ref range) => (range.start, range.end - range.start),
        None => panic!("bitfield!: Expected valid range, e.g. bits(1..=8, rw) or bit(4, r)"),
    };
    let (field_type_size, primitive_type) = match field_type_size_from_data_type {
        None => (number_of_bits, {
            if number_of_bits <= 8 {
                quote! { u8 }
            } else if number_of_bits <= 16 {
                quote! { u16 }
            } else if number_of_bits <= 32 {
                quote! { u32 }
            } else if number_of_bits <= 64 {
                quote! { u64 }
            } else if number_of_bits <= 128 {
                quote! { u128 }
            } else {
                panic!("bitfield!: number_of_bits is too large!")
            }
        }),
        Some(b) => (b, quote! { #ty }),
    };

    if field_type_size == BITCOUNT_BOOL {
        if number_of_bits != 1 {
            return Err(syn::Error::new_spanned(
                &field.attrs.first(),
                format!("bitfield!: Field {} is a bool, so it should only use a single bit (use syntax 'bit({})' instead)", field_name, lowest_bit)
            ).to_compile_error());
        }
    } else {
        if number_of_bits != field_type_size {
            return Err(syn::Error::new_spanned(
                &field.ty,
                format!("bitfield!: Field {} has type {}, which doesn't match the number of bits ({}) that are being used for it", field_name, ty.to_token_stream(), number_of_bits)
            ).to_compile_error());
        }
    }

    // Verify bounds for arrays
    if let Some(indexed_count) = indexed_count {
        // If stride wasn't given, use the field width
        if indexed_stride.is_none() {
            indexed_stride = Some(number_of_bits)
        }

        if number_of_bits > indexed_stride.unwrap() {
            return Err(syn::Error::new_spanned(
                &field.attrs.first(),
                format!(
                    "bitfield!: Field {} is declared as {} bits, which is larger than the stride {}",
                    field_name,
                    number_of_bits,
                    indexed_stride.unwrap()
                ),
            )
            .to_compile_error());
        }

        let number_of_bits_indexed = (indexed_count - 1) * indexed_stride.unwrap()
            + number_of_bits
            + range.clone().unwrap().start;
        if number_of_bits_indexed > base_data_size {
            return Err(syn::Error::new_spanned(
                &field.attrs.first(),
                format!(
                    "bitfield!: Field {} requires more bits via indexing ({} + ({} - 1) * {} + {} = {}) than the bitfield has ({})", field_name, range.unwrap().start, indexed_count, indexed_stride.unwrap(), number_of_bits, number_of_bits_indexed, base_data_size
                ),
            ).to_compile_error());
        }

        if indexed_count < 2 {
            return Err(syn::Error::new_spanned(
                &field.ty,
                format!(
                    "bitfield!: Field {} is declared as array, but with fewer than 2 elements.",
                    field_name
                ),
            )
            .to_compile_error());
        }
    }

    let (custom_type, getter_type, setter_type) = if field_type_size_from_data_type.is_none() {
        // Test for optional type. We have to dissect the Option<T> type to do that
        let (inner_type, result_type) = if let Type::Path(type_path) = ty {
            if type_path.path.segments.len() != 1 {
                panic!("Invalid path segment. Expected Enumeration or Option<Enumeration>");
            }
            let option_segment = type_path.path.segments.first().unwrap();
            if option_segment.ident == "Option" {
                match &option_segment.arguments {
                    PathArguments::AngleBracketed(args) => {
                        if args.args.len() != 1 {
                            panic!("Invalid Option<T> path. Expected exactly one generic type argument");
                        }
                        let option_generic_type = args.args.first().unwrap();
                        match option_generic_type {
                            GenericArgument::Type(generic_type) => {
                                let result_type_string = format!(
                                    "Result<{}, {}>",
                                    generic_type.to_token_stream(),
                                    primitive_type.to_token_stream(),
                                );
                                let result_type = syn::parse_str::<Type>(&result_type_string)
                                    .expect("bitfield!: Error creating type from Result<,>");

                                (generic_type, result_type)
                            }
                            _ => panic!("Invalid Option binding: Expected generic type"),
                        }
                    }
                    _ => panic!("Expected < after Option"),
                }
            } else {
                (ty, ty.clone())
            }
        } else {
            (ty, ty.clone())
        };

        (
            CustomType::Yes(inner_type.clone()),
            result_type,
            inner_type.clone(),
        )
    } else {
        (CustomType::No, ty.clone(), ty.clone())
    };

    let use_regular_int = match field_type_size_from_data_type {
        Some(i) => is_int_size_regular_type(i),
        None => {
            // For CustomTypes (e.g. enums), prefer u1 over bool
            number_of_bits != 1 && is_int_size_regular_type(number_of_bits)
        }
    };

    Ok(FieldDefinition {
        field_name: field_name.clone(),
        lowest_bit,
        number_of_bits,
        field_type_size,
        getter_type: if provide_getter {
            Some(getter_type)
        } else {
            None
        },
        setter_type: if provide_setter {
            Some(setter_type)
        } else {
            None
        },
        use_regular_int,
        primitive_type,
        custom_type,
        doc_comment,
        array: indexed_count.map(|count| (count, indexed_stride.unwrap())),
        field_type_size_from_data_type,
        unsigned_field_type,
    })
}
