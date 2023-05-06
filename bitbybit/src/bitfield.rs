use proc_macro::TokenStream;
use std::ops::{Deref, Range};
use std::str::FromStr;

use proc_macro2::TokenTree;
use quote::{quote, ToTokens};
use syn::__private::TokenStream2;
use syn::{Attribute, Data, DeriveInput, GenericArgument, PathArguments, Type};

/// In the code below, bools are considered to have 0 bits. This lets us distinguish them
/// from u1
const BITCOUNT_BOOL: usize = 0;

/// Returns true if the number can be expressed by a regular data type like u8 or u32.
/// 0 is special as it means bool (technically should be 1, but we use that for u1)
fn is_int_size_regular_type(size: usize) -> bool {
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
            None => panic!("bitfield!: Seen {}, but didn't expect anything. Example of valid syntax: #[bitfield(u32, default: 0)]", token_stream),
            Some(ArgumentType::Default) => {
                *default_value = Some(token_stream);
            }
        }
    }
    for arg in args.iter().skip(1) {
        match arg {
            TokenTree::Punct(p) => match p.to_string().as_str() {
                "," => next_expected = None,
                ":" => {}
                _ => panic!(
                    "bitfield!: Expected ',' or ':' in argument list. Seen '{}'",
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
            _ => panic!("bitfield!: Unexpected token. Example of valid syntax: #[bitfield(u32, default: 0)]"),
        }
    }

    let base_data_size = match base_data_type.to_string().as_str() {
        "u8" => 8,
        "u16" => 16,
        "u32" => 32,
        "u64" => 64,
        "u128" => 128,
        _ => panic!("bitfield!: Supported values for base data type are u8, u16, u32, u64, u128. {} is invalid", base_data_type.to_string().as_str())
    };

    let input = syn::parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let struct_vis = input.vis;
    let struct_attrs = input.attrs;

    let fields = match input.data {
        Data::Struct(struct_data) => struct_data.fields,
        _ => panic!("bitfield!: Must be used on struct"),
    };
    let accessors: Vec<TokenStream2> = fields.iter().map(|field| {
        let field_name = field.ident.as_ref().unwrap();
        let (ty, indexed_count) = {
            match &field.ty {
                Type::Array(ty) => {
                    let length = (&ty.len).into_token_stream().to_string();

                    (ty.elem.deref(), Some(length.parse::<usize>().unwrap_or_else(|_| panic!("{} is not a valid number", length))))
                }
                _ => (&field.ty, None)
            }
        };
        let field_type_size_from_data_type = match ty {
            Type::Path(path) => {
                match path.to_token_stream().to_string().as_str() {
                    "bool" => Some(BITCOUNT_BOOL),
                    "u8" | "i8" => Some(8),
                    "u16" | "i16" => Some(16),
                    "u32" | "i32" => Some(32),
                    "u64" | "i64" => Some(64),
                    "u128" | "i128" => Some(128),
                    s if parse_arbitrary_int_type(s).is_ok() => Some(parse_arbitrary_int_type(s).unwrap()),
                    _ => None, // Enum type - size is the the number of bits
                }
            }
            _ => panic!("bitfield!: Field type {} not valid. bool, u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, arbitrary int (e.g. u1, u3, u62). Their arrays are also supported", ty.into_token_stream()),
        };
        let mut range: Option<Range<usize>> = None;
        let mut provide_getter = false;
        let mut provide_setter = false;
        let mut indexed_stride: Option<usize> = None;

        let mut doc_comment: Option<&Attribute> = None;

        for attr in &field.attrs {
            let attr_name = attr.path().segments.first().unwrap_or_else(|| panic!("bitfield!: Invalid path")).ident.to_string();
            match attr_name.as_str() {
                "bits" | "bit" => {
                    let is_range = attr_name.as_str() == "bits";

                    if range.is_some() {
                        panic!("bitfield!: Only one 'bit' or 'bits' is supported per field");
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
                        panic!("bitfield!: Expected '(' after '{}'", attr_name.as_str());
                    }
                    if &attr_token_string[attr_token_string.len() - 1..] != ")" {
                        panic!("bitfield!: Expected ')' to close '{}'", attr_name.as_str());
                    }
                    let arguments_string = &attr_token_string[1..attr_token_string.len() - 1];
                    let arguments: Vec<&str> = arguments_string.split(',').map(|s| s.trim()).collect();

                    if arguments.len() < 2 {
                        if is_range {
                            panic!("bitfield!: Expected inclusive bit-range and read/write specifier, e.g. bits(1..=8, rw). Supported read/write specifiers: rw, w, r.")
                        } else {
                            panic!("bitfield!: Expected bit index and read/write specifier, e.g. bit(5, rw). Supported read/write specifiers: rw, w, r.")
                        }
                    }

                    // *** Parse first argument:
                    //   inclusive range like "6..=10" if attr_name = "bits"
                    //   single bit "6" if attr_name = "bit"
                    if is_range {
                        let range_elements: Vec<&str> = arguments[0].split("..").map(|s| s.trim()).collect();
                        if range_elements.len() != 2 {
                            panic!("bitfield!: Expected valid range, e.g. bits(1..=8, rw)");
                        }
                        let start = range_elements[0].parse::<usize>().unwrap_or_else(|x| panic!("bitfield!: Expected valid range, e.g. 1..=8 but '{}' is not a number", x));
                        let end_string = range_elements[1];
                        if &end_string[0..1] != "=" {
                            panic!("bitfield!: Expected inclusive range, e.g. bits(1..=8, rw)'");
                        }

                        let end = end_string[1..].trim().parse::<usize>().unwrap_or_else(|x| panic!("bitfield!: Expected valid range, e.g. 1..=8 but '{}' is not a number", x));
                        if start > end {
                            panic!("bitfield!: In Range {}..={}, start is not <= end", start, end);
                        }
                        if start >= base_data_size {
                            panic!("bitfield!: In Range {}..={}, start is out of range, as the base type has {} bits", start, end, base_data_size);
                        }
                        if end >= base_data_size {
                            panic!("bitfield!: In Range {}..={}, end is out of range, as the base type has {} bits", start, end, base_data_size);
                        }
                        if start == end {
                            panic!("bitfield!: In Range {start}..={end}, start is equal to end. Use the syntax 'bit({start})' instead", start = start, end = end)
                        }
                        range = Some(Range { start, end: end + 1 });
                    } else {
                        let bit_index_string = arguments[0];
                        let bit_index = bit_index_string.parse::<usize>()
                            .unwrap_or_else(|x| panic!("bitfield!: Expected valid bit index, e.g. bit(8) but '{}' is not a number: {}", bit_index_string, x));
                        if bit_index >= base_data_size {
                            panic!("bitfield!: Bit index {} is out of range", bit_index);
                        }
                        range = Some(Range { start: bit_index, end: bit_index + 1 });
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
                        _ => panic!("bitfield!: Unhandled read/write specifier {}. Expected 'r', 'w', or 'rw'", arguments[1])
                    }

                    // *** Parse additional named arguments (at the moment just stride: X)
                    for argument in arguments.iter().skip(2) {
                        let argument_elements: Vec<&str> = argument.split(':').map(|s| s.trim()).collect();
                        if argument_elements.len() != 2 {
                            panic!("bitfield!: Named arguments have to be in the form of 'argument: value'. Seen: {:?}", argument_elements)
                        }
                        match argument_elements[0] {
                            "stride" => {
                                if indexed_count.is_none() {
                                    panic!("bitfield!: stride is only supported for indexed properties. Use array type (e.g. [u8; 8]) to indicate");
                                }
                                indexed_stride = Some(argument_elements[1].parse().unwrap_or_else(|_| panic!("bitfield!: {} is not a number", argument_elements[1])))
                            }
                            _ => panic!("bitfield!: Unhandled named argument '{}'. Supported: 'stride'", argument_elements[0])
                        }
                    }
                }
                "doc" => {
                    // inline documentation. pass through to both getter and setter
                    doc_comment = Some(attr);
                }
                _ => panic!("bitfield!: Unhandled attribute '{}'. Only supported attributes are 'bit' or 'bits'", attr_name),
            }
        };

        let (lowest_bit, number_of_bits) = match range {
            Some(ref range) => (range.start, range.end - range.start),
            None => panic!("bitfield!: Expected valid range, e.g. bits(1..=8, rw) or bit(4, r)")
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
                    panic!("bitfield!: number_of_bits is to large!")
                }
            }),
            Some(b) => (b, quote! { #ty }),
        };

        if field_type_size == BITCOUNT_BOOL {
            if number_of_bits != 1 {
                panic!("bitfield!: Field {} is a bool, so it should only use a single bit (use syntax 'bit({})' instead)", field_name, lowest_bit);
            }
        } else {
            if number_of_bits != field_type_size {
                panic!("bitfield!: Field {} has type {}, which doesn't match the number of bits ({}) that are being used for it", field_name, ty.to_token_stream(), number_of_bits);
            }
        }

        // Verify bounds for arrays
        if let Some(indexed_count) = indexed_count {
            // If stride wasn't given, use the field width
            if indexed_stride.is_none() {
                indexed_stride = Some(number_of_bits)
            }

            if number_of_bits > indexed_stride.unwrap() {
                panic!("bitfield!: Field {} is declared as {} bits, which is larger than its stride {}", field_name, number_of_bits, indexed_stride.unwrap());
            }

            let number_of_bits_indexed = (indexed_count - 1) * indexed_stride.unwrap() + range.unwrap().start;
            if number_of_bits_indexed >= base_data_size {
                panic!("bitfield!: Field {} requires more bits via indexing ({}) than the bitfield has ({})", field_name, number_of_bits_indexed, base_data_size);
            }

            if indexed_count < 2 {
                panic!("bitfield!: Field {} is declared as indexing, but with fewer than 2 elements.", field_name);
            }
        }
        let one = syn::parse_str::<syn::LitInt>(format!("1u{}", base_data_size).as_str()).unwrap_or_else(|_| panic!("bitfield!: Error parsing one literal"));

        // If a convert_type is given, that will be the final getter/setter type. If not, it is the base type
        enum CustomType {
            No,
            Yes(Type),
        }
        let (custom_type, getter_type, setter_type) = if field_type_size_from_data_type.is_none() {
            // Test for optional type. We have to disect the Option<T> type to do that
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
                                    let result_type_string = format!("Result<{}, {}>", generic_type.to_token_stream(), primitive_type.to_token_stream());
                                    let result_type = syn::parse_str::<syn::Type>(&result_type_string).expect("bitfield!: Error creating type from Result<,>");

                                    (generic_type, result_type)
                                }
                                _ => panic!("Invalid Option binding: Expected generic type")
                            }
                        }
                        _ => panic!("Expected < after Option")
                    }
                } else {
                    (ty, ty.clone())
                }
            } else {
                (ty, ty.clone())
            };

            (CustomType::Yes(inner_type.clone()), result_type, inner_type.clone())
        } else {
            (CustomType::No, ty.clone(), ty.clone())
        };

        let use_regular_int = match field_type_size_from_data_type {
            Some(i) => is_int_size_regular_type(i),
            None => {
                // For CustomTypes (e.g. enums), prefer u1 over bool
                number_of_bits != 1 && is_int_size_regular_type(number_of_bits)
            },
        };

        let getter =
            if provide_getter {
                let extracted_bits = if use_regular_int {
                    // Extract standard type (u8, u16 etc)
                    if indexed_count.is_some() {
                        let indexed_stride = indexed_stride.unwrap();
                        if field_type_size == BITCOUNT_BOOL {
                            quote! { (self.raw_value & (#one << (#lowest_bit + index * #indexed_stride))) != 0 }
                        } else {
                            quote! { ((self.raw_value >> (#lowest_bit + index * #indexed_stride)) & ((#one << #number_of_bits) - #one)) as #primitive_type }
                        }
                    } else if field_type_size == BITCOUNT_BOOL {
                        quote! { (self.raw_value & (#one << #lowest_bit)) != 0 }
                    } else if number_of_bits == base_data_size {
                        // If the field is the whole size of the bitfield, we can't apply a mask
                        // as that would overflow. However, we don't need to
                        assert_eq!(lowest_bit, 0);
                        quote! { self.raw_value as #primitive_type }
                    } else {
                        quote! {
                            ((self.raw_value >> #lowest_bit) & ((#one << #number_of_bits) - #one)) as #primitive_type
                        }
                    }
                } else {
                    // Extract arbitrary int (e.g. u7), using one of the extract methods
                    let custom_type = TokenStream2::from_str(format!("arbitrary_int::u{}", number_of_bits).as_str()).unwrap();
                    let extract = TokenStream2::from_str(format!("extract_u{}", base_data_size).as_str()).unwrap();
                    if indexed_count.is_some() {
                        let indexed_stride = indexed_stride.unwrap();
                        quote! {
                            #custom_type::#extract(self.raw_value, #lowest_bit + index * #indexed_stride)
                        }
                    } else {
                        quote! {
                            #custom_type::#extract(self.raw_value, #lowest_bit)
                        }
                    }
                };

                let converted = match &custom_type {
                    CustomType::No => extracted_bits,
                    CustomType::Yes(convert_type) => {
                        quote! {
                            let extracted_bits = #extracted_bits;
                            #convert_type::new_with_raw_value(extracted_bits)
                        }
                    }
                };

                if indexed_count.is_some() {
                    quote! {
                        #doc_comment
                        #[inline]
                        pub const fn #field_name(&self, index: usize) -> #getter_type {
                            #converted
                        }
                    }
                } else {
                    quote! {
                        #doc_comment
                        #[inline]
                        pub const fn #field_name(&self) -> #getter_type {
                            #converted
                        }
                    }
                }
            } else {
                quote! {}
            };

        let setter = if provide_setter {
            let argument_converted =
                match custom_type {
                    CustomType::No => {
                        if use_regular_int {
                            quote! { field_value }
                        } else {
                            quote! { field_value.value() }
                        }
                    }
                    CustomType::Yes(_) => {
                        if use_regular_int {
                            quote! { field_value.raw_value() }
                        } else {
                            quote! { field_value.raw_value().value() }
                        }
                    }
                };

            let new_raw_value = if let Some(_indexed_count) = indexed_count {
                let indexed_stride = indexed_stride.unwrap();
                // bool?
                if field_type_size_from_data_type == Some(BITCOUNT_BOOL) {
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
                            (self.raw_value & !(((#one << #number_of_bits) - #one) << effective_index)) | ((#argument_converted as #base_data_type) << effective_index)
                        }
                    }
                }
            } else if field_type_size_from_data_type == Some(BITCOUNT_BOOL) {
                quote! {
                    if #argument_converted { self.raw_value | (#one << #lowest_bit) } else { self.raw_value & !(#one << #lowest_bit) }
                }
            } else if number_of_bits == base_data_size {
                // If the field is the whole size of the bitfield, we can't apply a mask
                // as that would overflow. However, we don't need to
                assert_eq!(lowest_bit, 0);
                quote! { #argument_converted as #base_data_type }
            } else {
                quote! { (self.raw_value & !(((#one << #number_of_bits) - #one) << #lowest_bit)) | ((#argument_converted as #base_data_type) << #lowest_bit) }
            };

            let setter_name = syn::parse_str::<syn::Ident>(format!("with_{}", field_name).as_str()).unwrap_or_else(|_| panic!("bitfield!: Error creating setter name"));
            if let Some(_indexed_count) = indexed_count {
                quote! {
                    #doc_comment
                    #[inline]
                    pub const fn #setter_name(&self, index: usize, field_value: #setter_type) -> Self {
                        Self {
                            raw_value: #new_raw_value
                        }
                    }
                }
            } else {
                quote! {
                    #doc_comment
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

    let (default_constructor, default_trait) = if let Some(default_value) = default_value {
        (
            quote! {
                /// Creates a new instance with the default value.
                ///
                /// For example, the following definition would create an instance with a
                /// raw_value of 0x123:
                /// #[bitfield(u32, default: 0x123)]
                #[inline]
                pub const fn new() -> #struct_name { #struct_name { raw_value: #default_value } }
            },
            quote! {
                impl Default for #struct_name {
                    fn default() -> Self {
                        Self::new()
                    }
                }
            },
        )
    } else {
        (quote! {}, quote! {})
    };

    let expanded = quote! {
        #[derive(Copy, Clone)]
        #[repr(C)]
        #( #struct_attrs )*
        #struct_vis struct #struct_name {
            raw_value: #base_data_type,
        }

        impl #struct_name {
            #default_constructor
            /// Returns the underlying raw value of this bitfield
            #[inline]
            pub const fn raw_value(&self) -> #base_data_type { self.raw_value }

            /// Creates a new instance of this bitfield with the given raw value.
            ///
            /// No checks are performed on the value, so it is possible to set bits that don't have any
            /// accessors specified.
            #[inline]
            pub const fn new_with_raw_value(value: #base_data_type) -> #struct_name { #struct_name { raw_value: value } }
            #( #accessors )*
        }
        #default_trait
    };
    // println!("Expanded: {}", expanded.to_string());
    TokenStream::from(expanded)
}
