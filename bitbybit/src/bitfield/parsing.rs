use crate::bitfield::{
    is_int_size_regular_type, parse_arbitrary_int_type, BaseDataSize, CustomType, FieldDefinition,
    BITCOUNT_BOOL,
};
use proc_macro2::{Ident, Literal, Punct, TokenStream as TokenStream2, TokenTree};
use quote::{quote, ToTokens};
use std::ops::{Deref, Range};
use syn::{
    parse2, Attribute, Error, ExprArray, Field, Fields, GenericArgument, MetaList, PathArguments,
    Type,
};

pub fn parse(
    fields: &Fields,
    base_data_size: BaseDataSize,
) -> Result<Vec<FieldDefinition>, TokenStream2> {
    let mut field_definitions = Vec::with_capacity(fields.len());

    for field in fields {
        match parse_field(base_data_size.internal, &field) {
            Ok(def) => field_definitions.push(def),
            Err(ts) => return Err(ts),
        }
    }

    Ok(field_definitions)
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

    let mut ranges: Vec<Range<usize>> = Vec::new();
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

                let mut finished_argument = |range_parser: ArgumentParser,
                                             is_in_array: bool|
                 -> Result<(), Error> {
                    match range_parser {
                        ArgumentParser::RangeGotBothLimits(lower, upper) => {
                            if !ranges.is_empty() && !is_in_array {
                                return Err(Error::new_spanned(
                                    &range_span,
                                    "bitfield!: Seen multiple bit-ranges, but only one is allowed",
                                ));
                            }
                            if !is_range && !is_in_array {
                                return Err(Error::new_spanned(
                                    &range_span,
                                    "bitfield!: bit requires an inclusive range, for examples bits(10..=19). bit(10) allows specifying a single bit",
                                ));
                            }
                            ranges.push(Range {
                                start: lower,
                                end: upper + 1,
                            });
                        }
                        ArgumentParser::RangeGotLowerLimit(lower) => {
                            if !ranges.is_empty() && !is_in_array {
                                return Err(Error::new_spanned(
                                    &range_span,
                                    "bitfield!: Seen multiple bit-ranges, but only one is allowed",
                                ));
                            }
                            if is_range && !is_in_array {
                                return Err(Error::new_spanned(
                                    &range_span,
                                    "bitfield!: bits requires a single bit, for examples bit(10). bits(10..=12) can be used to specify multiple bits",
                                ));
                            }
                            ranges.push(Range {
                                start: lower,
                                end: lower + 1,
                            });
                        }
                        ArgumentParser::ReadWrite => {
                            provide_getter = true;
                            provide_setter = true;
                        }
                        ArgumentParser::Read => {
                            provide_getter = true;
                        }
                        ArgumentParser::Write => {
                            provide_setter = true;
                        }
                        ArgumentParser::StrideComplete(stride) => {
                            if indexed_count.is_none() {
                                return Err(Error::new_spanned(
                                    &attr.meta,
                                    "bitfield!: stride is only supported for indexed properties. Use array type (e.g. [u8; 8]) to indicate"
                                ));
                            }
                            indexed_stride = Some(stride);
                        }
                        ArgumentParser::Reset => {
                            // An empty argument (happens after arrays as that's a separate ArgumentParser)
                        }
                        _ =>
                            Err(Error::new_spanned(
                                &range_span,
                                "bitfield!: Invalid syntax. Supported: bits(5..=6, access, stride = x), where x is an integer and access can be r, w or rw",
                            ))?,

                    };
                    Ok(())
                };

                ArgumentParser::parse_argument_tokens(
                    parse2::<MetaList>(attr.meta.to_token_stream())
                        .unwrap()
                        .tokens,
                    false,
                    &mut finished_argument,
                )
                .map_err(|e| e.to_compile_error())?;

                let token_string = attr.meta.to_token_stream().to_string();
                assert!(token_string.starts_with("bit"));
                let attr_token_string = if token_string.starts_with("bits") {
                    token_string.trim_start_matches("bits").trim()
                } else {
                    token_string.trim_start_matches("bit").trim()
                };

                if &attr_token_string[..1] != "(" {
                    return Err(Error::new_spanned(
                        &attr.meta,
                        format!("bitfield!: Expected '(' after '{}'", start),
                    )
                    .to_compile_error());
                }
                if &attr_token_string[attr_token_string.len() - 1..] != ")" {
                    return Err(Error::new_spanned(
                        &attr.meta,
                        format!("bitfield!: Expected ')' to close '{}'", start),
                    )
                    .to_compile_error());
                }
            }
            "doc" => {
                // inline documentation. pass through to both getter and setter
                doc_comment.push(attr.clone());
            }
            _ => {
                return Err(Error::new_spanned(
                    &attr_name,
                    format!("bitfield!: Unhandled attribute '{}'. Only supported attributes are 'bit' or 'bits'", attr_name)
                ).to_compile_error());
            }
        }
    }

    // We know that ranges has at least one value
    // TODO: Verify all uses of this - some are still good, others not so much
    let number_of_bits = ranges.iter().fold(0, |a, b| a + b.end - b.start);

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
        if number_of_bits != 1 || ranges.len() != 1 {
            return Err(Error::new_spanned(
                &field.attrs.first(),
                format!("bitfield!: Field {} is a bool, so it should only use a single bit (use syntax 'bit({})' instead)", field_name, ranges[0].start)
            ).to_compile_error());
        }
    } else {
        if number_of_bits != field_type_size {
            return Err(Error::new_spanned(
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
            return Err(Error::new_spanned(
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

        let highest_bit_index_in_ranges = ranges.iter().map(|range| range.end).max().unwrap_or(0);
        let number_of_bits_indexed =
            (indexed_count - 1) * indexed_stride.unwrap() + highest_bit_index_in_ranges;
        if number_of_bits_indexed > base_data_size {
            return Err(Error::new_spanned(
                &field.attrs.first(),
                format!(
                    "bitfield!: Array-field {} requires {number_of_bits_indexed} bits for the array, but only has ({})", field_name, base_data_size
                ),
            ).to_compile_error());
        }

        if indexed_count < 2 {
            return Err(Error::new_spanned(
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
        ranges,
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

/// Parses the arguments of a field. At the beginning and after each comma, Reset is used. After
/// that, the various take_xxx functions are used to switch states.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum ArgumentParser {
    // An unknown argument - could go into any of the categories below
    Reset,

    // For range-arrays: The range hasn't started (yet), but we know it's only going to be an array)
    ResetOnlyRangeAllowed,

    // Ranges
    RangeGotLowerLimit(usize),
    RangeGotFirstPeriod(usize),
    RangeGotSecondPeriod(usize),
    RangeGotEquals(usize),
    RangeGotBothLimits(usize, usize),

    // Stride
    StrideStarted,
    HasStrideEquals,
    StrideComplete(usize),

    // Read/Write
    Read,
    Write,
    ReadWrite,
}

impl ArgumentParser {
    fn parse_literal_number(number: Literal) -> Result<usize, Error> {
        number
            .to_string()
            .parse()
            .map_err(|_| Error::new_spanned(&number, "bitfield!: Not a valid number in bitrange."))
    }

    pub fn take_literal(&self, lit: Literal) -> Result<ArgumentParser, Error> {
        match self {
            ArgumentParser::Reset | ArgumentParser::ResetOnlyRangeAllowed => Ok(
                ArgumentParser::RangeGotLowerLimit(Self::parse_literal_number(lit)?),
            ),
            ArgumentParser::RangeGotEquals(lower) => Ok(ArgumentParser::RangeGotBothLimits(
                *lower,
                Self::parse_literal_number(lit)?,
            )),
            ArgumentParser::HasStrideEquals => Ok(ArgumentParser::StrideComplete(
                Self::parse_literal_number(lit)?,
            )),
            _ => Err(Error::new_spanned(
                &lit,
                "bitfield!: Invalid bit-range. Expected x..=y, for example 6..=10.",
            )),
        }
    }

    fn take_punct(&self, punct: Punct) -> Result<ArgumentParser, Error> {
        match self {
            ArgumentParser::RangeGotLowerLimit(lower) if punct.as_char() == '.' => {
                Ok(ArgumentParser::RangeGotFirstPeriod(*lower))
            }
            ArgumentParser::RangeGotFirstPeriod(lower) if punct.as_char() == '.' => {
                Ok(ArgumentParser::RangeGotSecondPeriod(*lower))
            }
            ArgumentParser::RangeGotSecondPeriod(lower) if punct.as_char() == '=' => {
                Ok(ArgumentParser::RangeGotEquals(*lower))
            }
            ArgumentParser::StrideStarted if punct.as_char() == '=' || punct.as_char() == ':' => {
                Ok(ArgumentParser::HasStrideEquals)
            }
            _ => Err(Error::new_spanned(
                &punct,
                "bitfield!: Invalid bit-range. Expected x..=y, for example 6..=10.",
            )),
        }
    }

    fn take_ident(&self, id: Ident) -> Result<ArgumentParser, Error> {
        let s = id.span().source_text().unwrap_or("".to_string());
        match self {
            ArgumentParser::Reset if s == "rw" => Ok(ArgumentParser::ReadWrite),
            ArgumentParser::Reset if s == "r" => Ok(ArgumentParser::Read),
            ArgumentParser::Reset if s == "w" => Ok(ArgumentParser::Write),
            ArgumentParser::Reset if s == "stride" => Ok(ArgumentParser::StrideStarted),
            _ => Err(Error::new_spanned(
                &id,
                "bitfield!: Invalid ident. Expected r, rw, w or stride",
            )),
        }
    }

    fn parse_argument_tokens<F: FnMut(ArgumentParser, bool) -> Result<(), Error>>(
        token_stream: TokenStream2,
        is_in_array: bool,
        finished_argument: &mut F,
    ) -> Result<(), Error> {
        let reset = if is_in_array {
            Self::ResetOnlyRangeAllowed
        } else {
            Self::Reset
        };
        let mut argument_parser = reset;
        for meta in token_stream {
            match meta {
                TokenTree::Group(group) => {
                    let range_array = parse2::<ExprArray>(group.to_token_stream()).unwrap();
                    for range in range_array.elems {
                        Self::parse_argument_tokens(
                            range.to_token_stream(),
                            true,
                            finished_argument,
                        )?;
                    }
                }
                TokenTree::Ident(id) => {
                    argument_parser = argument_parser.take_ident(id)?;
                }
                TokenTree::Punct(punct) => match punct.as_char() {
                    ',' => {
                        finished_argument(argument_parser, is_in_array)?;
                        argument_parser = reset;
                    }
                    _ => {
                        argument_parser = argument_parser.take_punct(punct)?;
                    }
                },
                TokenTree::Literal(lit) => {
                    argument_parser = argument_parser.take_literal(lit)?;
                }
            }
        }
        finished_argument(argument_parser, is_in_array)?;

        Ok(())
    }
}
