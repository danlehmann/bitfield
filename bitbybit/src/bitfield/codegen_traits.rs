use crate::bitfield::{BitfieldAttributes, FieldDefinition};
use proc_macro2::{Ident, TokenStream, TokenTree};
use quote::quote;

pub fn generate(
    struct_name: &Ident,
    bitfield_attrs: &BitfieldAttributes,
    field_definitions: &[FieldDefinition],
) -> impl Iterator<Item = TokenStream> {
    let mask = syn::parse_str::<TokenTree>(
        format!(
            "{:#x}",
            super::used_bits_mask_for_struct(&field_definitions)
        )
        .as_str(),
    )
    .unwrap();

    let debug_trait = generate_debug_trait(struct_name, bitfield_attrs, field_definitions);
    let eq_trait = generate_eq_trait(struct_name, bitfield_attrs);
    let partial_eq_trait = generate_partial_eq_trait(struct_name, bitfield_attrs, &mask);
    let ord_trait = generate_ord_trait(struct_name, bitfield_attrs, field_definitions);
    let partial_ord_trait =
        generate_partial_ord_trait(struct_name, bitfield_attrs, field_definitions);
    let hash_trait = generate_hash_trait(struct_name, bitfield_attrs, mask);

    debug_trait
        .into_iter()
        .chain(eq_trait.into_iter())
        .chain(partial_eq_trait.into_iter())
        .chain(ord_trait.into_iter())
        .chain(partial_ord_trait.into_iter())
        .chain(hash_trait.into_iter())
}

fn generate_debug_trait(
    struct_name: &Ident,
    bitfield_attrs: &BitfieldAttributes,
    field_definitions: &[FieldDefinition],
) -> Option<TokenStream> {
    if bitfield_attrs.debug_trait {
        let debug_fields: Vec<TokenStream> = field_definitions
            .iter()
            .map(|field| {
                let field_name = &field.field_name;
                quote! {
                    .field(stringify!(#field_name), &self.#field_name())
                }
            })
            .collect();
        Some(quote! {
            impl ::core::fmt::Debug for #struct_name {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    f.debug_struct(stringify!(#struct_name))
                        #(#debug_fields)*
                        .finish()
                }
            }
        })
    } else {
        None
    }
}

fn generate_eq_trait(
    struct_name: &Ident,
    bitfield_attrs: &BitfieldAttributes,
) -> Option<TokenStream> {
    if bitfield_attrs.eq_trait {
        Some(quote! {
            impl ::core::cmp::Eq for #struct_name {}
        })
    } else {
        None
    }
}

fn generate_partial_eq_trait(
    struct_name: &Ident,
    bitfield_attrs: &BitfieldAttributes,
    mask: &TokenTree,
) -> Option<TokenStream> {
    if bitfield_attrs.partial_eq_trait {
        Some(quote! {
            impl ::core::cmp::PartialEq for #struct_name {
                #[inline]
                fn eq(&self, other: &Self) -> bool {
                    self.raw_value & #mask == other.raw_value & #mask
                }
            }
        })
    } else {
        None
    }
}

fn generate_ord_trait(
    struct_name: &Ident,
    bitfield_attrs: &BitfieldAttributes,
    field_definitions: &[FieldDefinition],
) -> Option<TokenStream> {
    if bitfield_attrs.ord_trait {
        let ord_fields: Vec<TokenStream> = field_definitions
            .iter()
            .map(|field| {
                let field_name = &field.field_name;
                quote! {
                    match self.#field_name().cmp(&other.#field_name()) {
                        core::cmp::Ordering::Equal => {},
                        cmp => return cmp,
                    }
                }
            })
            .collect();
        Some(quote! {
            impl ::core::cmp::Ord for #struct_name {
                fn cmp(&self, other: &Self) -> ::core::cmp::Ordering {
                    #(#ord_fields)*
                    ::core::cmp::Ordering::Equal
                }
            }
        })
    } else {
        None
    }
}

fn generate_partial_ord_trait(
    struct_name: &Ident,
    bitfield_attrs: &BitfieldAttributes,
    field_definitions: &[FieldDefinition],
) -> Option<TokenStream> {
    if bitfield_attrs.partial_ord_trait {
        let partial_ord_fields: Vec<TokenStream> = field_definitions
            .iter()
            .map(|field| {
                let field_name = &field.field_name;
                quote! {
                    match self.#field_name().partial_cmp(&other.#field_name()) {
                        ::core::option::Option::Some(core::cmp::Ordering::Equal) => {},
                        ::core::option::Option::Some(cmp) => return ::core::option::Option::Some(cmp),
                        ::core::option::Option::None => return ::core::option::Option::None,
                    }
                }
            })
            .collect();
        Some(quote! {
            impl ::core::cmp::PartialOrd for #struct_name {
                fn partial_cmp(&self, other: &Self) -> ::core::option::Option<::core::cmp::Ordering> {
                    #(#partial_ord_fields)*
                    ::core::option::Option::Some(::core::cmp::Ordering::Equal)
                }
            }
        })
    } else {
        None
    }
}

fn generate_hash_trait(
    struct_name: &Ident,
    bitfield_attrs: &BitfieldAttributes,
    mask: TokenTree,
) -> Option<TokenStream> {
    if bitfield_attrs.hash_trait {
        Some(quote! {
            impl ::core::hash::Hash for #struct_name {
                #[inline]
                fn hash<H: ::core::hash::Hasher>(&self, state: &mut H) {
                    (self.raw_value & #mask).hash(state)
                }
            }
        })
    } else {
        None
    }
}
