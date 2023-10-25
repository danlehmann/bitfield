use proc_macro2::{Span, TokenStream};
use quote::{quote, spanned::Spanned, ToTokens};
use syn::{meta::ParseNestedMeta, Token};

use crate::bit_size::Bits;

const BAD_EXHAUSTIVE: &'static str = "The specified exhaustiveness is invalid, supported values are 'true' 'false' and 'conditional'";
const WRONG_EXHAUSTIVE: &'static str = "The specified 'exhaustive' is invalid";
const MISSING_EXHAUSTIVE: &'static str = "Missing the 'exhaustive = …' argument to bitenum";
const MISSING_SIZE: &'static str = "Missing the 'u[0-9]' argument to bitenum";
const MISSING_DISCR: &'static str =
    "ALL variants of a #[bitenum] MUST have an explicit literal discriminant eg: 'Variant = 1'.";
const UNEXPECTED_ATTRIBUTE_META: &'static str = "Invalid bitenum attribute, expected either `uN` where N in range 1..=64, or `exhaustive = (true|false|conditional)`";
const CFG_NON_CONDITIONAL: &'static str =
    "The specified exhaustiveness is invalid, the enum contains at least one \
    conditional variant, therefore, exhaustiveness should be specified as 'conditional'";
const NONLIT_DISCR: &'static str =
    "Discriminants must be literals integers, either binary, hexadecimal, octal or decimal";
const TOO_MANY_VARIANTS: &'static str =
    "The enum has more variants than can be stored within the provided storage type, consider \
    using a larger storage type or reducing the number of variants.";
const DISCR_TOO_LARGE: &'static str =
    "The largest discriminant value is larger than can be stored in the provided storage type.";

#[derive(Clone, Copy)]
enum Exhaustiveness {
    True,
    False,
    Conditional,
}
struct Exhaustive {
    span: Span,
    kind: Exhaustiveness,
}

impl Exhaustive {
    fn is_conditional(&self) -> bool {
        matches!(self.kind, Exhaustiveness::Conditional)
    }
    fn matches(&self, expected: bool) -> bool {
        use Exhaustiveness::{False, True};
        !matches!((self.kind, expected), (True, false) | (False, true))
    }
}
impl syn::parse::Parse for Exhaustive {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let span = input.span();
        let kind = match (input.parse::<syn::LitBool>(), input.parse::<syn::Ident>()) {
            (Ok(bool), _) if bool.value => Exhaustiveness::True,
            (Ok(_), _) => Exhaustiveness::False,
            (_, Ok(ident)) if ident == "conditional" => Exhaustiveness::Conditional,
            _ => return Err(syn::Error::new(span, BAD_EXHAUSTIVE)),
        };
        Ok(Exhaustive { span, kind })
    }
}

#[derive(Default)]
pub(crate) struct Config {
    explicit_bits: Option<Bits>,
    explicit_exhaustive: Option<Exhaustive>,
}
struct FullConfig {
    bits: Bits,
    exhaustive: Exhaustive,
}
impl Config {
    pub(crate) fn parse(&mut self, meta: ParseNestedMeta) -> syn::Result<()> {
        let unexpected_attr = || Err(meta.error(UNEXPECTED_ATTRIBUTE_META));
        match () {
            () if meta.path.is_ident("exhaustive") => {
                let result1 = meta.input.parse::<Token![:]>();
                let result2 = meta.input.parse::<Token![=]>();
                if result1.is_err() && result2.is_err() {
                    return Err(meta.error("Expected either ':' or '=' following 'expected'"));
                }
                self.explicit_exhaustive = Some(meta.input.parse()?);
            }
            () => {
                let Some(last_segment) = meta.path.segments.last() else {
                    return unexpected_attr();
                };
                let value = last_segment.ident.to_string();
                let ("u", size) = value.split_at(1) else {
                    return unexpected_attr();
                };
                let Ok(size) = size.parse() else {
                    return unexpected_attr();
                };
                let path = meta.path.clone();
                self.explicit_bits = Some(Bits { path, size });
            }
        }
        Ok(())
    }

    fn explicit(self) -> syn::Result<FullConfig> {
        let span = Span::call_site();
        let Some(bits) = self.explicit_bits else {
            return Err(syn::Error::new(span, MISSING_SIZE));
        };
        let Some(exhaustive) = self.explicit_exhaustive else {
            return Err(syn::Error::new(span, MISSING_EXHAUSTIVE));
        };
        Ok(FullConfig { bits, exhaustive })
    }
}

fn conditional_attr(attr: &syn::Attribute) -> bool {
    attr.path().is_ident("cfg")
}
fn check_explicit_conditional(config: &FullConfig, input: &syn::ItemEnum) -> syn::Result<()> {
    let conditional_variant = |v: &syn::Variant| v.attrs.iter().any(conditional_attr);
    let is_conditional = input.variants.iter().any(conditional_variant);

    if is_conditional && !config.exhaustive.is_conditional() {
        Err(syn::Error::new(config.exhaustive.span, CFG_NON_CONDITIONAL))
    } else {
        Ok(())
    }
}
// Determine the integer value itself. While we don't need it further down (for now),
// this ensures that only constants are being used; due to the way how new_with_raw_value()
// is written, some expressions would cause compilation issues (e.g. those that refer to other
// enum values).
fn parse_expr(expr: &syn::Expr) -> Option<u128> {
    let string_value = expr.to_token_stream().to_string().replace('_', "");

    let int_value = if let Some(stripped) = string_value.strip_prefix("0x") {
        u128::from_str_radix(stripped, 16)
    } else if let Some(stripped) = string_value.strip_prefix("0b") {
        u128::from_str_radix(stripped, 2)
    } else if let Some(stripped) = string_value.strip_prefix("0o") {
        u128::from_str_radix(stripped, 8)
    } else {
        string_value.parse::<u128>()
    };
    int_value.ok()
}
fn check_explicit_exhaustive(config: &FullConfig, input: &syn::ItemEnum) -> syn::Result<()> {
    let max_count = 1_u128 << config.bits.size;
    let count = input.variants.len() as u128;
    let actually_exhaustive = match count.cmp(&max_count) {
        std::cmp::Ordering::Equal => true,
        std::cmp::Ordering::Greater if !config.exhaustive.is_conditional() => {
            let msg = format!("{TOO_MANY_VARIANTS}, has up to {max_count} variants, got {count}");
            return Err(syn::Error::new_spanned(&config.bits.path, msg));
        }
        _ => false,
    };
    if !config.exhaustive.matches(actually_exhaustive) {
        let span = config.exhaustive.span;
        let msg = format!("{WRONG_EXHAUSTIVE}, expected {actually_exhaustive} got the reverse.");
        return Err(syn::Error::new(span, msg));
    }
    let (mut max_discr, mut max_discr_span) = (0, Span::call_site());
    for variant in &input.variants {
        let Some((_, discriminant)) = variant.discriminant.as_ref() else {
            return Err(syn::Error::new_spanned(&variant.ident, MISSING_DISCR));
        };
        let Some(value) = parse_expr(discriminant) else {
            return Err(syn::Error::new_spanned(discriminant, NONLIT_DISCR));
        };
        if value > max_discr {
            max_discr = value;
            max_discr_span = discriminant.__span();
        }
    }
    if max_discr >= max_count {
        let max_value = max_count - 1;
        let msg = format!("{DISCR_TOO_LARGE}, max value is {max_value}");
        return Err(syn::Error::new(max_discr_span, msg));
    }
    Ok(())
}

pub(crate) fn bitenum(config: Config, input: syn::ItemEnum) -> syn::Result<TokenStream> {
    let config = config.explicit()?;
    check_explicit_conditional(&config, &input)?;
    check_explicit_exhaustive(&config, &input)?;

    let bits = config.bits;
    let (base_type, qualified_type) = (bits.base_type()?, bits.qualified_path()?);
    let (constructor, reader) = (bits.constructor()?, bits.reader());

    let non_exhaustive = config.exhaustive.matches(false);
    let ok = non_exhaustive.then_some(quote!(Ok));
    let new_return_type = match non_exhaustive {
        true => quote!(std::result::Result<Self, #base_type>),
        false => quote!(Self),
    };
    let new_default_branch = match non_exhaustive {
        true => quote!(value => Err(value)),
        false => quote!(_ => unreachable!()),
    };
    let new_match_branches = input.variants.iter().map(|variant| {
        let cfg_attrs = variant.attrs.iter().filter(|a| conditional_attr(a));
        let value = &variant.discriminant.as_ref().unwrap().1;
        let variant_name = &variant.ident;
        quote!( #( #cfg_attrs )* (#value) => #ok(Self::#variant_name) )
    });
    let (attrs, vis, name, variants) = (&input.attrs, &input.vis, &input.ident, &input.variants);
    Ok(quote! {
        #[derive(Copy, Clone)]
        #( #attrs )*
        #vis enum #name {
            #variants
        }

        impl #name {
            /// Returns the underlying raw value of this bitfield.
            pub const fn raw_value(self) -> #qualified_type {
                #constructor(self as #base_type)
            }

            /// Creates a new instance of this bitfield with the given raw value.
            pub const fn new_with_raw_value(value: #qualified_type) -> #new_return_type {
                match value #reader {
                    #( #new_match_branches ,)*
                    #new_default_branch
                }
            }
        }
    })
}
