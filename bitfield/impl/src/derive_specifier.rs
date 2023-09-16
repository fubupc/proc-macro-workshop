use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{spanned::Spanned, Attribute, Error, Expr, ExprLit, Item, ItemEnum, Lit, Meta, Result};

pub(crate) fn generate(item: Item) -> Result<TokenStream2> {
    let Item::Enum(e) = item else {
        return Err(Error::new(item.span(), "expected struct by #[bitfied]"));
    };

    let name = &e.ident;
    let bits_attr = parse_bits_attr(&e.attrs)?;
    let bits = check_enum_variants_num(&e, bits_attr)?;

    let match_arms = e
        .variants
        .iter()
        .map(|v| match &v.discriminant {
            Some((_, expr)) => {
                let v_ident = &v.ident;
                Ok(quote!( #expr => #name::#v_ident))
            }
            None => Err(Error::new(
                v.span(),
                "#derive[BitfieldSpecifier]: expected explicit discriminant",
            )),
        })
        .collect::<Result<Vec<_>>>()?;

    let (get_type, from_fn) = if bits_attr.is_some() {
        (
            quote!( ::std::result::Result<#name> ),
            quote!(
                fn from_u64(v: u64) -> Self::Get {
                    let e = match v {
                        #(#match_arms,)*
                        _ => return ::std::result::Result::Err(::bitfield::Unrecognized{raw: v}),
                    };
                    ::std::result::Result::Ok(e)
                }
            ),
        )
    } else {
        (
            quote!( #name ),
            quote!(
                fn from_u64(v: u64) -> Self::Get {
                    match v {
                        #(#match_arms,)*
                        _ => ::std::panic!("#[bitfield] BitfieldSpecifier derive macro bug")
                    }
                }
            ),
        )
    };
    let set_type = quote!( #name );

    Ok(quote!(
        impl ::bitfield::Specifier for #name {
            const BITS: usize = #bits;
            type Get = #get_type;
            type Set = #set_type;

            #from_fn

            fn into_u64(v: Self::Set) -> u64 {
                v as u64
            }
        }
    ))
}

fn parse_bits_attr(attrs: &[Attribute]) -> Result<Option<(usize, &Attribute)>> {
    for a in attrs {
        if a.path().is_ident("bits") {
            if let Meta::NameValue(kv) = &a.meta {
                if let Expr::Lit(ExprLit {
                    lit: Lit::Int(lit), ..
                }) = &kv.value
                {
                    return Ok(Some((lit.base10_parse()?, a)));
                }
            };

            return Err(Error::new(a.span(), "expected #[bits = ...]"));
        }
    }
    Ok(None)
}

fn check_enum_variants_num(e: &ItemEnum, bits_attr: Option<(usize, &Attribute)>) -> Result<usize> {
    match bits_attr {
        Some((bits, attr)) => {
            if bits > 64 {
                return Err(Error::new(attr.span(), "only support max bit size of 64"));
            }
            let max: usize = !0 >> (64 - bits);
            if e.variants.len() > max {
                return Err(Error::new(
                    attr.span(),
                    format!("enum has too many variants to fit in {} bits", bits),
                ));
            }
            Ok(bits)
        }
        None => {
            if !e.variants.len().is_power_of_two() {
                return Err(Error::new(
                    Span::call_site(),
                    "expected enum to have a power-of-two number of variants, or use #[bits = N]",
                ));
            }
            Ok(e.variants.len().ilog2() as usize)
        }
    }
}
