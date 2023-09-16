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
    let var_idents: Vec<_> = e.variants.iter().map(|v| &v.ident).collect();
    let (get_type, from_fn) = if bits_attr.is_some() {
        (
            quote!( ::std::result::Result<#name, ::bitfield::Unrecognized> ),
            quote!(
                fn from_u64(v: u64) -> Self::Get {
                    match v {
                        #(x if x == #name::#var_idents as u64 => ::std::result::Result::Ok(#name::#var_idents),)*
                        _ => ::std::result::Result::Err(::bitfield::Unrecognized{raw: v}),
                    }
                }
            ),
        )
    } else {
        (
            quote!( #name ),
            quote!(
                fn from_u64(v: u64) -> Self::Get {
                    match v {
                        #(x if x == #name::#var_idents as u64 => #name::#var_idents,)*
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
                    attr.meta.span(),
                    format!("enum has too many variants to fit in {} bits", bits),
                ));
            }
            Ok(bits)
        }
        None => {
            if !e.variants.len().is_power_of_two() {
                return Err(Error::new(
                    Span::call_site(),
                    "BitfieldSpecifier expected a number of variants which is a power of 2",
                ));
            }
            Ok(e.variants.len().ilog2() as usize)
        }
    }
}
