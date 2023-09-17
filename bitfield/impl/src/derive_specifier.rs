use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{spanned::Spanned, Attribute, Error, Expr, ExprLit, Item, ItemEnum, Lit, Meta, Result};

pub(crate) fn generate(item: Item) -> Result<TokenStream2> {
    let Item::Enum(e) = item else {
        return Err(Error::new(item.span(), "expected struct by #[bitfied]"));
    };

    let enum_name = &e.ident;
    let bits_attr = parse_bits_attr(&e.attrs)?;
    let bits = check_enum_variants_num(&e, bits_attr)?;
    let var_idents: Vec<_> = e.variants.iter().map(|v| &v.ident).collect();
    let discriminant_checks = gen_discriminant_checks(enum_name, &var_idents, bits);
    let get_type = gen_get_type(enum_name, bits_attr.is_some());
    let set_type = quote!( #enum_name );
    let from_fn = gen_from_fn(enum_name, &var_idents, bits_attr.is_some());

    Ok(quote!(
        #(#discriminant_checks)*

        impl ::bitfield::Specifier for #enum_name {
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

fn gen_get_type(enum_name: &Ident, exhaustive: bool) -> TokenStream2 {
    if exhaustive {
        quote!( ::std::result::Result<#enum_name, ::bitfield::Unrecognized> )
    } else {
        quote!( #enum_name )
    }
}

fn gen_from_fn(enum_name: &Ident, var_idents: &[&Ident], exhaustive: bool) -> TokenStream2 {
    if exhaustive {
        quote!(
            fn from_u64(v: u64) -> Self::Get {
                match v {
                    #(x if x == #enum_name::#var_idents as u64 => ::std::result::Result::Ok(#enum_name::#var_idents),)*
                    _ => ::std::result::Result::Err(::bitfield::Unrecognized{raw: v}),
                }
            }
        )
    } else {
        quote!(
            fn from_u64(v: u64) -> Self::Get {
                match v {
                    #(x if x == #enum_name::#var_idents as u64 => #enum_name::#var_idents,)*
                    _ => ::std::panic!("#[bitfield] BitfieldSpecifier derive macro bug")
                }
            }
        )
    }
}

fn gen_discriminant_checks(
    enum_name: &Ident,
    var_idents: &[&Ident],
    bits: usize,
) -> Vec<TokenStream2> {
    let max_discriminant: u64 = !0 >> (64 - bits);
    var_idents
        .iter()
        .map(|v| {
            let msg = format!(
                "enum discriminant `{}::{}` too large to fit int {} bits",
                enum_name, v, bits
            );
            quote!(
                const _:() = if (#enum_name::#v as u64) > #max_discriminant {
                    ::std::panic!(#msg)
                };
            )
        })
        .collect()
}
