use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::Result;

pub(crate) fn generate() -> Result<TokenStream2> {
    (1_usize..=64)
        .map(|n| {
            let name = format_ident!("B{}", n);
            let back_type = min_uint_type(n).unwrap();
            Ok(quote!(
                pub enum #name {}

                impl crate::Specifier for #name {
                    const BITS: usize = #n;
                    type Get = #back_type;
                    type Set = #back_type;

                    fn from_u64(v: u64) -> Self::Get {
                        v as Self::Get
                    }

                    fn into_u64(v: Self::Set) -> u64 {
                        v as u64
                    }
                }
            ))
        })
        .collect()
}

fn min_uint_type(bits: usize) -> Option<TokenStream2> {
    match bits {
        1..=8 => Some(quote!(u8)),
        9..=16 => Some(quote!(u16)),
        17..=32 => Some(quote!(u32)),
        33..=64 => Some(quote!(u64)),
        _ => None,
    }
}
