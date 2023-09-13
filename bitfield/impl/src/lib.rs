use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, LitInt, Result,
};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let _ = input;

    unimplemented!()
}

#[proc_macro]
pub fn gen_b_types(input: TokenStream) -> TokenStream {
    let gen = parse_macro_input!(input as GenBTypes);

    impl_gen_b_types(gen)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

struct GenBTypes {
    max: usize,
}
impl Parse for GenBTypes {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(GenBTypes {
            max: input.parse::<LitInt>()?.base10_parse()?,
        })
    }
}

fn impl_gen_b_types(gen: GenBTypes) -> Result<TokenStream2> {
    (1..=gen.max)
        .map(|n| {
            let name = format_ident!("B{}", n);
            Ok(quote!(
                pub enum #name {}

                impl crate::Specifier for #name {
                    const BITS: usize = #n;
                }
            ))
        })
        .collect()
}
