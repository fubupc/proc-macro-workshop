use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote,
    spanned::Spanned,
    Error, Fields, FieldsNamed, Item, ItemEnum, ItemStruct, LitInt, Result,
};

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item = parse_macro_input!(input as Item);

    impl_bitfield(item)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn impl_bitfield(item: Item) -> Result<TokenStream2> {
    match item {
        Item::Struct(strukt) => bitfield_struct(strukt),
        Item::Enum(en) => bitfield_enum(en),
        _ => Err(Error::new(item.span(), "expected struct by #[bitfied]")),
    }
}

fn bitfield_enum(en: ItemEnum) -> Result<TokenStream2> {
    Ok(en.to_token_stream())
}

fn bitfield_struct(strukt: ItemStruct) -> Result<TokenStream2> {
    let Fields::Named(fields) = &strukt.fields else {
        return Err(Error::new(
            strukt.span(),
            "tuple struct not supported by #[bitfield]",
        ));
    };
    let field_types: Vec<_> = fields.named.iter().map(|f| &f.ty).collect();
    let bits_const = format_ident!("__bitfield_bits_of_{}__", &strukt.ident);

    let storage: FieldsNamed = parse_quote!({ data: [u8; #bits_const / 8]});
    let mut replaced = strukt.clone();
    replaced.fields = Fields::Named(storage);

    Ok(quote!(
        #replaced

        #[allow(non_upper_case_globals)]
        const #bits_const: usize = #(<#field_types as ::bitfield::Specifier>::BITS)+*;
    ))
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
