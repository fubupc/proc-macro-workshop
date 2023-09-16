use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, spanned::Spanned, Error, Fields, FieldsNamed, Item, ItemEnum, ItemStruct,
    Result,
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
    if strukt.generics.params.len() > 0 {
        return Err(Error::new(
            strukt.span(),
            "struct with generic not supported by #[bitfield]",
        ));
    }
    let Fields::Named(fields) = &strukt.fields else {
        return Err(Error::new(
            strukt.span(),
            "tuple struct not supported by #[bitfield]",
        ));
    };

    let name = &strukt.ident;
    let attrs = &strukt.attrs;
    let vis = &strukt.vis;

    let total_bits = calc_total_bits(fields);
    let (getters, setters) = gen_accessors(&fields);

    Ok(quote!(

        #(#attrs)*
        #vis struct #name {
            data: [u8; #total_bits / 8]
        }

        impl #name {
            fn new() -> #name {
                const _: () = if (#total_bits % 8 != 0) {
                    panic!("expected total bit size of struct is multiple of 8");
                };

                #name{data: [0u8; #total_bits / 8]}
            }

            #(#getters)*

            #(#setters)*
        }
    ))
}

fn calc_total_bits(fields: &FieldsNamed) -> TokenStream2 {
    let bit_infos = fields.named.iter().map(|f| {
        let ty = &f.ty;
        quote!(<#ty as ::bitfield::Specifier>::BITS)
    });
    quote!( (#(#bit_infos)+*) )
}

fn gen_accessors(fields: &FieldsNamed) -> (Vec<TokenStream2>, Vec<TokenStream2>) {
    let mut getters: Vec<TokenStream2> = Vec::new();
    let mut setters: Vec<TokenStream2> = Vec::new();
    let mut curr_offset = quote!(0);

    for f in &fields.named {
        let ty = &f.ty;
        let vis = &f.vis;
        let as_specifier = quote!(<#ty as ::bitfield::Specifier>);

        let getter_name = format_ident!("get_{}", f.ident.as_ref().unwrap());
        getters.push(quote!(
            #vis fn #getter_name(&self) -> #as_specifier::BackType {
                <#ty as ::bitfield::Specifier>::from_u64(::bitfield::read_bits(&self.data, #curr_offset, #as_specifier::BITS))
            }
        ));

        let setter_name = format_ident!("set_{}", f.ident.as_ref().unwrap());
        setters.push(quote!(
            #vis fn #setter_name(&mut self, v: #as_specifier::BackType) {
               ::bitfield::write_bits(&mut self.data, #curr_offset, #as_specifier::BITS, #as_specifier::into_u64(v))
            }
        ));

        curr_offset = quote!(#curr_offset + #as_specifier::BITS);
    }

    (getters, setters)
}

#[proc_macro]
pub fn gen_b_types(_: TokenStream) -> TokenStream {
    impl_gen_b_types()
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn impl_gen_b_types() -> Result<TokenStream2> {
    (1_usize..=64)
        .map(|n| {
            let name = format_ident!("B{}", n);
            let back_type = match n {
                1..=8 => quote!(u8),
                9..=16 => quote!(u16),
                17..=32 => quote!(u32),
                33..=64 => quote!(u64),
                _ => unreachable!(),
            };
            Ok(quote!(
                pub enum #name {}

                impl crate::Specifier for #name {
                    const BITS: usize = #n;
                    type BackType = #back_type;

                    fn from_u64(v: u64) -> #back_type {
                        v as #back_type
                    }

                    fn into_u64(v: #back_type) -> u64 {
                        v as u64
                    }
                }
            ))
        })
        .collect()
}
