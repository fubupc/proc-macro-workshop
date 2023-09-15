use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
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

        let _tmp = f.ident.as_ref().unwrap();

        let consts = quote!(
            const FIELD_BIT_OFFSET: usize = #curr_offset;
            const FIELD_BITS: usize = <#ty as ::bitfield::Specifier>::BITS;
            const START_BYTE_INDEX: usize = FIELD_BIT_OFFSET / 8;
            const START_BIT_OFFSET: usize = FIELD_BIT_OFFSET % 8;
            const END_BYTE_INDEX: usize = (FIELD_BIT_OFFSET + FIELD_BITS) / 8;
            const END_BIT_OFFSET: usize = (FIELD_BIT_OFFSET + FIELD_BITS) % 8;
        );

        let getter_name = format_ident!("get_{}", f.ident.as_ref().unwrap());
        getters.push(quote!(
            #vis fn #getter_name(&self) -> u64 {
                #consts

                if START_BYTE_INDEX == END_BYTE_INDEX {
                    return ((self.data[START_BYTE_INDEX] & (!0 >> START_BIT_OFFSET)) >> (8 - END_BIT_OFFSET)) as u64
                }

                let mut r: u64 = (self.data[START_BYTE_INDEX] & (!0 >> START_BIT_OFFSET)) as u64;
                for i in START_BYTE_INDEX+1..END_BYTE_INDEX {
                    r = (r << 8) | self.data[i] as u64
                }
                if END_BIT_OFFSET > 0 {
                    r = (r << END_BIT_OFFSET) | (self.data[END_BYTE_INDEX] >> (8 - END_BIT_OFFSET)) as u64;
                }
                r
            }
        ));

        let setter_name = format_ident!("set_{}", f.ident.as_ref().unwrap());
        setters.push(quote!(
            #vis fn #setter_name(&mut self, v: u64) {
                #consts

                // Safe guard: check input value range
                if v & (!0 << FIELD_BITS) != 0 {
                    panic!("{}: input value is out of bits range", stringify!(#setter_name));
                }

                if START_BYTE_INDEX == END_BYTE_INDEX {
                    let mask: u8 = !(!0 >> START_BIT_OFFSET) | (!0 >> END_BIT_OFFSET);
                    self.data[START_BYTE_INDEX] = (self.data[START_BYTE_INDEX] & mask) | (v << (8 - END_BIT_OFFSET)) as u8;
                    return;
                }

                self.data[START_BYTE_INDEX] = (self.data[START_BYTE_INDEX] & !(!0 >> START_BIT_OFFSET)) | (v >> (FIELD_BITS + START_BIT_OFFSET - 8)) as u8;
                for i in START_BYTE_INDEX+1..END_BYTE_INDEX {
                    self.data[i] = (v >> (FIELD_BITS + START_BIT_OFFSET - 8 - 8*(i-START_BYTE_INDEX))) as u8;
                }
                if END_BIT_OFFSET > 0 {
                    self.data[END_BYTE_INDEX] = (self.data[END_BYTE_INDEX] & (!0 >> END_BIT_OFFSET)) | (v << (8 - END_BIT_OFFSET)) as u8;
                }
            }
        ));

        curr_offset = quote!(#curr_offset + <#ty as ::bitfield::Specifier>::BITS);
    }

    (getters, setters)
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
