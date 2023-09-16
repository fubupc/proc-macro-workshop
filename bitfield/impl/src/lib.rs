use proc_macro::TokenStream;
use syn::{parse_macro_input, Item};

mod bitfield;
mod derive_specifier;
mod gen_b_types;

#[proc_macro_attribute]
pub fn bitfield(args: TokenStream, input: TokenStream) -> TokenStream {
    let _ = args;
    let item = parse_macro_input!(input as Item);

    bitfield::generate(item)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

#[proc_macro]
pub fn gen_b_types(_: TokenStream) -> TokenStream {
    gen_b_types::generate()
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

#[proc_macro_derive(BitfieldSpecifier, attributes(bits))]
pub fn derive_specifier(input: TokenStream) -> TokenStream {
    let item = parse_macro_input!(input as Item);

    derive_specifier::generate(item)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}
