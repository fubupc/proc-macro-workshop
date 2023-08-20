use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data, DataStruct, DeriveInput,
    Error, Fields, FieldsNamed, GenericParam, Lit, LitStr, Meta, MetaNameValue, Result,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    impl_derive(&ast)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn impl_derive(ast: &DeriveInput) -> Result<proc_macro2::TokenStream> {
    let Data::Struct(DataStruct {
        fields: Fields::Named(FieldsNamed { named: fields, .. }),
        ..
    }) = &ast.data
    else {
        return Err(Error::new(ast.span(), "expected struct with named fields"));
    };
    let struct_ident = &ast.ident;

    let field_setters = fields
        .iter()
        .map(|f| {
            let ident = f.ident.as_ref().expect("bug: named field without ident");
            let value = match parse_debug_attr(&f.attrs)? {
                Some(fmt) => quote!(&::std::format_args!(#fmt, &self.#ident)),
                None => quote!(&self.#ident),
            };
            Ok(quote!( .field(stringify!(#ident), #value) ))
        })
        .collect::<Result<Vec<_>>>()?;

    let mut generics = ast.generics.clone();
    for p in &mut generics.params {
        if let GenericParam::Type(ty) = p {
            ty.bounds.push(parse_quote!(::std::fmt::Debug));
        }
    }
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    Ok(quote!(
        impl #impl_generics ::std::fmt::Debug for #struct_ident #ty_generics #where_clause {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.debug_struct(stringify!(#struct_ident))
                 #(#field_setters)*
                 .finish()
            }
        }
    ))
}

fn parse_debug_attr(attrs: &[Attribute]) -> Result<Option<LitStr>> {
    let debugs = attrs
        .iter()
        .filter(|a| a.path.is_ident("debug"))
        .collect::<Vec<_>>();

    match debugs.as_slice() {
        [] => Ok(None),
        [d] => match d.parse_meta()? {
            Meta::NameValue(MetaNameValue { lit, .. }) => match lit {
                Lit::Str(fmt) => Ok(Some(fmt)),
                _ => Err(Error::new(lit.span(), "expected literal string")),
            },
            bad => Err(Error::new(bad.span(), r#"expected `#[debug = "..."]`"#)),
        },
        [_, dup, ..] => Err(Error::new(dup.span(), "duplicated `#[debug]`")),
    }
}
