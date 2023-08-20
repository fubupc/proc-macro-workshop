use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data, DataStruct, DeriveInput,
    Error, Fields, FieldsNamed, GenericArgument, GenericParam, Lit, LitStr, Meta, MetaNameValue,
    PathArguments, Result, ReturnType, Type, TypeParam,
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
        if let GenericParam::Type(tp) = p {
            let need_debug = fields.iter().any(|f| appear_out_of_phantom(tp, &f.ty));
            if need_debug {
                tp.bounds.push(parse_quote!(::std::fmt::Debug));
            }
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

fn appear_out_of_phantom(tp: &TypeParam, ty: &Type) -> bool {
    match ty {
        Type::Array(a) => appear_out_of_phantom(tp, a.elem.as_ref()),
        Type::BareFn(_) => false,
        Type::Group(g) => appear_out_of_phantom(tp, &g.elem),
        Type::ImplTrait(_) => false,
        Type::Infer(_) => false,
        // FIXME: Macro in type position?
        Type::Macro(_) => todo!(),
        Type::Never(_) => false,
        Type::Paren(p) => appear_out_of_phantom(tp, &p.elem),
        Type::Path(p) => {
            let Some(last) = p.path.segments.last() else {
                return false;
            };
            if tp.ident == last.ident {
                return true;
            }
            if last.ident == "PhantomData" {
                return false;
            }
            match &last.arguments {
                PathArguments::AngleBracketed(args) => {
                    args.args.iter().any(|g| match g {
                        GenericArgument::Type(t) => appear_out_of_phantom(tp, t),
                        // FIXME: `T` appears in binding may not require `T: Debug`
                        GenericArgument::Binding(_) => todo!(),
                        _ => false,
                    })
                }
                PathArguments::None => return false,
                PathArguments::Parenthesized(args) => {
                    let out = match &args.output {
                        ReturnType::Default => false,
                        ReturnType::Type(_, o) => appear_out_of_phantom(tp, o),
                    };
                    out || args.inputs.iter().any(|i| appear_out_of_phantom(tp, i))
                }
            }
        }
        Type::Ptr(_) => false,
        Type::Reference(r) => appear_out_of_phantom(tp, &r.elem),
        Type::Slice(s) => appear_out_of_phantom(tp, &s.elem),
        Type::TraitObject(_) => false,
        Type::Tuple(tup) => tup.elems.iter().any(|e| appear_out_of_phantom(tp, e)),
        Type::Verbatim(_) => todo!(),
        _ => todo!(),
    }
}
