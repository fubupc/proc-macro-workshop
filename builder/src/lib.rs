use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Attribute, Data::Struct,
    DeriveInput, Field, Ident, Lit, MetaNameValue, Path, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn builder_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    impl_builder_derive(&input)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn impl_builder_derive(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let strukt = match &input.data {
        Struct(s) => s,
        _ => return Err(syn::Error::new(input.span(), "expected struct type")),
    };

    let struct_ident = &input.ident;
    let builder_ident = format_ident!("{}Builder", struct_ident);

    let field_classes = strukt
        .fields
        .iter()
        .map(|f| match &f.ident {
            Some(ident) => Ok((ident, parse_field_class(f)?)),
            None => Err(syn::Error::new(
                f.ident.span(),
                "unnamed field not supported",
            )),
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let field_defs = field_classes.iter().map(|(ident, class)| match class {
        FieldClass::Nominal(ty) | FieldClass::Optional(ty) => {
            quote!( #ident: ::std::option::Option<#ty> )
        }
        FieldClass::Repeated(_, elem_type) => quote!( #ident: ::std::vec::Vec<#elem_type> ),
    });
    let field_inits = field_classes.iter().map(|(ident, class)| match class {
        FieldClass::Nominal(_) | FieldClass::Optional(_) => {
            quote!(#ident: ::std::option::Option::None)
        }
        FieldClass::Repeated(_, _) => quote!(#ident: ::std::vec::Vec::new()),
    });
    let field_setters = field_classes.iter().map(|(ident, class)| match class {
        FieldClass::Nominal(ty) | FieldClass::Optional(ty) => quote!(
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = ::std::option::Option::Some(#ident);
                self
            }
        ),
        FieldClass::Repeated(name, elem_type) => quote!(
            fn #name(&mut self, #name: #elem_type) -> &mut Self {
                self.#ident.push(#name);
                self
            }
        ),
    });
    let build_assigns = field_classes.iter().map(|(ident, class)| match class {
        FieldClass::Nominal(_) => {
            let msg = format!("uninitialized required field {}", ident);
            quote!(#ident: self.#ident.take().ok_or(::std::boxed::Box::<dyn ::std::error::Error>::from(::std::string::String::from(#msg)))?)
        }
        FieldClass::Optional(_) => quote!(#ident: self.#ident.take()),
        FieldClass::Repeated(_, _) => {
            quote!(#ident: std::mem::replace(&mut self.#ident, Vec::new()))
        }
    });

    Ok(quote!(
        pub struct #builder_ident {
            #(#field_defs),*
        }
        impl #builder_ident {
            fn build(&mut self) -> ::std::result::Result<#struct_ident, ::std::boxed::Box<dyn ::std::error::Error>> {
                ::std::result::Result::Ok(#struct_ident {
                    #(#build_assigns),*
                })
            }

            #(#field_setters)*
        }

        impl #struct_ident {
            fn builder() -> #builder_ident {
                #builder_ident {
                    #(#field_inits),*
                }
            }
        }

    ))
}

enum FieldClass<'a> {
    Nominal(&'a Type),         // (field type)
    Optional(&'a Type),        // (inner type)
    Repeated(Ident, &'a Type), // (setter ident, elem type)
}

fn parse_field_class<'a>(f: &'a Field) -> syn::Result<FieldClass<'a>> {
    match parse_builder_attr(&f.attrs)? {
        Some(name) => match parse_vec(&f.ty)? {
            Some(elem) => Ok(FieldClass::Repeated(name, elem)),
            None => Err(syn::Error::new(
                f.ty.span(),
                "expected `Vec<T>` type for field with `#[builder]`",
            )),
        },
        None => match parse_optional(&f.ty)? {
            Some(inner) => Ok(FieldClass::Optional(inner)),
            None => Ok(FieldClass::Nominal(&f.ty)),
        },
    }
}

// Parse `ty` parameter to check whether it's `Option<T>`, returns its inner type `T` when true.
fn parse_optional(ty: &Type) -> syn::Result<Option<&Type>> {
    parse_inner(ty, "Option")
}

// Parse `ty` parameter to check whether it's `Vec<T>`, returns its inner type `T` when true.
fn parse_vec(ty: &Type) -> syn::Result<Option<&Type>> {
    parse_inner(ty, "Vec")
}

fn parse_builder_attr<'a>(attrs: &'a [Attribute]) -> syn::Result<Option<Ident>> {
    let builders: Vec<_> = attrs
        .iter()
        .filter(|a| a.path.is_ident("builder"))
        .collect();
    match builders.as_slice() {
        [] => Ok(None),
        [builder] => {
            let kv: MetaNameValue = builder.parse_args().map_err(|_| {
                syn::Error::new(builder.span(), "expected `builder(each = \"...\")`")
            })?;
            if !kv.path.is_ident("each") {
                return Err(syn::Error::new(
                    kv.span(),
                    "expected `each` in `#[builder]` argument path",
                ));
            }
            match kv.lit {
                Lit::Str(name) => Ok(Some(Ident::new(&name.value(), name.span()))),
                _ => Err(syn::Error::new(kv.lit.span(), "expected literal string")),
            }
            // // Expect Vec<T>
            // match parse_vec(ty)? {
            //     Some(elem) => Ok(Some((Ident::new(&name.value(), name.span()), elem))),
            //     None => Err(syn::Error::new(
            //         ty.span(),
            //         "#[builder] can only apply to Vec<T>",
            //     )),
            // }
        }
        [_, dup, ..] => Err(syn::Error::new(
            dup.span(),
            "duplicated `#[builder]` attribute",
        )),
    }
}

// Check if `ty` is `#ident<T>`, returns its inner type `T` when true.
fn parse_inner<'a>(ty: &'a Type, ident: &str) -> syn::Result<Option<&'a Type>> {
    match ty {
        Type::Path(TypePath {
            qself: None,
            path:
                Path {
                    leading_colon: None,
                    segments,
                },
        }) => {
            let first = segments
                .first()
                .ok_or(syn::Error::new(ty.span(), "empty type path"))?;
            if first.ident != ident {
                return Ok(None);
            }
            match &first.arguments {
                PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. })
                    if args.len() == 1 =>
                {
                    match &args[0] {
                        syn::GenericArgument::Type(inner) => Ok(Some(inner)),
                        _ => Err(syn::Error::new(args.span(), "expect type parameter")),
                    }
                }
                _ => Err(syn::Error::new(first.arguments.span(), "expect <..>")),
            }
        }
        _ => Ok(None),
    }
}
