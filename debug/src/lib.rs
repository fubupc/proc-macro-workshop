use std::vec;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, Attribute, Data, DataStruct, DeriveInput,
    Error, Fields, FieldsNamed, GenericArgument, Lit, LitStr, Meta, MetaNameValue, PathArguments,
    Result, Type, TypeParam, TypePath, TypeTuple,
};

// use syn::generics::TypeParams;

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
    for p in generics.type_params_mut() {
        if fields.iter().any(|f| need_debug(p, &f.ty)) {
            p.bounds.push(parse_quote!(::std::fmt::Debug));
        }
    }

    // TODO: Deduplicate the same associated types.
    let assoc_types: Vec<_> = fields
        .iter()
        .flat_map(|f| {
            generics
                .type_params()
                .flat_map(|tp| find_assoc_types(tp, &f.ty))
        })
        .collect();
    let mut where_clause = generics
        .where_clause
        .take()
        .map_or(syn::parse_quote!(where), |c| c);
    for at in assoc_types {
        where_clause
            .predicates
            .push(syn::parse_quote!(#at: ::std::fmt::Debug))
    }
    generics.where_clause = Some(where_clause);

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

// fn ident_in_type_params(ident: &Ident, type_params: &[&TypeParam]) -> bool {
//     type_params.iter().any(|p| ident == &p.ident)
// }

// Determine whether type paramter `T` needs Debug trait bound.
//
// Need examples:
// 1. `T`
// 2. `Vec<T>`, `::std::boxed::Box<T>`
// 3. `[T; N]`, `[Vec<T>; N]`
// 4. `(T, U)`, `(Box<T>, Vec<N>)
// 5. `&'a T`, `&Vec<T>`
// 6. `*const T`, `*const Vec<T>`
//
// Exclude:
// 1. `PhantomData<T>`
// 2. `T::Assoc`
fn need_debug(tp: &TypeParam, ty: &Type) -> bool {
    match ty {
        Type::Array(a) => need_debug(tp, &a.elem),
        Type::BareFn(_) => false,
        Type::Group(g) => need_debug(tp, &g.elem),
        Type::ImplTrait(_) => false,
        Type::Infer(_) => false,
        Type::Macro(_) => false,
        Type::Never(_) => false,
        Type::Paren(p) => need_debug(tp, &p.elem),
        Type::Path(p) => need_debug_for_type_path(tp, p),
        Type::Ptr(p) => need_debug(tp, &p.elem),
        Type::Reference(r) => need_debug(tp, &r.elem),
        Type::Slice(s) => need_debug(tp, &s.elem),
        Type::TraitObject(_) => false,
        Type::Tuple(t) => t.elems.iter().any(|e| need_debug(tp, e)),
        Type::Verbatim(_) => false,
        _ => todo!(),
    }
}

fn need_debug_for_type_path(tp: &TypeParam, path: &TypePath) -> bool {
    match &path.qself {
        Some(_) => false, // `T::Assoc`, ignored.
        None => {
            let segments: Vec<_> = path.path.segments.iter().collect();
            match segments.as_slice() {
                [] => unreachable!(),
                [s] => {
                    // The path segment following leading `::` must be crate. So a valid path with only one segment
                    // cannot start with `::`.
                    assert!(path.path.leading_colon.is_none());

                    if s.ident == tp.ident {
                        // Standalone `T`
                        assert!(s.arguments.is_none());
                        return true;
                    } else if s.ident == "PhantomData" {
                        // `PhantomData<T>`, ignored.
                        return false;
                    } else {
                        // `Vec<T>`
                        need_debug_for_path_arguments(tp, &s.arguments)
                    }
                }
                [heads @ .., last] => {
                    // `std::boxed::Box<T>`

                    // FIXME: Path segments except the last one cannot have arguments?
                    for h in heads {
                        assert!(h.arguments.is_none())
                    }
                    need_debug_for_path_arguments(tp, &last.arguments)
                }
            }
        }
    }
}

fn need_debug_for_path_arguments(tp: &TypeParam, args: &PathArguments) -> bool {
    match args {
        PathArguments::None => false,
        PathArguments::AngleBracketed(args) => {
            for a in &args.args {
                if let GenericArgument::Type(t) = a {
                    if need_debug(tp, &t) {
                        return true;
                    }
                }
            }
            false
        }
        PathArguments::Parenthesized(_) => false,
    }
}

// Find associated types in type with type parameter `T`, `U`.
//
// Support:
// 1. `T::Assoc`
// 2. `<T as a::b::Trait>::Assoc`
// 3. `<u8 as Trait<T>>::Assoc`
// 4. `<u8 as Trait>::Assoc<T>`
// 5. `[T::Assoc; N]`
// 6. `(T::Assoc1, U::Assoc2)`
// 7. `&'a T::Assoc1`
// 8. `*const T::Assoc1`
//
// Not support:
// 1. `T::Assoc1<U::Assoc2>` (?)
fn find_assoc_types<'a>(tp: &TypeParam, ty: &'a Type) -> Vec<&'a TypePath> {
    match ty {
        Type::Array(a) => find_assoc_types(tp, &a.elem),
        Type::BareFn(_) => Vec::new(),
        Type::Group(g) => find_assoc_types(tp, &g.elem),
        Type::ImplTrait(_) => Vec::new(),
        Type::Infer(_) => Vec::new(),
        // FIXME: Cannot determine whether macro used type parameter.
        Type::Macro(_) => Vec::new(),
        Type::Never(_) => Vec::new(),
        Type::Paren(p) => find_assoc_types(tp, &p.elem),
        Type::Path(p) => find_in_type_path(tp, p),
        Type::Ptr(p) => find_assoc_types(tp, &p.elem),
        Type::Reference(r) => find_assoc_types(tp, &r.elem),
        Type::Slice(s) => find_assoc_types(tp, &s.elem),
        Type::TraitObject(_) => Vec::new(),
        Type::Tuple(t) => find_in_tuple(tp, t),
        // FIXME: Cannot determine whether verbatim tokenstream used type parameter.
        Type::Verbatim(_) => Vec::new(),
        _ => todo!(),
    }
}

// Find the outmost (if nested) associated types.
fn find_in_type_path<'a>(tp: &TypeParam, path: &'a TypePath) -> Vec<&'a TypePath> {
    match &path.qself {
        Some(qself) => {
            assert!(path.path.segments.len() == qself.position + 1);

            // 1. `<T as a::b::Trait>::Assoc`
            // 2. `<Vec<T> as a::b::Trait>::Assoc`
            if need_debug(tp, qself.ty.as_ref()) {
                return vec![path];
            }

            // 3. `<u8 as a::b::Trait<T>>::Assoc`
            let trait_ = path.path.segments.iter().take(qself.position).last();
            if let Some(s) = trait_ {
                if need_debug_for_path_arguments(tp, &s.arguments) {
                    return vec![path];
                }
            }

            // 4. `<u8 as a::b::Trait>::Assoc<T>`
            let assoc = &path.path.segments[qself.position];
            if need_debug_for_path_arguments(tp, &assoc.arguments) {
                return vec![path];
            }
            vec![]
        }
        None => {
            let segments: Vec<_> = path.path.segments.iter().collect();
            match segments.as_slice() {
                [] => unreachable!(),
                [s] => {
                    // The path segment following leading `::` must be crate. So a valid path with only one segment
                    // cannot start with `::`.
                    assert!(path.path.leading_colon.is_none());

                    if s.ident == tp.ident {
                        // Standalone `T`
                        assert!(s.arguments.is_none());
                        return vec![];
                    } else if s.ident == "PhantomData" {
                        // `PhantomData<T>`
                        return vec![];
                    } else {
                        // `Vec<T::Assoc>`
                        find_in_path_arguments(tp, &s.arguments)
                    }
                }
                [s1, s2] => {
                    // `T::Assoc`
                    if s1.ident == tp.ident && path.path.leading_colon.is_none() {
                        assert!(s1.arguments.is_none());
                        return vec![path];
                    }

                    // 1. `::a::X<T::Assoc>`
                    // 2. `a::X<T::Assoc>`
                    find_in_path_arguments(tp, &s2.arguments)
                }
                [heads @ .., last] => {
                    // FIXME: Path segments except the last one cannot have arguments?
                    for h in heads {
                        assert!(h.arguments.is_none())
                    }
                    // `std::boxed::Box<T::Assoc>`
                    find_in_path_arguments(tp, &last.arguments)
                }
            }
        }
    }
}

fn find_in_tuple<'a>(tp: &TypeParam, f: &'a TypeTuple) -> Vec<&'a TypePath> {
    f.elems
        .iter()
        .flat_map(|e| find_assoc_types(tp, e))
        .collect()
}

fn find_in_path_arguments<'a>(tp: &TypeParam, args: &'a PathArguments) -> Vec<&'a TypePath> {
    match args {
        PathArguments::None => vec![],
        PathArguments::AngleBracketed(args) => args
            .args
            .iter()
            .flat_map(|a| match a {
                GenericArgument::Type(gt) => find_assoc_types(tp, gt),
                _ => vec![],
            })
            .collect(),
        PathArguments::Parenthesized(_) => vec![],
    }
}
