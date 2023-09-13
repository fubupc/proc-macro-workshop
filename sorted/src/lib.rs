use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{
    spanned::Spanned,
    visit_mut::{self, VisitMut},
    Error, ExprMatch, ExprPath, Ident, Item, ItemFn, Pat, PatStruct, PatTupleStruct, Path, Result,
};

#[proc_macro_attribute]
pub fn sorted(args: TokenStream, input: TokenStream) -> TokenStream {
    impl_sorted(args, input)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

#[proc_macro_attribute]
pub fn check(args: TokenStream, input: TokenStream) -> TokenStream {
    impl_check(args, input)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn impl_sorted(_: TokenStream, input: TokenStream) -> Result<TokenStream2> {
    let input: Item = syn::parse(input)?;

    let Item::Enum(input) = input else {
        return Err(Error::new(
            Span::call_site(),
            "expected enum or match expression",
        ));
    };

    let variants: Vec<_> = input.variants.iter().collect();
    for i in 1..variants.len() {
        let (prev, curr) = (variants[i - 1], variants[i]);
        if curr.ident < prev.ident {
            // First unordered variant found
            match variants[0..i].binary_search_by_key(&&curr.ident, |v| &v.ident) {
                Ok(_) => return Err(Error::new(curr.span(), "duplicated variant identifier")),
                Err(j) => {
                    return Err(Error::new(
                        curr.ident.span(),
                        format!("{} should sort before {}", curr.ident, variants[j].ident),
                    ))
                }
            }
        }
    }

    Ok(quote!(#input))
}

fn impl_check(_: TokenStream, input: TokenStream) -> Result<TokenStream2> {
    let mut item_fn: ItemFn = syn::parse(input)?;
    let mut visitor = SortedVisitor { err: None };
    visitor.visit_item_fn_mut(&mut item_fn);

    let err: TokenStream2 = visitor
        .err
        .into_iter()
        .map(|e| e.into_compile_error())
        .collect();

    Ok(quote!(
        #item_fn

        #err
    ))
}

struct SortedVisitor {
    err: Option<Error>,
}

impl VisitMut for SortedVisitor {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        let sorted_removed: Vec<_> = i
            .attrs
            .iter()
            .filter(|a| !a.path().is_ident("sorted"))
            .cloned()
            .collect();

        if sorted_removed.len() < i.attrs.len() {
            // Has #[sorted]
            i.attrs = sorted_removed;

            if let Err(e) = check_match_expr(i) {
                self.err = Some(e);
                return;
            }
        }

        visit_mut::visit_expr_match_mut(self, i);
    }
}

fn check_match_expr(expr: &ExprMatch) -> Result<()> {
    let mut pats: Vec<Path> = Vec::new();
    for (idx, a) in expr.arms.iter().enumerate() {
        let p = match &a.pat {
            Pat::Ident(i) => Path::from(i.ident.clone()),
            Pat::Path(ExprPath { path, .. })
            | Pat::Struct(PatStruct { path, .. })
            | Pat::TupleStruct(PatTupleStruct { path, .. }) => path.clone(),
            Pat::Wild(_) => {
                if idx < expr.arms.len() - 1 {
                    return Err(Error::new(
                        a.pat.span(),
                        "wildcard(_) arm should be placed at last",
                    ));
                }
                continue;
            }
            _ => return Err(Error::new(a.pat.span(), "unsupported by #[sorted]")),
        };
        pats.push(p);
    }

    for i in 1..pats.len() {
        let (prev, curr) = (&pats[i - 1], &pats[i]);
        let (prev_key, curr_key) = (path_cmp_key(prev), path_cmp_key(curr));

        if curr_key < prev_key {
            return match pats[0..i].binary_search_by_key(&curr_key, |p| path_cmp_key(p)) {
                Ok(_) => Err(Error::new(curr.span(), "duplicated match arm")),
                Err(j) => Err(Error::new(
                    curr.span(),
                    format!(
                        "{} should sort before {}",
                        path_to_string(curr),
                        path_to_string(&pats[j]),
                    ),
                )),
            };
        }
    }

    Ok(())
}

// Only use the last path segment to compare
fn path_cmp_key(path: &Path) -> &Ident {
    &path.segments.last().unwrap().ident
}

fn path_to_string(path: &Path) -> String {
    path.to_token_stream().to_string().replace(" ", "")
}
