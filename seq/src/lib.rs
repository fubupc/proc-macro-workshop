use proc_macro::TokenStream;
use proc_macro2::{Group, Literal, TokenStream as TokenStream2, TokenTree};
use quote::ToTokens;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input, token, Ident, LitInt, Result, Token,
};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);

    impl_seq(&seq)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn impl_seq(seq: &Seq) -> Result<TokenStream2> {
    let start: usize = seq.start.base10_parse()?;
    let end: usize = seq.end.base10_parse()?;
    let repeated: TokenStream2 = (start..end)
        .map(|n| replace_ident(seq.content.clone(), &seq.ident, n))
        .collect();

    Ok(repeated)
}

fn replace_ident(content: TokenStream2, ident: &Ident, n: usize) -> TokenStream2 {
    content
        .into_iter()
        .map(|tt| replace_in_tt(tt, ident, n))
        .collect()
}

fn replace_in_tt(tt: TokenTree, ident: &Ident, n: usize) -> TokenTree {
    match tt {
        TokenTree::Group(g) => {
            let delimiter = g.delimiter();
            let replaced = replace_ident(g.stream(), ident, n);
            TokenTree::Group(Group::new(delimiter, replaced))
        }
        TokenTree::Ident(i) => {
            if &i == ident {
                TokenTree::Literal(Literal::usize_unsuffixed(n))
            } else {
                TokenTree::Ident(i)
            }
        }
        TokenTree::Punct(p) => TokenTree::Punct(p),
        TokenTree::Literal(l) => TokenTree::Literal(l),
    }
}

struct Seq {
    ident: Ident,
    in_token: Token![in],
    start: LitInt,
    dot2_token: Token![..],
    end: LitInt,
    brace_token: token::Brace,
    content: TokenStream2,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        Ok(Seq {
            ident: input.parse()?,
            in_token: input.parse()?,
            start: input.parse()?,
            dot2_token: input.parse()?,
            end: input.parse()?,
            brace_token: braced!(content in input),
            content: content.parse()?,
        })
    }
}
