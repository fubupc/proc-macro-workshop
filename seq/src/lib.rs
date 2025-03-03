use std::ops::Range;

use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenStream as TokenStream2, TokenTree};
use quote::{format_ident, TokenStreamExt};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input, token, Error, Ident, LitInt, Result, Token,
};

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let seq = parse_macro_input!(input as Seq);

    impl_seq(&seq)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

fn impl_seq(seq: &Seq) -> Result<TokenStream2> {
    let index_range = if seq.inclusive {
        seq.start..seq.end + 1
    } else {
        seq.start..seq.end
    };

    let (expanded, repeat_found) = expand_repeat_sections(seq.content.clone(), &|section| {
        gen_seq(section, &seq.index_ident, index_range.clone())
    })?;

    if repeat_found {
        Ok(expanded)
    } else {
        // If no repeat section `#(...)*` found, just repeat the whole content of `seq!`
        gen_seq(seq.content.clone(), &seq.index_ident, index_range)
    }
}

/// Generate repeated sequence of token stream
fn gen_seq(
    ts: TokenStream2,
    index_ident: &Ident,
    index_range: Range<usize>,
) -> Result<TokenStream2> {
    index_range
        .map(|n| replace_and_paste(ts.clone(), index_ident, n))
        .collect()
}

/// Expand inner repeat sections `#(...)*` if exist.
fn expand_repeat_sections<F>(
    stream: TokenStream2,
    repeat_callback: &F,
) -> Result<(TokenStream2, bool)>
where
    F: Fn(TokenStream2) -> Result<TokenStream2>,
{
    let mut remaining = stream.into_iter();
    let mut result = TokenStream2::new();
    let mut repeat_found = false;

    while let Some(tt) = remaining.next() {
        match tt {
            TokenTree::Group(g) => {
                let (expanded, _repeat_found) =
                    expand_repeat_sections(g.stream(), repeat_callback)?;
                repeat_found |= _repeat_found;
                result.append(Group::new(g.delimiter(), expanded));
            }
            // Search for `#(...)*` pattern
            TokenTree::Punct(p) if p.as_char() == '#' => {
                let mut peeker = remaining.clone();
                match (peeker.next(), peeker.next()) {
                    (Some(TokenTree::Group(g)), Some(TokenTree::Punct(p2)))
                        if matches!(g.delimiter(), Delimiter::Parenthesis)
                            && p2.as_char() == '*' =>
                    {
                        repeat_found = true;
                        let (expanded, _) = expand_repeat_sections(g.stream(), repeat_callback)?;
                        result.extend(repeat_callback(expanded)?);
                        remaining = peeker;
                    }
                    _ => result.append(p),
                }
            }
            _ => result.append(tt),
        }
    }

    Ok((result, repeat_found))
}

// Replace index identifier with number in token stream. Paste tokens connected by `~`.
fn replace_and_paste(
    stream: TokenStream2,
    index_ident: &Ident,
    index_num: usize,
) -> Result<TokenStream2> {
    let mut remaining = stream.into_iter();
    let mut result = TokenStream2::new();

    while let Some(tt) = remaining.next() {
        match tt {
            TokenTree::Group(g) => {
                let replaced = replace_and_paste(g.stream(), index_ident, index_num)?;
                result.append(Group::new(g.delimiter(), replaced))
            }
            // Assuming `N` is the `seq!` index.
            TokenTree::Ident(i) => {
                if &i == index_ident {
                    // `N~...` is invalid because the symbol generated by paste operation will be an invalid identifier
                    // start with number.
                    let mut peeker = remaining.clone();
                    if let Some(TokenTree::Punct(p)) = peeker.next() {
                        if p.as_char() == '~' {
                            return Err(Error::new(
                                i.span(),
                                "seq!: non-standalone index identifier cannot be the start of paste expression",
                        ));
                        }
                    }
                    // Standalone `N` is ok to be replaced by literal number.
                    result.append(Literal::usize_unsuffixed(index_num));
                } else {
                    let (pasted, consumed_tokens) =
                        peek_and_paste(index_ident, index_num, i, remaining.clone())?;
                    result.append(pasted);
                    for _ in 0..consumed_tokens {
                        remaining.next();
                    }
                }
            }
            _ => result.append(tt),
        }
    }
    Ok(result)
}

// Paste tokens connected by `~`
// Example: `a~N~b~N~666~c` (N=1) should produce `a1b1666c`.
fn peek_and_paste(
    index_ident: &Ident,
    index_num: usize,
    first: Ident,
    mut peeker: impl Iterator<Item = TokenTree>,
) -> Result<(Ident, usize)> {
    let mut pasted = first;
    let mut consumed_tokens = 0;

    while let Some(TokenTree::Punct(p)) = peeker.next() {
        if p.as_char() != '~' {
            break;
        }
        consumed_tokens += 2;
        match peeker.next() {
            Some(TokenTree::Ident(i2)) => {
                if &i2 == index_ident {
                    pasted = format_ident!("{}{}", pasted, index_num);
                } else {
                    pasted = format_ident!("{}{}", pasted, i2);
                }
            }
            Some(TokenTree::Literal(lit)) => {
                pasted = format_ident!("{}{}", pasted, lit.to_string());
            }
            _ => {
                return Err(Error::new(
                    p.span(),
                    "seq!: paste operator `~` must be followed by identifier or literal integer",
                ))
            }
        }
    }

    Ok((pasted, consumed_tokens))
}

struct Seq {
    index_ident: Ident,
    start: usize,
    inclusive: bool,
    end: usize,
    content: TokenStream2,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        let _in_token: Token![in] = input.parse()?;
        let start = input.parse::<LitInt>()?.base10_parse()?;
        let _dot2_token: Token![..] = input.parse()?;
        let inclusive = input.parse::<Option<Token![=]>>()?.is_some();
        let end = input.parse::<LitInt>()?.base10_parse()?;
        let content;
        let _brace_token: token::Brace = braced!(content in input);
        Ok(Seq {
            index_ident: ident,
            start,
            inclusive,
            end,
            content: content.parse()?,
        })
    }
}
