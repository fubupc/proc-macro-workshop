use proc_macro::TokenStream;
use proc_macro2::{Delimiter, Group, Literal, TokenStream as TokenStream2, TokenTree};
use quote::{format_ident, TokenStreamExt};
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    spanned::Spanned,
    token, Error, Ident, LitInt, Result, Token,
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

    let range = if seq.inclusive.is_some() {
        start..end + 1
    } else {
        start..end
    };

    if contains_repeat_section(seq.content.clone())? {
        process_repeat_sections(seq.content.clone(), &|section| {
            range
                .clone()
                .map(|n| process_ident(section.clone(), &seq.ident, n))
                .collect()
        })
    } else {
        range
            .map(|n| process_ident(seq.content.clone(), &seq.ident, n))
            .collect()
    }
}

/// Check whether contains repeat section `#(...)*`
fn contains_repeat_section(stream: TokenStream2) -> Result<bool> {
    let mut stream = stream.into_iter().peekable();

    while let Some(tt) = stream.next() {
        match tt {
            // Found `#`
            TokenTree::Punct(p) if p.as_char() == '#' => {
                match stream.peek() {
                    // Found `#(...)`
                    Some(TokenTree::Group(g))
                        if matches!(g.delimiter(), Delimiter::Parenthesis) =>
                    {
                        // Consume `(...)` token actually
                        stream.next();
                        match stream.peek() {
                            // Found `#(...)*`
                            Some(TokenTree::Punct(p2)) if p2.as_char() == '*' => return Ok(true),
                            other => {
                                return Err(Error::new(
                                    other.span(),
                                    "expect `*` of repeat section `#(...)*`",
                                ))
                            }
                        };
                    }
                    _ => {}
                };
            }
            TokenTree::Group(g) => {
                if contains_repeat_section(g.stream())? {
                    return Ok(true);
                }
            }
            _ => {}
        };
    }
    Ok(false)
}

fn process_repeat_sections<F>(
    stream: TokenStream2,
    repeat_section_callback: &F,
) -> Result<TokenStream2>
where
    F: Fn(TokenStream2) -> Result<TokenStream2>,
{
    let mut stream = stream.into_iter().peekable();
    let mut result = TokenStream2::new();

    while let Some(tt) = stream.next() {
        match tt {
            // Found `#`
            TokenTree::Punct(p) if p.as_char() == '#' => {
                match stream.peek() {
                    // Found `#(...)`
                    Some(TokenTree::Group(g))
                        if matches!(g.delimiter(), Delimiter::Parenthesis) =>
                    {
                        // Consume `(...)` token actually
                        let g = match stream.next() {
                            Some(TokenTree::Group(g)) => g,
                            _ => unreachable!(),
                        };
                        match stream.peek() {
                            // Found `#(...)*`
                            Some(TokenTree::Punct(p2)) if p2.as_char() == '*' => {
                                // Consume `*` token actually
                                stream.next();

                                // Generate repeat sections
                                let repeated = repeat_section_callback(g.stream())?;
                                result.extend(repeated);
                            }
                            other => {
                                return Err(Error::new(
                                    other.span(),
                                    "expect `*` of repeat section `#(...)*`",
                                ))
                            }
                        };
                        g
                    }
                    _ => {
                        result.append(TokenTree::Punct(p));
                        continue;
                    }
                };
            }
            TokenTree::Group(g) => {
                let delimiter = g.delimiter();
                let processed = process_repeat_sections(g.stream(), repeat_section_callback)?;
                result.append(TokenTree::Group(Group::new(delimiter, processed)));
            }
            _ => result.append(tt),
        };
    }

    Ok(result)
}

fn process_ident(stream: TokenStream2, seq_ident: &Ident, num: usize) -> Result<TokenStream2> {
    let mut stream = stream.into_iter().peekable();
    let mut result = TokenStream2::new();

    while let Some(tt) = stream.next() {
        match tt {
            TokenTree::Group(g) => {
                let delimiter = g.delimiter();
                let pasted = process_ident(g.stream(), seq_ident, num)?;
                result.append(TokenTree::Group(Group::new(delimiter, pasted)))
            }
            TokenTree::Ident(i) => {
                if &i == seq_ident {
                    match stream.peek() {
                        Some(TokenTree::Punct(p)) if p.as_char() == '~' => return Err(Error::new(
                            i.span(),
                            "identifier cannot be start with number (produced by seq identifier)",
                        )),
                        _ => result.append(TokenTree::Literal(Literal::usize_unsuffixed(num))),
                    };
                } else {
                    let mut curr = i;
                    loop {
                        match stream.peek() {
                            Some(TokenTree::Punct(p)) if p.as_char() == '~' => {
                                stream.next(); // pop ~
                                match stream.next() {
                                    Some(TokenTree::Ident(i2)) => {
                                        if &i2 == seq_ident {
                                            curr = format_ident!("{}{}", curr, num);
                                        } else {
                                            curr = format_ident!("{}{}", curr, i2);
                                        }
                                    }
                                    other => {
                                        return Err(Error::new(
                                            other.span(),
                                            "paste operator ~ must be followed by an identifier",
                                        ))
                                    }
                                }
                            }
                            _ => {
                                result.append(curr);
                                break;
                            }
                        };
                    }
                }
            }
            TokenTree::Punct(p) => {
                if p.as_char() == '~' {
                    return Err(Error::new(
                        p.span(),
                        "paste operator ~ must be preceded by an identifier",
                    ));
                } else {
                    result.append(TokenTree::Punct(p))
                }
            }
            TokenTree::Literal(l) => result.append(TokenTree::Literal(l)),
        }
    }
    Ok(result)
}

struct Seq {
    ident: Ident,
    in_token: Token![in],
    start: LitInt,
    dot2_token: Token![..],
    inclusive: Option<Token![=]>,
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
            inclusive: input.parse()?,
            end: input.parse()?,
            brace_token: braced!(content in input),
            content: content.parse()?,
        })
    }
}
