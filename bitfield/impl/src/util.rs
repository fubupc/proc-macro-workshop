use syn::{spanned::Spanned, Attribute, Error, Expr, ExprLit, Lit, Meta, Result};

pub(crate) fn parse_bits_attr(attrs: &[Attribute]) -> Result<Option<(usize, &Attribute)>> {
    let msg = "expected #[bits = N] where N in range: [1, 64]";
    for a in attrs {
        if a.path().is_ident("bits") {
            if let Meta::NameValue(kv) = &a.meta {
                if let Expr::Lit(ExprLit {
                    lit: Lit::Int(lit), ..
                }) = &kv.value
                {
                    let bits: usize = lit.base10_parse().map_err(|_| Error::new(a.span(), msg))?;
                    if bits > 64 {
                        return Err(Error::new(a.span(), "only support max bit size of 64"));
                    }
                    return Ok(Some((bits, a)));
                }
            };

            return Err(Error::new(a.span(), msg));
        }
    }
    Ok(None)
}
