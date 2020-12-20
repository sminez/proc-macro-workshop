use proc_macro::TokenStream;
use quote::quote;
use std::iter::FromIterator;
use syn::{
    braced,
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Ident, LitInt, Token,
};

// Valid bodies are one of the following:
//      { ... #ident ... }
//      { ... PREF #ident ... }
//      { ... PREF #ident #SUFF ... }
//      { ... #( ... #ident ... )* ... }
//      { ... #( ... PREF #ident ... )* ... }
//      { ... #( ... PREF #ident #SUFF ... )* ... }
//
//  where '...' can also include other substitutions

struct Seq {
    ident: Ident,
    from: usize,
    to: usize,
    body: proc_macro2::TokenStream,
}
impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let from: LitInt = input.parse()?;
        input.parse::<Token![..]>()?;
        let to: LitInt = input.parse()?;

        let content;
        let _braces = braced!(content in input);
        let body = proc_macro2::TokenStream::parse(&content)?;

        Ok(Self {
            ident,
            from: from.base10_parse::<usize>()?,
            to: to.base10_parse::<usize>()?,
            body,
        })
    }
}

fn replace_in_stream(
    ident: &Ident,
    n: usize,
    stream: proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let mut replaced = proc_macro2::TokenStream::new();
    let mut token_trees = stream.into_iter().peekable();

    while let Some(tt) = token_trees.next() {
        replaced.extend(replace_with_lookahead(tt, ident, n, &mut token_trees));
    }

    replaced
}

fn replace_with_lookahead(
    tt: proc_macro2::TokenTree,
    ident: &Ident,
    n: usize,
    tts: &mut std::iter::Peekable<proc_macro2::token_stream::IntoIter>,
) -> proc_macro2::TokenStream {
    let tt = match tt {
        // Our target identifier to replace on its own
        proc_macro2::TokenTree::Ident(ref i) if i == ident => {
            let mut lit = proc_macro2::Literal::usize_unsuffixed(n);
            lit.set_span(ident.span());
            proc_macro2::TokenTree::Literal(lit)
        }

        // Some other identifier: could be i, i#ident or i#ident#_suffix
        proc_macro2::TokenTree::Ident(mut i) => {
            if let Some(proc_macro2::TokenTree::Punct(ref p)) = tts.peek() {
                if p.as_char() == '#' {
                    let (_, should_be_ident) = (tts.next(), tts.next().unwrap());
                    match should_be_ident {
                        proc_macro2::TokenTree::Ident(ref i) if i != ident => {
                            panic!("expected #{} got #{}", ident, should_be_ident)
                        }
                        _ => {}
                    }
                    i = proc_macro2::Ident::new(&format!("{}{}", i, n), ident.span());

                    // Check for a suffix as well
                    if let Some(proc_macro2::TokenTree::Punct(ref p)) = tts.peek() {
                        if p.as_char() == '#' {
                            let (_, suffix) = (tts.next(), tts.next().unwrap());
                            i = proc_macro2::Ident::new(&format!("{}{}", i, suffix), ident.span());
                        }
                    }
                }
            }
            proc_macro2::TokenTree::Ident(i)
        }

        // Some delimited group so we need to recurse
        proc_macro2::TokenTree::Group(ref g) => {
            let delim = g.delimiter();
            proc_macro2::TokenTree::Group(proc_macro2::Group::new(
                delim,
                replace_in_stream(ident, n, g.stream()),
            ))
        }

        // Nothing we need to worry about so just pass it back
        _ => tt,
    };

    proc_macro2::TokenStream::from_iter(std::iter::once(tt))
}

#[proc_macro]
pub fn seq(input: TokenStream) -> TokenStream {
    let Seq {
        ident,
        from,
        to,
        body,
    } = parse_macro_input!(input as Seq);

    let iterations = (from..to).map(|n| replace_in_stream(&ident, n, body.clone()));
    TokenStream::from(quote! { #(#iterations)* })
}
