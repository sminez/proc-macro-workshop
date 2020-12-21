use proc_macro::TokenStream;
use std::iter::FromIterator;
use syn::{
    braced,
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Ident, LitInt, Token,
};

#[derive(Debug)]
enum Section {
    Passthrough(proc_macro2::TokenStream),
    Replace(proc_macro2::TokenStream),
}

struct Seq {
    ident: Ident,
    from: usize,
    to: usize,
    body: Vec<Section>,
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

        Ok(Self {
            ident,
            from: from.base10_parse::<usize>()?,
            to: to.base10_parse::<usize>()?,
            body: split_body(&content)?,
        })
    }
}

fn split_body(input: ParseStream) -> Result<Vec<Section>> {
    let everything = proc_macro2::TokenStream::parse(input)?;

    let mut sections = Vec::new();
    let mut tts = everything.into_iter().peekable();
    let mut current = proc_macro2::TokenStream::new();

    while let Some(tt) = tts.next() {
        match tt {
            // Potential start of a #( ... )* group
            proc_macro2::TokenTree::Punct(ref p) if p.as_char() == '#' => {
                if tts.peek().is_none() {
                    continue;
                }
                let next = tts.next().unwrap();
                match next {
                    // #(...) #[...] #<...> ?
                    proc_macro2::TokenTree::Group(ref g) => {
                        if g.delimiter() == proc_macro2::Delimiter::Parenthesis {
                            // #( ... )*
                            if let Some(proc_macro2::TokenTree::Punct(ref p)) = tts.peek() {
                                if p.as_char() == '*' {
                                    tts.next(); // consume the '*'
                                    sections.push(Section::Passthrough(current));
                                    sections.push(Section::Replace(g.stream()));
                                    current = proc_macro2::TokenStream::new();
                                    continue;
                                }
                                panic!("Invalid seq! section");
                            }
                        }
                        current.extend(proc_macro2::TokenStream::from_iter(vec![tt, next]));
                    }

                    // # something
                    next => {
                        current.extend(proc_macro2::TokenStream::from_iter(vec![tt, next]));
                    }
                }
            }

            // Literally anything other than a '#'
            tt => {
                current.extend(proc_macro2::TokenStream::from_iter(std::iter::once(tt)));
            }
        }
    }

    if !current.is_empty() {
        sections.push(Section::Passthrough(current));
    }

    for section in sections.iter() {
        match section {
            Section::Passthrough(s) => eprintln!("PASSTHROUGH :: {}", s),
            Section::Replace(s) => eprintln!("REPLACE :: {}", s),
        }
    }
    Ok(sections)
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

    let mut replaced = proc_macro2::TokenStream::new();
    let replace_all = body.len() == 1;

    for section in body {
        let (should_replace, stream) = match section {
            Section::Passthrough(s) => (replace_all, s),
            Section::Replace(s) => (true, s),
        };

        if should_replace {
            for n in from..to {
                replaced.extend(replace_in_stream(&ident, n, stream.clone()));
            }
        } else {
            replaced.extend(stream)
        }
    }

    TokenStream::from(replaced)
}
