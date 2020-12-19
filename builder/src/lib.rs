use proc_macro::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse_macro_input, Attribute, Data::Struct, DataStruct, DeriveInput, Field, Fields::Named,
    GenericArgument, Ident, Lit, Meta, NestedMeta, PathArguments, Type, TypePath,
};

// Check that `ty` _is_ contained in the named wrapper (e.g. Vec, Option) and then
// return Some(T) for the wrapped T if it is, otherwise None.
fn wrapped_type<'a>(wrapper: &str, ty: &'a Type) -> std::option::Option<&'a Type> {
    if let Type::Path(TypePath { ref path, .. }) = ty {
        if path.segments.len() != 1 || path.segments[0].ident != wrapper {
            return None;
        }

        if let PathArguments::AngleBracketed(ref inner) = path.segments[0].arguments {
            if inner.args.len() != 1 {
                return None;
            }
            if let Some(GenericArgument::Type(ref t)) = inner.args.first() {
                return Some(t);
            }
        }
    }
    None
}

// Find our #[builder(each = "name")] attribute if it exists
fn builder_attr(f: &Field) -> Option<&Attribute> {
    for attr in f.attrs.iter() {
        let segs = &attr.path.segments;
        if segs.len() == 1 && segs[0].ident == "builder" {
            return Some(attr);
        }
    }
    None
}

// Helper for generating compile errors in `push_method`
fn builder_attr_error<T: ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
    Some((
        false,
        syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
    ))
}

// Construct the user named method for pushing onto a Vec<T> when decorated with #[builder(each = "name")]
// None if there is no `builder` attr, Some((name == field_name, method / compile error)) otherwise
fn push_method(f: &Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let attr = builder_attr(f)?;

    // If we have the #[builder(...)] attribute then this should be a name-value pair
    let meta = match attr.parse_meta() {
        Ok(Meta::List(mut elems)) => {
            // Something that looks like #[builder(...)] which we now need to validate
            if elems.nested.len() != 1 {
                return builder_attr_error(elems);
            }
            match elems.nested.pop().unwrap().into_value() {
                NestedMeta::Meta(Meta::NameValue(nv)) => {
                    if !nv.path.is_ident("each") {
                        return builder_attr_error(elems);
                    }
                    nv
                }
                m => {
                    return builder_attr_error(m);
                }
            }
        }
        Ok(meta) => {
            // #[builder] or #[builder = "thing"] (bth invalid)
            return builder_attr_error(meta);
        }
        Err(e) => {
            // Already an error before it gets to us?
            return Some((false, e.to_compile_error()));
        }
    };

    // This should be the "name" in `each = "name"`
    match meta.lit {
        Lit::Str(s) => {
            // We made it! This is our new method name so construct the method
            let name = &f.ident.as_ref().unwrap();
            let push_name = Ident::new(&s.value(), s.span());
            let ty = wrapped_type("Vec", &f.ty).unwrap();

            let method = quote! {
                pub fn #push_name(&mut self, #push_name: #ty) -> &mut Self {
                    self.#name.push(#push_name);
                    self
                }
            };
            Some((s.value() == name.to_string(), method))
        }
        // TODO: is there a repr we can get at for this other than .suffix() ?
        non_string => panic!("expected string, found {}", non_string.suffix()),
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let input_ident = ast.ident;
    let builder_ident = format_ident!("{}Builder", input_ident);

    // Find all of the named fields for the struct
    let fields = match ast.data {
        Struct(DataStruct {
            fields: Named(named),
            ..
        }) => named.named,
        _ => unimplemented!(),
    };

    // Map those field names into:

    // Corresponding fields on the builder
    let builder_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        // Option types don't get double wrapped and Vecs start empty
        if wrapped_type("Option", &f.ty).is_some() || push_method(&f).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    // Empty initial values for when we construct the builder
    let none_fields = fields.iter().map(|f| {
        let name = &f.ident;
        if push_method(&f).is_some() {
            quote! { #name: std::vec::Vec::new() }
        } else {
            quote! { #name: std::option::Option::None }
        }
    });

    // The per-field unwrap as part of the build method
    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;

        // Option types and vecs that we push to don't need unwrapping
        if wrapped_type("Option", &f.ty).is_some() || push_method(&f).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    // The individual setter methods for each field
    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let (arg_type, arg_val) = if let Some(inner) = wrapped_type("Option", &f.ty) {
            // The field itself is an option, so accept the wrapped type in the builder
            (inner, quote! { std::option::Option::Some(#name) })
        } else if builder_attr(f).is_some() {
            // Set the entire value of the Vec<T> using the field name
            (&f.ty, quote! { #name })
        } else {
            // Set Some(T) for a standard field
            (&f.ty, quote! { std::option::Option::Some(#name) })
        };

        let set_method = quote! {
            pub fn #name(&mut self, #name: #arg_type) -> &mut Self {
                self.#name = #arg_val;
                self
            }
        };

        match push_method(&f) {
            // No `builder` attr so no push method
            None => set_method,
            // Only push method as names conflict
            Some((true, push_method)) => push_method,
            // No conflict: generate both
            Some((false, push_method)) => {
                let methods = quote! {
                    #set_method
                    #push_method
                };
                methods
            }
        }
    });

    // Now build the new tokens to hand back to the compiler
    TokenStream::from(quote! {
        pub struct #builder_ident {
            #(#builder_fields,)*
        }

        impl #builder_ident {
            #(#methods)*

            fn build(&self) -> std::result::Result<#input_ident, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#input_ident {
                    #(#build_fields,)*
                })
            }
        }

        impl #input_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident { #(#none_fields,)* }
            }
        }
    })
}
