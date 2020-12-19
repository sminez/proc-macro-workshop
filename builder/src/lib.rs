use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data::Struct, DataStruct, DeriveInput, Fields::Named};

#[proc_macro_derive(Builder)]
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
        quote! { #name: std::option::Option<#ty> }
    });

    // Empty initial values for when we construct the builder
    let none_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote! { #name: std::option::Option::None }
    });

    // The per-field unwrap as part of the build method
    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
        }
    });

    // The individual setter methods for each field
    let setter_methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! {
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = std::option::Option::Some(#name);
                self
            }
        }
    });

    // Now build the new tokens to hand back to the compiler
    TokenStream::from(quote! {
        pub struct #builder_ident {
            #(#builder_fields,)*
        }

        impl #builder_ident {
            #(#setter_methods)*

            fn build(&self) -> Result<#input_ident, Box<dyn std::error::Error>> {
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
