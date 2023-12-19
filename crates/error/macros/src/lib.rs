use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, Fields::Named, Field};
use quote::quote;

#[proc_macro_derive(GetSpan)]
pub fn get_span_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let trait_impl: proc_macro2::TokenStream = generate_span_accessor(&input.data, &input.ident);
    TokenStream::from(quote!{
        #trait_impl
    })
}

fn generate_span_accessor(data: &Data, ident: &syn::Ident) -> proc_macro2::TokenStream {
    match data {
        Data::Struct(struct_data) => match &struct_data.fields {
            Named(fields) => {
                let option_field = fields.named.iter().find(|field| {
                    if let Some(str_ident) = &field.ident {
                        str_ident == "span"
                    } else {
                        false
                    }
                });
                return generate_span_fns(option_field, ident)
            },
            _ => {}
        },
        _ => {}
    }
    generate_span_fns(None, ident)
}

fn generate_span_fns(option_field: Option<&Field>, ident: &syn::Ident) -> proc_macro2::TokenStream {
    match option_field {
        Some(field) => {
            let ty = &field.ty;
            quote!{
                impl #ident {
                    pub fn get_span(&self) -> &#ty {
                        &self.span
                    }
    
                    pub fn get_option_span(&self) -> Option<&#ty> {
                        Some(&self.span)
                    }
                }
            }
        },
        None => {
            quote!{
                impl #ident {
                    pub fn get_span(&self) -> &error::span::Span {
                        unreachable!()
                    }

                    pub fn get_option_span(&self) -> Option<&error::span::Span> {
                        None
                    }
                }
            }
        }
    }
}
