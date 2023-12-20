use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, Fields::Named, Field, DataStruct, DataEnum};
use quote::quote;

#[proc_macro_derive(GetSpan, attributes(span))]
pub fn get_span_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let trait_impl: proc_macro2::TokenStream = generate_span_getters(&input.data, &input.ident);
    TokenStream::from(quote!{
        #trait_impl
    })
}

fn generate_span_getters(data: &Data, ident: &syn::Ident) -> proc_macro2::TokenStream {
    return match data {
        Data::Struct(struct_data) => generate_span_getters_struct(struct_data, ident),
        Data::Enum(enum_data) => generate_span_getters_enum(enum_data, ident),
        Data::Union(..) => unimplemented!()
    }
}

fn generate_span_getters_struct(struct_data: &DataStruct, ident: &syn::Ident) -> proc_macro2::TokenStream {
    match &struct_data.fields {
        Named(fields) => {
            let option_field = fields.named.iter().find_map(|field| {
                for attr in &field.attrs {
                    if attr.path().is_ident("span") {
                        return Some(field)
                    }
                }
                None
            });
            return generate_fns_struct(option_field, ident)
        }
        _ => generate_fns_struct(None, ident)
    }
}

fn generate_span_getters_enum(enum_data: &DataEnum, ident: &syn::Ident) -> proc_macro2::TokenStream {
    let match_arms = enum_data.variants.iter().map(|variant| {
        match &variant.fields {
            Named(fields) => {
                let field: Option<&syn::Ident> = fields.named.iter().find_map(|field| {
                    for attr in &field.attrs {
                        if attr.path().is_ident("span") {
                            return Some(field.ident.as_ref().unwrap())
                        }
                    }
                    None
                });
                
                generate_match_arm(field, ident, &variant.ident)
            }
            _ => generate_match_arm(None, ident, &variant.ident)
        }
    });
    
    quote!{
        impl #ident {
            pub fn get_option_span(&self) -> Option<&span::Span> {
                match self {
                    #( #match_arms )*
                    _ => None
                }
            }

            pub fn get_span(&self) -> &span::Span {
                unimplemented!()
            }
        }
    }
}

fn generate_match_arm(field: Option<&syn::Ident>, ident: &syn::Ident, variant_ident: &syn::Ident) -> proc_macro2::TokenStream {
    println!("{:?}", field);
    match field {
        Some(field_ident) => quote!{
            #ident::#variant_ident { #field_ident, .. } => Some(#field_ident),
        },
        None => quote!{
            #ident::#variant_ident { .. } => None,
        }
    }
}

fn generate_fns_struct(option_field: Option<&Field>, ident: &syn::Ident) -> proc_macro2::TokenStream {
    match option_field {
        Some(field) => {
            let ty = &field.ty;
            let field_ident = &field.ident;
            quote!{
                impl #ident {
                    pub fn get_span(&self) -> &#ty {
                        &self.#field_ident
                    }
    
                    pub fn get_option_span(&self) -> Option<&#ty> {
                        Some(&self.#field_ident)
                    }
                }
            }
        },
        None => {
            quote!{
                impl #ident {
                    pub fn get_span(&self) -> &span::Span {
                        unreachable!()
                    }

                    pub fn get_option_span(&self) -> Option<&span::Span> {
                        None
                    }
                }
            }
        }
    }
}
