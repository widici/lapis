use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, Fields::Named, Field, FieldsNamed, DataStruct, DataEnum};
use quote::quote;

#[proc_macro_derive(GetSpan)]
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
            let option_field = fields.named.iter().find(|field| {
                if let Some(str_ident) = &field.ident {
                    str_ident == "span"
                } else {
                    false
                }
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
                let mut has_span = false;
                for field in fields.named.iter() {
                    if let Some(str_ident) = &field.ident {
                        has_span = str_ident == "span";
                        break;
                    }
                }
                generate_match_arm(has_span, ident, &variant.ident)
            }
            _ => generate_match_arm(false, ident, &variant.ident)
        }
    });
    
    quote!{
        impl #ident {
            pub fn get_option_span(&self) -> Option<&error::span::Span> {
                match self {
                    #( #match_arms )*
                    _ => None
                }
            }

            pub fn get_span(&self) -> &error::span::Span {
                unreachable!()
            }
        }
    }
}

fn generate_match_arm(has_span: bool, ident: &syn::Ident, variant_ident: &syn::Ident) -> proc_macro2::TokenStream {
    if has_span {
        quote!{
            #ident::#variant_ident { span, .. } => Some(span),
        }
    } else {
        quote!{
            #ident::#variant_ident { .. } => None,
        }
    }
}

fn generate_fns_struct(option_field: Option<&Field>, ident: &syn::Ident) -> proc_macro2::TokenStream {
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
