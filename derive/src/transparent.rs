use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{DeriveInput, Ident};

use crate::{require_repr_transparent, resolve_crate, ReprTransparent};

pub fn derive_subtype_cast(input: DeriveInput) -> syn::Result<TokenStream> {
    derive_cast(input, Ident::new("SubtypeCast", Span::call_site()))
}

pub fn derive_newtype_cast(input: DeriveInput) -> syn::Result<TokenStream> {
    derive_cast(input, Ident::new("NewtypeCast", Span::call_site()))
}

fn derive_cast(input: DeriveInput, trait_ident: Ident) -> syn::Result<TokenStream> {
    #![allow(non_snake_case)]

    let DeriveInput {
        attrs,
        ident,
        generics,
        data,
        ..
    } = input;

    let ReprTransparent {
        field,
        field_ty: inner,
        extra_init,
    } = require_repr_transparent(&ident, &trait_ident.to_string(), &attrs, &data)?;

    let imctk_transparent = resolve_crate("imctk-transparent");

    let SubtypeCast = quote![#imctk_transparent::SubtypeCast];

    let target_ident = ident;
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let target_type = quote![#target_ident #type_generics];

    Ok(quote! {
        unsafe impl #impl_generics #SubtypeCast for #target_type #where_clause {
            type Repr = #inner;

            const __STATIC_ASSERT_HELPER: () = {
                let _ = |&Self { #field: _ #extra_init }: &Self| ();
            };
        }
    })
}
