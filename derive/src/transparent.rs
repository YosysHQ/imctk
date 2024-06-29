use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::{require_repr_transparent, resolve_crate, ReprTransparent};

pub fn derive_subtype_cast(input: DeriveInput) -> syn::Result<TokenStream> {
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
    } = require_repr_transparent(&ident, "SubtypeCast", &attrs, &data)?;

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

pub fn derive_newtype_cast(input: DeriveInput) -> syn::Result<TokenStream> {
    #![allow(non_snake_case)]

    let DeriveInput {
        ident, generics, ..
    } = input;

    let imctk_transparent = resolve_crate("imctk-transparent");

    let NewtypeCast = quote![#imctk_transparent::NewtypeCast];

    let target_ident = ident;
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let target_type = quote![#target_ident #type_generics];

    Ok(quote! {
        // SAFETY: this trait is unsafe to allow SubtypeCast types to uphold extra invariants, but
        // since the derive macro is restricted to the definition site of the type it can only be
        // used at the place where such a decision would be made
        unsafe impl #impl_generics #NewtypeCast for #target_type #where_clause {
        }
    })
}
