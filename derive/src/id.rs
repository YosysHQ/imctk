use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::{require_repr_transparent, resolve_crate, ReprTransparent};

pub fn derive_id(input: DeriveInput) -> syn::Result<TokenStream> {
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
    } = require_repr_transparent(&ident, "Id", &attrs, &data)?;

    // We're extra careful about shadowing of prelude items, as we're emitting code using `unsafe`
    let imctk_ids = resolve_crate("imctk-ids");
    let usize = quote![::core::primitive::usize];
    let bool = quote![::core::primitive::bool];

    let Id = quote![#imctk_ids::Id];

    let Clone = quote![::core::prelude::rust_2021::Clone];
    let Copy = quote![::core::prelude::rust_2021::Copy];
    let Eq = quote![::core::prelude::rust_2021::Eq];
    let Hash = quote![::core::hash::Hash];
    let Hasher = quote![::core::hash::Hasher];
    let Option = quote![::core::prelude::rust_2021::Option];
    let Sized = quote![::core::prelude::rust_2021::Sized];
    let Ord = quote![::core::prelude::rust_2021::Ord];
    let PartialEq = quote![::core::prelude::rust_2021::PartialEq];
    let PartialOrd = quote![::core::prelude::rust_2021::PartialOrd];
    let Ordering = quote![::core::cmp::Ordering];
    let Send = quote![::core::marker::Send];
    let Sync = quote![::core::marker::Sync];

    let target_ident = ident;
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let target_type = quote![#target_ident #type_generics];

    Ok(quote! {
        // SAFETY: forwarding to an existing implementation
        unsafe impl #impl_generics #Id for #target_type #where_clause {
            type Base = <#inner as #Id>::Base;
            type Generic = <#inner as #Id>::Generic;

            const MAX_INDEX: #usize = <#inner as #Id>::MAX_INDEX;
            const MIN: Self = Self { #field: <#inner as #Id>::MIN #extra_init };
            const MAX: Self = Self { #field: <#inner as #Id>::MAX #extra_init };

            #[inline(always)]
            unsafe fn from_index_unchecked(index: #usize) -> Self {
                // SAFETY: forwarding to an existing implementation
                Self {
                    #field: unsafe { <#inner as #Id>::from_index_unchecked(index) }
                    #extra_init
                }
            }

            #[inline(always)]
            fn from_index(index: #usize) -> Self {
                Self { #field: <#inner as #Id>::from_index(index) #extra_init }
            }

            #[inline(always)]
            fn try_from_index(index: #usize) -> #Option<Self> {
                Some(Self { #field: <#inner as #Id>::try_from_index(index)? #extra_init })
            }

            #[inline(always)]
            fn index(self) -> #usize {
                let Self { #field: inner #extra_init } = self;
                inner.index()
            }
        }

        // SAFETY: we ensure that the newtype wrapped type is `Id` and thus `Send`
        unsafe impl #impl_generics #Send for #target_type #where_clause {
        }

        // SAFETY: we ensure that the newtype wrapped type is `Id` and thus `Sync`
        unsafe impl #impl_generics #Sync for #target_type #where_clause {
        }

        impl #impl_generics #Copy for #target_type #where_clause {
        }

        impl #impl_generics #Clone for #target_type #where_clause {
            #[inline(always)]
            fn clone(&self) -> Self {
                *self
            }
        }

        impl #impl_generics #PartialEq for #target_type #where_clause {
            #[inline(always)]
            fn eq(&self, other: &Self) -> #bool {
                #PartialEq::eq(&self.#field, &other.#field)
            }
        }

        impl #impl_generics #Eq for #target_type #where_clause {}

        impl #impl_generics #Hash for #target_type #where_clause {
            #[inline(always)]
            fn hash<H: #Hasher>(&self, state: &mut H) {
                #Hash::hash(&self.#field, state)
            }

            #[inline(always)]
            fn hash_slice<H: #Hasher>(data: &[Self], state: &mut H)
            where
                Self: #Sized,
            {
                // SAFETY: repr(transparent) cast
                let data = unsafe {&*(data as *const [Self] as *const [#inner])};
                <#inner as #Hash>::hash_slice(data, state)
            }
        }

        impl #impl_generics #PartialOrd for #target_type #where_clause {
            #[inline(always)]
            fn partial_cmp(&self, other: &Self) -> #Option<#Ordering> {
                Some(#Ord::cmp(self, other))
            }

            #[inline(always)]
            fn lt(&self, other: &Self) -> bool {
                #PartialOrd::lt(&self.#field, &other.#field)
            }

            #[inline(always)]
            fn le(&self, other: &Self) -> bool {
                #PartialOrd::le(&self.#field, &other.#field)
            }

            #[inline(always)]
            fn gt(&self, other: &Self) -> bool {
                #PartialOrd::gt(&self.#field, &other.#field)
            }

            #[inline(always)]
            fn ge(&self, other: &Self) -> bool {
                #PartialOrd::ge(&self.#field, &other.#field)
            }
        }

        impl #impl_generics #Ord for #target_type #where_clause {
            #[inline(always)]
            fn cmp(&self, other: &Self) -> #Ordering {
                #Ord::cmp(&self.#field, &other.#field)
            }

            #[inline(always)]
            fn max(self, other: Self) -> Self
            where
                Self: #Sized,
            {
                Self { #field: #Ord::max(self.#field, other.#field) #extra_init }
            }

            #[inline(always)]
            fn min(self, other: Self) -> Self
            where
                Self: #Sized,
            {
                Self { #field: #Ord::min(self.#field, other.#field) #extra_init }
            }

            #[inline(always)]
            fn clamp(self, min: Self, max: Self) -> Self
            where
                Self: #Sized,
                Self: #PartialOrd,
            {
                Self { #field: #Ord::clamp(self.#field, min.#field, max.#field) #extra_init }
            }
        }
    })
}