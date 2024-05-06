use proc_macro2::TokenStream;
use quote::quote;
use syn::DeriveInput;

use crate::resolve_crate;

pub fn real_derive_id(input: DeriveInput) -> syn::Result<TokenStream> {
    #![allow(non_snake_case)]

    let DeriveInput {
        attrs,
        ident,
        generics,
        data,
        ..
    } = input;

    let mut repr_transparent_found = false;

    for attr in attrs {
        if attr.meta.path().is_ident("repr") {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("transparent") {
                    repr_transparent_found = true;
                } else {
                    bail!(
                        &attr,
                        "deriving `Id` is only supported for `#[repr(transparent)]` structs"
                    );
                }
                Ok(())
            })?;
        }
    }

    if !repr_transparent_found {
        bail!(&ident, "`derive(Id)` requries `#[repr(transparent)]`");
    }

    let data_struct = match data {
        syn::Data::Struct(data_struct) => data_struct,
        syn::Data::Enum(data_enum) => {
            bail!(
                data_enum.enum_token,
                "can only derive `Id` for `struct` types"
            );
        }
        syn::Data::Union(data_union) => {
            bail!(
                data_union.union_token,
                "can only derive `Id` for `struct` types"
            );
        }
    };

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
    let PhantomData = quote![::core::marker::PhantomData];
    let Send = quote![::core::marker::Send];
    let Sync = quote![::core::marker::Sync];

    let mut fields = data_struct.fields.iter();

    let Some(field_def) = fields.next() else {
        bail!(
            data_struct.fields,
            "can only derive `Id` for structs where the first field's type implements `Id` and any following fields are `PhantomData`"
        );
    };

    let mut extra_init = quote![];

    for field_def in fields {
        if let Some(ident) = &field_def.ident {
            extra_init.extend(quote![, #ident: #PhantomData]);
        } else {
            extra_init.extend(quote![, #PhantomData]);
        }
    }

    let field = if let Some(ident) = &field_def.ident {
        quote![#ident]
    } else {
        quote![0]
    };

    let construct = |inner: TokenStream| {
        if let Some(ident) = &field_def.ident {
            quote![Self { #ident: #inner #extra_init}]
        } else {
            quote![Self(#inner #extra_init)]
        }
    };

    let target_ident = ident;
    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();

    let target_type = quote![#target_ident #type_generics];

    let inner = &field_def.ty;

    let min_def = construct(quote![<#inner as #Id>::MIN]);
    let max_def = construct(quote![<#inner as #Id>::MAX]);

    let from_index_unchecked_body = construct(quote! {
        // SAFETY: forwarding to an existing implementation
        unsafe { <#inner as #Id>::from_index_unchecked(index) }
    });

    let from_index_body = construct(quote! {
        <#inner as #Id>::from_index(index)
    });

    let try_from_index_body = construct(quote! {
        <#inner as #Id>::try_from_index(index)?
    });

    let max_body = construct(quote! {
        #Ord::max(self.#field, other.#field)
    });

    let min_body = construct(quote! {
        #Ord::min(self.#field, other.#field)
    });

    let clamp_body = construct(quote! {
        #Ord::clamp(self.#field, min.#field, max.#field)
    });

    Ok(quote! {
        // SAFETY: forwarding to an existing implementation
        unsafe impl #impl_generics #Id for #target_type #where_clause {
            type Base = <#inner as #Id>::Base;
            type Generic = <#inner as #Id>::Generic;

            const MAX_INDEX: #usize = <#inner as #Id>::MAX_INDEX;
            const MIN: Self = #min_def;
            const MAX: Self = #max_def;

            #[inline(always)]
            unsafe fn from_index_unchecked(index: #usize) -> Self {
                #from_index_unchecked_body
            }

            #[inline(always)]
            fn from_index(index: #usize) -> Self {
                #from_index_body
            }

            #[inline(always)]
            fn try_from_index(index: #usize) -> #Option<Self> {
                Some(#try_from_index_body)
            }

            #[inline(always)]
            fn index(self) -> #usize {
                self.#field.index()
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
                #max_body
            }

            #[inline(always)]
            fn min(self, other: Self) -> Self
            where
                Self: #Sized,
            {
                #min_body
            }

            #[inline(always)]
            fn clamp(self, min: Self, max: Self) -> Self
            where
                Self: #Sized,
                Self: #PartialOrd,
            {
                #clamp_body
            }
        }
    })
}
