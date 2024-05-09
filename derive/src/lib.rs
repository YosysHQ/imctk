use proc_macro::TokenStream as MacroTokenStream;
use proc_macro2::{Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Attribute, Data, DeriveInput, Ident, Member, PathSegment, Type};

macro_rules! bail {
    ($span:expr, $($tokens:tt)*) => {
        {
            return Err(::syn::Error::new_spanned(
                $span,
                format!($($tokens)*)
            ));
        }
    };
}

fn resolve_crate(name: &str) -> TokenStream {
    match proc_macro_crate::crate_name(name) {
        Ok(FoundCrate::Name(name)) => {
            let ident = Ident::new_raw(&name, Span::call_site());
            quote!(:: #ident)
        }
        Ok(FoundCrate::Itself) => {
            quote!(crate)
        }
        _ => {
            let ident = Ident::new_raw(name, Span::call_site());
            quote!(:: #ident)
        }
    }
}

struct ReprTransparent<'a> {
    field: Member,
    field_ty: &'a Type,
    extra_init: TokenStream,
}

fn require_repr_transparent<'a>(
    target: &impl ToTokens,
    name: &str,
    attrs: &[Attribute],
    data: &'a Data,
) -> syn::Result<ReprTransparent<'a>> {
    let mut repr_transparent_found = false;

    for attr in attrs {
        if attr.meta.path().is_ident("repr") {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("transparent") {
                    repr_transparent_found = true;
                } else {
                    bail!(
                        &attr,
                        "deriving `{name}` is only supported for `#[repr(transparent)]` structs"
                    );
                }
                Ok(())
            })?;
        }
    }

    if !repr_transparent_found {
        bail!(target, "`derive({name})` requries `#[repr(transparent)]`");
    }

    let data_struct = match data {
        syn::Data::Struct(data_struct) => data_struct,
        syn::Data::Enum(data_enum) => {
            bail!(
                data_enum.enum_token,
                "can only derive `{name}` for `struct` types"
            );
        }
        syn::Data::Union(data_union) => {
            bail!(
                data_union.union_token,
                "can only derive `{name}` for `struct` types"
            );
        }
    };

    if data_struct.fields.is_empty() {
        bail!(
            target,
            "`derive({name})` requries exactly one non-`PhantomData` field"
        );
    };

    let inner_pos = data_struct
        .fields
        .iter()
        .position(|field| match &field.ty {
            Type::Path(path) => {
                if let Some(PathSegment { ident, .. }) = path.path.segments.last() {
                    ident != "PhantomData"
                } else {
                    true
                }
            }
            _ => true,
        })
        .unwrap_or_default();

    let inner_field_def = data_struct.fields.iter().nth(inner_pos).unwrap();

    let mut extra_init = quote![];

    for (i, field_def) in data_struct.fields.iter().enumerate() {
        if i == inner_pos {
            continue;
        }

        let ident: Member = if let Some(ident) = &field_def.ident {
            ident.clone().into()
        } else {
            i.into()
        };
        extra_init.extend(quote![, #ident: ::core::marker::PhantomData]);
    }

    let field: Member = if let Some(ident) = &inner_field_def.ident {
        ident.clone().into()
    } else {
        inner_pos.into()
    };

    Ok(ReprTransparent {
        field,
        field_ty: &inner_field_def.ty,
        extra_init,
    })
}

mod id;
mod transparent;

#[proc_macro_derive(Id)]
pub fn derive_id(input: MacroTokenStream) -> MacroTokenStream {
    id::derive_id(parse_macro_input!(input as DeriveInput))
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_derive(SubtypeCast)]
pub fn derive_subtype_cast(input: MacroTokenStream) -> MacroTokenStream {
    transparent::derive_subtype_cast(parse_macro_input!(input as DeriveInput))
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_derive(NewtypeCast)]
pub fn derive_newtype_cast(input: MacroTokenStream) -> MacroTokenStream {
    transparent::derive_newtype_cast(parse_macro_input!(input as DeriveInput))
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
