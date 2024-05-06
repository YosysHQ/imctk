use proc_macro::TokenStream as MacroTokenStream;
use proc_macro2::{Span, TokenStream};
use proc_macro_crate::FoundCrate;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

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

mod id;

#[proc_macro_derive(Id)]
pub fn derive_id(input: MacroTokenStream) -> MacroTokenStream {
    id::real_derive_id(parse_macro_input!(input as DeriveInput))
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
