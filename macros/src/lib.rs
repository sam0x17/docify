use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::Result;

#[proc_macro_attribute]
pub fn export(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    match export_internal(attr, tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn export_internal(
    _attr: impl Into<TokenStream2>,
    _tokens: impl Into<TokenStream2>,
) -> Result<TokenStream2> {
    Ok(quote!())
}
