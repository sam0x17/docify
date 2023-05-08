use derive_syn_parse::Parse;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::{env, fs};
use syn::{parse2, spanned::Spanned, Error, File, Ident, Item, LitStr, Result, Token};

#[proc_macro_attribute]
pub fn export(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    match export_internal(attr, tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[derive(Parse)]
struct ExportAttr {
    ident: Option<Ident>,
}

fn export_internal(
    attr: impl Into<TokenStream2>,
    tokens: impl Into<TokenStream2>,
) -> Result<TokenStream2> {
    let attr = parse2::<ExportAttr>(attr.into())?;
    let item = parse2::<Item>(tokens.into())?;

    // get export ident
    let _export_ident = match attr.ident {
        Some(ident) => ident,
        None => {
            let ident = match item.clone() {
                Item::Const(item_const) => Some(item_const.ident),
                Item::Enum(item_enum) => Some(item_enum.ident),
                Item::ExternCrate(item_extern_crate) => Some(item_extern_crate.ident),
                Item::Fn(item_fn) => Some(item_fn.sig.ident),
                Item::Macro(item_macro) => item_macro.ident, // note this one might not have an Ident as well
                Item::Mod(item_mod) => Some(item_mod.ident),
                Item::Static(item_static) => Some(item_static.ident),
                Item::Struct(item_struct) => Some(item_struct.ident),
                Item::Trait(item_trait) => Some(item_trait.ident),
                Item::TraitAlias(item_trait_alias) => Some(item_trait_alias.ident),
                Item::Type(item_type) => Some(item_type.ident),
                Item::Union(item_union) => Some(item_union.ident),
                // Item::ForeignMod(item_foreign_mod) => None,
                // Item::Use(item_use) => None,
                // Item::Impl(item_impl) => None,
                // Item::Verbatim(_) => None,
                _ => None,
            };
            match ident {
                Some(ident) => ident,
                None => {
                    return Err(Error::new(
                        item.span(),
                        "Cannot automatically detect ident from this item. \
				        You will need to specify a name manually as the argument \
						for the #[export] attribute, i.e. #[export(as my_name)].",
                    ))
                }
            }
        }
    };

    Ok(quote!(#item))
}

#[proc_macro]
pub fn embed(tokens: TokenStream) -> TokenStream {
    match embed_internal(tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[derive(Parse)]
struct EmbedArgs {
    file_path: LitStr,
    #[prefix(Option<Token![,]> as comma)]
    #[parse_if(comma.is_some())]
    item_ident: Option<Ident>,
}

fn into_doc_comments(st: String, ignore: bool) -> String {
    let mut lines: Vec<String> = Vec::new();
    if ignore {
        lines.push(String::from("```ignore"));
    } else {
        lines.push(String::from("```"));
    }
    for line in st.lines() {
        lines.push(String::from(line));
    }
    lines.push(String::from("```"));
    lines.join("\n")
}

fn embed_internal(tokens: impl Into<TokenStream2>) -> Result<TokenStream2> {
    let args = parse2::<EmbedArgs>(tokens.into())?;
    let source_code = match fs::read_to_string(args.file_path.value()) {
        Ok(src) => src,
        Err(_) => {
            return Err(Error::new(
                args.file_path.span(),
                format!(
                    "Could not read the specified path '{}' relative to '{}'.",
                    args.file_path.value(),
                    env::current_dir()
                        .expect("Could not read current directory!")
                        .display()
                ),
            ))
        }
    };
    let parsed = source_code.parse::<TokenStream2>()?;
    let item_file = parse2::<File>(parsed)?;

    let source_code = into_doc_comments(source_code, true);

    Ok(quote!(#source_code))
}

#[cfg(test)]
mod tests;
