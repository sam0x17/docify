use derive_syn_parse::Parse;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::{env, fs};
use syn::{
    parse2,
    spanned::Spanned,
    visit::{self, Visit},
    AttrStyle, Attribute, Error, File, Ident, Item, LitStr, Result, Token,
};

/// Gets a copy of the inherent name ident of an [`Item`], if applicable.
fn name_ident(item: &Item) -> Option<Ident> {
    match item {
        Item::Const(item_const) => Some(item_const.ident.clone()),
        Item::Enum(item_enum) => Some(item_enum.ident.clone()),
        Item::ExternCrate(item_extern_crate) => Some(item_extern_crate.ident.clone()),
        Item::Fn(item_fn) => Some(item_fn.sig.ident.clone()),
        Item::Macro(item_macro) => item_macro.ident.clone(), // note this one might not have an Ident as well
        Item::Mod(item_mod) => Some(item_mod.ident.clone()),
        Item::Static(item_static) => Some(item_static.ident.clone()),
        Item::Struct(item_struct) => Some(item_struct.ident.clone()),
        Item::Trait(item_trait) => Some(item_trait.ident.clone()),
        Item::TraitAlias(item_trait_alias) => Some(item_trait_alias.ident.clone()),
        Item::Type(item_type) => Some(item_type.ident.clone()),
        Item::Union(item_union) => Some(item_union.ident.clone()),
        // Item::ForeignMod(item_foreign_mod) => None,
        // Item::Use(item_use) => None,
        // Item::Impl(item_impl) => None,
        // Item::Verbatim(_) => None,
        _ => None,
    }
}

/// Gets a copy of any attributes associated with this [`Item`], if applicable.
fn item_attributes(item: &Item) -> &Vec<Attribute> {
    const EMPTY: &'static Vec<Attribute> = &Vec::new();
    match item {
        Item::Const(c) => &c.attrs,
        Item::Enum(e) => &e.attrs,
        Item::ExternCrate(e) => &e.attrs,
        Item::Fn(f) => &f.attrs,
        Item::ForeignMod(f) => &f.attrs,
        Item::Impl(i) => &i.attrs,
        Item::Macro(m) => &m.attrs,
        Item::Mod(m) => &m.attrs,
        Item::Static(s) => &s.attrs,
        Item::Struct(s) => &s.attrs,
        Item::Trait(t) => &t.attrs,
        Item::TraitAlias(t) => &t.attrs,
        Item::Type(t) => &t.attrs,
        Item::Union(u) => &u.attrs,
        Item::Use(u) => &u.attrs,
        _ => &EMPTY,
    }
}
fn set_item_attributes(item: &mut Item, attrs: Vec<Attribute>) {
    match item {
        Item::Const(c) => c.attrs = attrs,
        Item::Enum(e) => e.attrs = attrs,
        Item::ExternCrate(e) => e.attrs = attrs,
        Item::Fn(f) => f.attrs = attrs,
        Item::ForeignMod(f) => f.attrs = attrs,
        Item::Impl(i) => i.attrs = attrs,
        Item::Macro(m) => m.attrs = attrs,
        Item::Mod(m) => m.attrs = attrs,
        Item::Static(s) => s.attrs = attrs,
        Item::Struct(s) => s.attrs = attrs,
        Item::Trait(t) => t.attrs = attrs,
        Item::TraitAlias(t) => t.attrs = attrs,
        Item::Type(t) => t.attrs = attrs,
        Item::Union(u) => u.attrs = attrs,
        Item::Use(u) => u.attrs = attrs,
        _ => unimplemented!(),
    }
}

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
        None => match name_ident(&item) {
            Some(ident) => ident,
            None => {
                return Err(Error::new(
                    item.span(),
                    "Cannot automatically detect ident from this item. \
				    You will need to specify a name manually as the argument \
				    for the #[export] attribute, i.e. #[export(my_name)].",
                ))
            }
        },
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

fn into_example(st: String, ignore: bool) -> String {
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

struct ItemVisitor {
    search: Ident,
    results: Vec<Item>,
}

impl<'ast> Visit<'ast> for ItemVisitor {
    fn visit_item(&mut self, node: &'ast Item) {
        let mut i = 0;
        let attrs = item_attributes(&node);
        for attr in attrs {
            i += 1; // note, 1-based
            let AttrStyle::Outer = attr.style else { continue };
            let Some(last_seg) = attr.path().segments.last() else { continue };
            if last_seg.ident != "export" {
                continue;
            }
            let Some(second_to_last_seg) = attr.path().segments.iter().rev().nth(1) else { continue };
            if second_to_last_seg.ident != last_seg.ident && second_to_last_seg.ident != "docify" {
                continue;
            }
            // we have found a #[something::docify::export] or #[docify::export] or
            // #[export]-style attribute
            let item_ident = match attr.meta.path().get_ident() {
                Some(ident) => ident.clone(),
                None => match name_ident(&node) {
                    Some(ident) => ident,
                    None => continue,
                },
            };
            // check if this ident matches the one we're searching for
            if item_ident == self.search {
                let mut item = node.clone();
                // modify item's attributes to not include this one so this one is excluded
                // from the code example
                let attrs_without_this_one: Vec<Attribute> = attrs
                    .iter()
                    .enumerate()
                    .filter(|&(n, _)| n != i - 1)
                    .map(|(_, v)| v)
                    .cloned()
                    .collect();
                set_item_attributes(&mut item, attrs_without_this_one);
                // add the item to results
                self.results.push(item);
                // no need to explore the attributes of this item further, it is already in results
                break;
            }
        }
        visit::visit_item(self, node);
    }
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
    let source_file = parse2::<File>(parsed)?;
    let ignore = true;

    let output = if let Some(ident) = args.item_ident {
        let mut visitor = ItemVisitor {
            search: ident.clone(),
            results: Vec::new(),
        };
        visitor.visit_file(&source_file);
        if visitor.results.is_empty() {
            return Err(Error::new(
                ident.span(),
                format!(
                    "Could not find docify export item '{}' in '{}'.",
                    ident.to_string(),
                    args.file_path.value()
                ),
            ));
        }
        let results: Vec<String> = visitor
            .results
            .iter()
            .map(|r| into_example(r.to_token_stream().to_string(), ignore))
            .collect();
        results.join("\n")
    } else {
        into_example(source_code, ignore)
    };

    Ok(quote!(#output))
}

#[cfg(test)]
mod tests;
