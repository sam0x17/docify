//! This crate contains the proc macros used by [docify](https://crates.io/crates/docify).

use common_path::common_path;
use derive_syn_parse::Parse;
use once_cell::sync::Lazy;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use regex::Regex;
use std::{
    cmp::min,
    collections::HashMap,
    fs::{self, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
    str::FromStr,
};
use syn::{
    parse2,
    spanned::Spanned,
    token::Paren,
    visit::{self, Visit},
    AttrStyle, Attribute, Error, File, Ident, ImplItem, Item, LitStr, Meta, Result, Token,
    TraitItem,
};
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use toml::{Table, Value};
use walkdir::WalkDir;

fn line_start_position<S: AsRef<str>>(source: S, pos: usize) -> usize {
    let source = source.as_ref();
    if source.len() <= pos {
        panic!(
            "The specified position ({}) is longer than the source string length ({}).",
            pos,
            source.len()
        );
    }
    let mut cursor = 0;
    for line in source.lines() {
        cursor += line.len();
        if cursor > pos {
            return cursor - line.len();
        }
        cursor += 1; // '\n'
    }
    unreachable!()
}

fn fix_leading_indentation<S: AsRef<str>>(source: S) -> String {
    let source = source.as_ref();
    let mut shared_indent: Option<usize> = None;

    for line in source.lines() {
        if line.trim().is_empty() {
            continue; // Skip whitespace-only or empty lines
        }
        let prefix = &line[..(line.len() - line.trim_start().len())];
        if let Some(shared) = shared_indent {
            shared_indent = Some(std::cmp::min(prefix.len(), shared));
        } else {
            shared_indent = Some(prefix.len());
        }
    }

    let shared_indent = shared_indent.unwrap_or(0);
    let mut output_lines = source
        .lines()
        .map(|line| {
            if line.len() >= shared_indent {
                line[shared_indent..].to_string()
            } else {
                line.to_string()
            }
        })
        .collect::<Vec<String>>();

    // Add trailing newline if the source had it
    if source.ends_with('\n') {
        output_lines.push("".to_string());
    }

    output_lines.join("\n")
}

fn fix_indentation<S: AsRef<str>>(source: S) -> String {
    let source = source.as_ref();
    // let source = fix_first_line_indentation(source);
    let source = fix_leading_indentation(source);
    source
}

fn caller_crate_root() -> Option<PathBuf> {
    let crate_name =
        std::env::var("CARGO_PKG_NAME").expect("failed to read ENV var `CARGO_PKG_NAME`!");
    let current_dir = PathBuf::from(
        std::env::var("CARGO_MANIFEST_DIR").expect("failed to read ENV var `CARGO_MANIFEST_DIR`!"),
    );
    for entry in WalkDir::new(&current_dir)
        .into_iter()
        .filter_entry(|e| !e.file_name().eq_ignore_ascii_case("target"))
    {
        let Ok(entry) = entry else { continue };
        if !entry.file_type().is_file() {
            continue;
        }
        let Some(file_name) = entry.path().file_name() else {
            continue;
        };
        if !file_name.eq_ignore_ascii_case("Cargo.toml") {
            continue;
        }
        let Ok(cargo_toml) = std::fs::read_to_string(&entry.path()) else {
            continue;
        };
        let Ok(table) = Table::from_str(cargo_toml.as_str()) else {
            continue;
        };
        let Some(package) = table.get("package") else {
            continue;
        };
        let Some(Value::String(package_name)) = package.get("name") else {
            continue;
        };
        if package_name.eq_ignore_ascii_case(&crate_name) {
            return Some(entry.path().parent().unwrap().to_path_buf());
        }
    }
    None
}

/// Prettifies a long path so that leading segments other than the crate root are ignored
///
/// NOTE: unwraps [`caller_crate_root`], as you would only use this if that has already
/// evaluated to a non-`None` value.
fn prettify_path<P: AsRef<Path>>(path: P) -> PathBuf {
    let path = path.as_ref();
    if path.is_relative() {
        return path.into();
    }
    let Some(prefix) = common_path(caller_crate_root().unwrap(), path) else {
        return path.into();
    };
    path.components()
        .skip(prefix.components().collect::<Vec<_>>().len())
        .collect::<PathBuf>()
}

const DOCIFYING: &'static str = "   Docifying ";

/// Tries to write the specified string to the terminal in green+bold. Falls back to normal
/// `print!()`. Function is infallible.
fn write_green<S: AsRef<str>>(st: S) {
    let mut stdout = StandardStream::stdout(ColorChoice::Always);
    let _ = stdout.set_color(ColorSpec::new().set_fg(Some(Color::Green)).set_bold(true));
    if let Err(_) = write!(&mut stdout, "{}", st.as_ref()) {
        print!("{}", st.as_ref());
    }
    let _ = stdout.set_color(ColorSpec::new().set_fg(None).set_bold(false));
}

/// An item that may or may not have an inherent "name" ident.
trait NamedItem {
    /// Gets a copy of the inherent name ident of this item, if applicable.
    fn name_ident(&self) -> Option<Ident>;
}

impl NamedItem for Item {
    fn name_ident(&self) -> Option<Ident> {
        match self {
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
}

impl NamedItem for ImplItem {
    fn name_ident(&self) -> Option<Ident> {
        match self {
            ImplItem::Const(impl_item_const) => Some(impl_item_const.ident.clone()),
            ImplItem::Fn(impl_item_fn) => Some(impl_item_fn.sig.ident.clone()),
            ImplItem::Type(impl_item_type) => Some(impl_item_type.ident.clone()),
            // ImplItem::Macro(impl_item_macro) => None,
            // ImplItem::Verbatim(impl_item_verbatim) => None,
            _ => None,
        }
    }
}

impl NamedItem for TraitItem {
    fn name_ident(&self) -> Option<Ident> {
        match self {
            TraitItem::Const(trait_item_const) => Some(trait_item_const.ident.clone()),
            TraitItem::Fn(trait_item_fn) => Some(trait_item_fn.sig.ident.clone()),
            TraitItem::Type(trait_item_type) => Some(trait_item_type.ident.clone()),
            // TraitItem::Macro(trait_item_macro) => None,
            // TraitItem::Verbatim(trait_item_verbatim) => None,
            _ => None,
        }
    }
}

/// Generalizes over items that have some underlying set of [`Attribute`] associated with them.
trait AttributedItem {
    /// Gets a reference to the underlying [`Vec`] of [`Attribute`]s for this item, if
    /// applicable. If not applicable, an empty [`Vec`] will be returned.
    fn item_attributes(&self) -> &Vec<Attribute>;

    /// Sets the underlying [`Vec`] of [`Attribute`]s for this item, if applicable. Panics if
    /// you attempt to use this on an inapplicable item!
    fn set_item_attributes(&mut self, attrs: Vec<Attribute>);
}

impl AttributedItem for Item {
    fn item_attributes(&self) -> &Vec<Attribute> {
        const EMPTY: &Vec<Attribute> = &Vec::new();
        match self {
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
            _ => EMPTY,
        }
    }

    fn set_item_attributes(&mut self, attrs: Vec<Attribute>) {
        match self {
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
}

impl AttributedItem for ImplItem {
    fn item_attributes(&self) -> &Vec<Attribute> {
        const EMPTY: &Vec<Attribute> = &Vec::new();
        match self {
            ImplItem::Const(impl_item_const) => &impl_item_const.attrs,
            ImplItem::Fn(impl_item_fn) => &impl_item_fn.attrs,
            ImplItem::Type(impl_item_type) => &impl_item_type.attrs,
            ImplItem::Macro(impl_item_macro) => &impl_item_macro.attrs,
            // ImplItem::Verbatim(impl_item_verbatim) => &EMPTY,
            _ => &EMPTY,
        }
    }

    fn set_item_attributes(&mut self, attrs: Vec<Attribute>) {
        match self {
            ImplItem::Const(impl_item_const) => impl_item_const.attrs = attrs,
            ImplItem::Fn(impl_item_fn) => impl_item_fn.attrs = attrs,
            ImplItem::Type(impl_item_type) => impl_item_type.attrs = attrs,
            ImplItem::Macro(impl_item_macro) => impl_item_macro.attrs = attrs,
            // ImplItem::Verbatim(impl_item_verbatim) => unimplemented!(),
            _ => unimplemented!(),
        }
    }
}

impl AttributedItem for TraitItem {
    fn item_attributes(&self) -> &Vec<Attribute> {
        const EMPTY: &Vec<Attribute> = &Vec::new();
        match self {
            TraitItem::Const(trait_item_const) => &trait_item_const.attrs,
            TraitItem::Fn(trait_item_fn) => &trait_item_fn.attrs,
            TraitItem::Type(trait_item_type) => &trait_item_type.attrs,
            TraitItem::Macro(trait_item_macro) => &trait_item_macro.attrs,
            // TraitItem::Verbatim(trait_item_verbatim) => &EMPTY,
            _ => &EMPTY,
        }
    }

    fn set_item_attributes(&mut self, attrs: Vec<Attribute>) {
        match self {
            TraitItem::Const(trait_item_const) => trait_item_const.attrs = attrs,
            TraitItem::Fn(trait_item_fn) => trait_item_fn.attrs = attrs,
            TraitItem::Type(trait_item_type) => trait_item_type.attrs = attrs,
            TraitItem::Macro(trait_item_macros) => trait_item_macros.attrs = attrs,
            // TraitItem::Verbatim(trait_item_verbatim) => unimplemented!(),
            _ => unimplemented!(),
        }
    }
}

/// Marks an item for export, making it available for embedding as a rust doc example via
/// [`docify::embed!(..)`](`macro@embed`) or [`docify::embed_run!(..)`](`macro@embed_run`).
///
/// By default, you can just call the attribute with no arguments like the following:
/// ```ignore
/// #[docify::export]
/// mod some_item {
///     fn some_func() {
///         println!("hello world");
///     }
/// }
/// ```
///
/// When you [`docify::embed!(..)`](`macro@embed`) this item, you will have to refer to it by
/// the primary ident associated with the item, in this case `some_item`. In some cases, such
/// as with `impl` statements, there is no clear main ident. You should handle these situations
/// by specifying an ident manually (not doing so will result in a compile error):
/// ```ignore
/// #[docify::export(some_name)]
/// impl SomeTrait for Something {
///     // ...
/// }
/// ```
///
/// You are also free to specify an alternate export name for items that _do_ have a clear
/// ident if you need/want to:
/// ```ignore
/// #[docify::export(SomeName)]
/// fn hello_world() {
///     println!("hello");
///     println!("world");
/// }
/// ```
///
/// When you go to [`docify::embed!(..)`](`macro@embed`) or
/// [`docify::embed_run!(..)`](`macro@embed_run`) such an item, you must refer to it by
/// `SomeName` (in this case), or whatever name you provided to `#[docify::export]`.
///
/// There is no guard to prevent duplicate export names in the same file, and export names are
/// all considered within the global namespace of the file in question (they do not exist
/// inside a particular module or scope within a source file). When using
/// [`docify::embed!(..)`](`macro@embed`), duplicate results are simply embedded one after
/// another, and this is by design.
///
/// If there are multiple items with the same inherent name in varipous scopes in the same
/// file, and you want to export just one of them as a doc example, you should specify a unique
/// ident as the export name for this item.
///
/// Note that if you wish to embed an _entire_ file, you don't need `#[docify::export]` at all
/// and can instead specify just a path to [`docify::embed!(..)`](`macro@embed`) or
/// [`docify::embed_run!(..)`](`macro@embed_run`).
#[proc_macro_attribute]
pub fn export(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    match export_internal(attr, tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Like [`#[docify::export]`](`macro@export`) but only exports the inner contents of whatever
/// item the attribute is attached to.
///
/// For example, given the following:
/// ```ignore
/// #[docify::export_content]
/// mod my_mod {
///     pub fn some_fun() {
///         println!("hello world!");
///     }
/// }
/// ```
///
/// only this part would be exported:
/// ```ignore
/// pub fn some_fun() {
///     println!("hello world");
/// }
/// ```
///
/// Note that if [`#[docify::export_content]`](`macro@export_content`) is used on an item that
/// has no notion of inner contents, such as a type, static, or const declaration, it will
/// simply function like a regular [`#[docify::export]`](`macro@export`) attribute.
///
/// Supported items include:
/// - functions
/// - modules
/// - trait declarations
/// - trait impls
/// - basic blocks (when inside an outer macro pattern)
///
/// All other items will behave like they normally do with
/// [`#[docify::export]`](`macro@export`). Notably this includes structs and enums, because
/// while these items have a defined notion of "contents", those contents cannot stand on their
/// own as valid rust code.
#[proc_macro_attribute]
pub fn export_content(attr: TokenStream, tokens: TokenStream) -> TokenStream {
    match export_internal(attr, tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Used to parse args for `#[export(..)]`
#[derive(Parse)]
struct ExportAttr {
    ident: Option<Ident>,
}

/// Internal implementation for `#[export]`
fn export_internal(
    attr: impl Into<TokenStream2>,
    tokens: impl Into<TokenStream2>,
) -> Result<TokenStream2> {
    let attr = parse2::<ExportAttr>(attr.into())?;
    let item = parse2::<Item>(tokens.into())?;

    // get export ident
    let _export_ident = attr.ident.or_else(|| item.name_ident()).ok_or_else(|| {
        Error::new(
            item.span(),
            "Cannot automatically detect ident from this item. \
            You will need to specify a name manually as the argument \
            for the #[export] attribute, i.e. #[export(my_name)].",
        )
    })?;

    Ok(quote!(#item))
}

/// Embeds the specified item from the specified source file in a rust doc example, with pretty
/// formatting enabled.
///
/// Should be used in a `#[doc = ...]` statement, like the following:
///
/// ```ignore
/// /// some doc comments here
/// #[doc = docify::embed!("path/to/file.rs", my_example)]
/// /// more doc comments
/// struct DocumentedItem;
/// ```
///
/// Which will expand to the `my_example` item in `path/to/file.rs` being embedded in a rust
/// doc example marked with `ignore`. If you want to have your example actually run in rust
/// docs as well, you should use [`docify::embed_run!(..)`](`macro@embed_run`).
///
/// ### Arguments
/// - `source_path`: the file path (relative to the current crate root) that contains the item
///   you would like to embed, represented as a string literal. If you wish to embed an entire
///   file, simply specify only a `source_path` with no other arguments and the entire file
///   will be embedded as a doc example. If the path cannot be read for whatever reason, a
///   compile error will be issued. The `source_path` _does  not_ have to be a file that is
///   part of the current compilation unit, though typically it should be. The only requirement
///   is that it must contain valid Rust source code, and must be a descendant of the current
///   crate's root directory. While embedding files from a parent directory of the current
///   crate may work locally, this will fail when you go to deploy to `crates.io` and/or
///   `docs.rs`, so you should not use `../` or similar means unless you plan to never deploy
///   to these services.
/// - `item_ident`: (optional) can be specified after `source_path`, preceded by a comma. This
///   should match the export name you used to [`#[docify::export(..)]`](`macro@export`) the
///   item, or, if no export name was specified, this should match the inherent ident/name of
///   the item. If the item cannot be found, a compile error will be issued. As mentioned
///   above, if no `item_ident` is specified, the entire file will be embedded as an example.
///
/// All items in the `source_file` exist in the same global scope when they are exported for
/// embedding. Special care must be taken with how you
/// [`#[docify::export(..)]`](`macro@export`) items in order to get the item you want.
///
/// If there multiple items in a file that resolve to the same `item_ident` (whether as an
/// inherent ident name or as a manually specified `item_ident`), and you embed using this
/// ident, all matching items will be embedded, one after another, listed in the order that
/// they appear in the `source_file`.
///
/// Here is an example of embedding an _entire_ source file as an example:
/// ```ignore
/// /// Here is a cool example module:
/// #[doc = docify::embed!("examples/my_example.rs")]
/// struct DocumentedItem
/// ```
///
/// You are also free to embed multiple examples in the same set of doc comments:
/// ```ignore
/// /// Example 1:
/// #[doc = docify::embed!("examples/example_1.rs")]
/// /// Example 2:
/// #[doc = docify::embed!("examples/example_2.rs")]
/// /// More docs
/// struct DocumentedItem;
/// ```
///
/// Note that all examples generated by `docify::embed!(..)` are set to `ignore` by default,
/// since they are typically already functioning examples or tests elsewhere in the project,
/// and so they do not need to be run as well in the context where they are being embedded. If
/// for whatever reason you _do_ want to also run an embedded example as a doc example, you can
/// use [`docify::embed_run!(..)`](`macro@embed_run`) which removes the `ignore` tag from the
/// generated example but otherwise functions exactly like `#[docify::embed!(..)]` in every
/// way.
///
/// Output should match `rustfmt` output exactly.
#[proc_macro]
pub fn embed(tokens: TokenStream) -> TokenStream {
    match embed_internal(tokens, MarkdownLanguage::Ignore) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Exactly like [`docify::embed!(..)`](`macro@embed`) in every way _except_ the generated
/// examples are also run automatically as rust doc examples (`ignore` is not included).
///
/// Other than this fact all of the usual docs and syntax and behaviors for
/// [`docify::embed!(..)`](`macro@embed`) also apply to this macro.
#[proc_macro]
pub fn embed_run(tokens: TokenStream) -> TokenStream {
    match embed_internal(tokens, MarkdownLanguage::Blank) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

/// Used to parse args for `docify::embed!(..)`
#[derive(Parse)]
struct EmbedArgs {
    file_path: LitStr,
    #[prefix(Option<Token![,]> as comma)]
    #[parse_if(comma.is_some())]
    item_ident: Option<Ident>,
}

impl ToTokens for EmbedArgs {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(self.file_path.to_token_stream());
        let Some(item_ident) = &self.item_ident else {
            return;
        };
        tokens.extend(quote!(,));
        tokens.extend(item_ident.to_token_stream());
    }
}

mod keywords {
    use syn::custom_keyword;

    custom_keyword!(docify);
    custom_keyword!(embed);
}

/// Used to parse a full `docify::embed!(..)` call, as seen in markdown documents and other
/// embedded settings
#[derive(Parse)]
struct EmbedCommentCall {
    #[prefix(keywords::docify)]
    #[prefix(Token![::])]
    #[prefix(keywords::embed)]
    #[prefix(Token![!])]
    #[paren]
    _paren: Paren,
    #[inside(_paren)]
    args: EmbedArgs,
    _semi: Option<Token![;]>,
}

/// This corresponds with the string immediately following the "```" in codeblocks. Blank means
/// no language is specified. Ignore will cause the example not to run in rust docs.
#[derive(Copy, Clone, Eq, PartialEq)]
enum MarkdownLanguage {
    Ignore,
    Rust,
    Blank,
}

/// Converts a source string to a codeblocks wrapped example
fn into_example(st: &str, lang: MarkdownLanguage) -> String {
    let mut lines: Vec<String> = Vec::new();
    match lang {
        MarkdownLanguage::Ignore => lines.push(String::from("```ignore")),
        MarkdownLanguage::Rust => lines.push(String::from("```rust")),
        MarkdownLanguage::Blank => lines.push(String::from("```")),
    }
    for line in st.lines() {
        lines.push(String::from(line));
    }
    lines.push(String::from("```"));
    lines.join("\n")
}

/// Generalizes over items that we support exporting via Docify, used by [`ItemVisitor`].
trait SupportedVisitItem<'ast> {
    fn visit_supported_item<T: NamedItem + AttributedItem + ToTokens + Clone>(
        &mut self,
        node: &'ast T,
    );
}

impl<'ast> SupportedVisitItem<'ast> for ItemVisitor {
    fn visit_supported_item<T: NamedItem + AttributedItem + ToTokens + Clone>(
        &mut self,
        node: &'ast T,
    ) {
        let mut i = 0;
        let attrs = node.item_attributes();
        for attr in attrs {
            i += 1; // note, 1-based
            let AttrStyle::Outer = attr.style else {
                continue;
            };
            let Some(last_seg) = attr.path().segments.last() else {
                continue;
            };
            let is_export_content = last_seg.ident == "export_content";
            if last_seg.ident != "export" && !is_export_content {
                continue;
            }
            let Some(second_to_last_seg) = attr.path().segments.iter().rev().nth(1) else {
                continue;
            };
            if second_to_last_seg.ident != last_seg.ident && second_to_last_seg.ident != "docify" {
                continue;
            }
            // we have found a #[something::docify::export] or #[docify::export] or
            // #[export]-style attribute
            // (OR any of the above but export_content)

            // resolve item_ident
            let item_ident = match &attr.meta {
                Meta::List(list) => match parse2::<Ident>(list.tokens.clone()) {
                    Ok(ident) => Some(ident),
                    Err(_) => None,
                },
                _ => None,
            };
            let item_ident = match item_ident {
                Some(ident) => ident,
                None => match node.name_ident() {
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
                item.set_item_attributes(attrs_without_this_one);
                // add the item to results
                self.results.push((
                    item.to_token_stream(),
                    match is_export_content {
                        true => ResultStyle::ExportContent,
                        false => ResultStyle::Export,
                    },
                ));
                // no need to explore the attributes of this item further, it is already in results
                break;
            }
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum ResultStyle {
    Export,
    ExportContent,
}

/// Visitor pattern for finding items
struct ItemVisitor {
    search: Ident,
    results: Vec<(TokenStream2, ResultStyle)>,
}

impl<'ast> Visit<'ast> for ItemVisitor {
    fn visit_trait_item(&mut self, node: &'ast TraitItem) {
        self.visit_supported_item(node);
        visit::visit_trait_item(self, node);
    }

    fn visit_impl_item(&mut self, node: &'ast ImplItem) {
        self.visit_supported_item(node);
        visit::visit_impl_item(self, node);
    }

    fn visit_item(&mut self, node: &'ast Item) {
        self.visit_supported_item(node);
        visit::visit_item(self, node);
    }
}

/// Abstraction for a character that has been transposed/offset from its original position in
/// the original string in which it appeared (i.e. if the string has been compressed in some way)
#[derive(Copy, Clone, Eq, PartialEq)]
struct OffsetChar {
    char: char,
    original_pos: usize,
}

impl OffsetChar {
    fn new(char: char, original_pos: usize) -> OffsetChar {
        OffsetChar { char, original_pos }
    }
}

/// Used to mark an entity within a piece of source code. Used with [`CompressedString`].
#[derive(Clone, PartialEq, Eq)]
struct SourceEntity {
    start: usize,
    end: usize,
}

impl SourceEntity {
    pub fn new(start: usize, end: usize) -> SourceEntity {
        SourceEntity { start, end }
    }

    /// Marks the character positions corresponding with this entity as belonging to this
    /// entity in the enclosing [`CompressedString`].
    pub fn claim(&self, claimed: &mut Vec<bool>) {
        for i in self.start..min(self.end, claimed.len()) {
            claimed[i] = true;
        }
    }

    /// Returns `true` if this entity already appears in the specified claimed vec
    pub fn is_claimed(&self, claimed: &Vec<bool>) -> bool {
        claimed[(self.start + self.end) / 2]
    }

    // pub fn value<'a>(&self, source: &'a String) -> &'a str {
    //     &source.as_str()[self.start..self.end]
    // }

    // pub fn contains(&self, x: usize) -> bool {
    //     x >= self.start && x < self.end
    // }
}

/// Represents a [`String`] that has been compressed in some way, and includes data structures
/// allowing us to map individual characters back to their original positions in the
/// uncompressed version of the [`String`].
struct CompressedString {
    chars: HashMap<usize, OffsetChar>,
    chars_arr: Vec<OffsetChar>,
}

impl CompressedString {
    fn to_string(&self) -> String {
        self.chars_arr.iter().map(|c| c.char).collect()
    }
}

static DOCIFY_ATTRIBUTES: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"\n?\#\[(?:\w+\s*::\s*)*(?:export|export_content)(?:\s*\(\s*(\w+)\s*\))?\]\n?")
        .unwrap()
});

static DOC_COMMENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"///.*").unwrap());
static DOC_COMMENT_ATTR: Lazy<Regex> =
    Lazy::new(|| Regex::new(r#"#\[doc\s*=\s*".*"\s*]"#).unwrap());
static LINE_COMMENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"//.*").unwrap());
static MULTI_LINE_COMMENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"/\*[\s\S]*?\*/").unwrap());
static HTML_COMMENT: Lazy<Regex> = Lazy::new(|| Regex::new(r"<!--[\s\S]*?-->").unwrap());
static MARKDOWN_CODEBLOCK: Lazy<Regex> = Lazy::new(|| Regex::new(r"```[\s\S]*?```").unwrap());
// static ref STRING_LIT: Regex = Regex::new(r#"("([^"\\]|\\[\s\S])*")"#).unwrap();

impl From<&String> for CompressedString {
    fn from(value: &String) -> Self {
        let mut entities: Vec<SourceEntity> = Vec::new();
        let mut claimed: Vec<bool> = vec![false; value.len()];
        for m in DOC_COMMENT.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end());
            entity.claim(&mut claimed);
            entities.push(entity);
        }
        for m in DOC_COMMENT_ATTR.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end());
            if !entity.is_claimed(&claimed) {
                entity.claim(&mut claimed);
                entities.push(entity);
            }
        }
        for m in MULTI_LINE_COMMENT.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end());
            if !entity.is_claimed(&claimed) {
                entity.claim(&mut claimed);
                entities.push(entity);
            }
        }
        for m in LINE_COMMENT.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end());
            if !entity.is_claimed(&claimed) {
                entity.claim(&mut claimed);
                entities.push(entity);
            }
        }
        for m in DOCIFY_ATTRIBUTES.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end());
            if !entity.is_claimed(&claimed) {
                entity.claim(&mut claimed);
                entities.push(entity);
            }
        }
        let mut compressed = CompressedString {
            chars_arr: Vec::new(),
            chars: HashMap::new(),
        };
        let mut cursor = 0;
        let mut byte_index = 0;
        while byte_index < value.len() {
            let current_char = &value[byte_index..].chars().next().unwrap(); // get the current character
            let char_len = current_char.len_utf8(); // get its length in bytes

            if claimed[byte_index] || current_char.is_whitespace() {
                byte_index += char_len;
                continue;
            }
            let oc = OffsetChar::new(*current_char, byte_index);
            compressed.chars.insert(cursor, oc);
            compressed.chars_arr.push(oc);
            cursor += 1;
            byte_index += char_len;
        }

        compressed
    }
}

/// Responsible for retrieving the "contents" of an item, used by `#[docify::export_contents]`
fn get_content_tokens<'a>(item: &'a Item) -> TokenStream2 {
    match item {
        // Item::Const(item_const) => item_const.to_token_stream(),
        // Item::Enum(item_enum) => item_enum.to_token_stream(),
        // Item::ExternCrate(item_extern) => item_extern.to_token_stream(),
        Item::Fn(item_fn) => {
            let mut tokens = TokenStream2::new();
            tokens.extend(item_fn.block.stmts.iter().map(|t| t.to_token_stream()));
            tokens
        }
        Item::ForeignMod(item_mod) => {
            let mut tokens = TokenStream2::new();
            tokens.extend(item_mod.items.iter().map(|t| t.to_token_stream()));
            tokens
        }
        Item::Impl(item_impl) => {
            let mut tokens = TokenStream2::new();
            tokens.extend(item_impl.items.iter().map(|t| t.to_token_stream()));
            tokens
        }
        // Item::Macro(item_macro) => item_macro.to_token_stream(),
        Item::Mod(item_mod) => {
            let Some(content) = &item_mod.content else {
                return item_mod.to_token_stream();
            };
            let mut tokens = TokenStream2::new();
            tokens.extend(content.1.iter().map(|t| t.to_token_stream()));
            tokens
        }
        // Item::Static(item_static) => item_static.to_token_stream(),
        // Item::Struct(item_struct) => item_struct.to_token_stream(),
        Item::Trait(item_trait) => {
            let mut tokens = TokenStream2::new();
            tokens.extend(item_trait.items.iter().map(|t| t.to_token_stream()));
            tokens
        }
        Item::TraitAlias(item_trait_alias) => item_trait_alias.to_token_stream(),
        // Item::Type(item_type) => item_type.to_token_stream(),
        // Item::Union(item_union) => item_union.to_token_stream(),
        // Item::Use(item_use) => item_use.to_token_stream(),
        // Item::Verbatim(item_verbatim) => item_verbatim.to_token_stream(),
        _ => item.to_token_stream(),
    }
}

/// Finds and returns the specified [`Item`] within a source text string and returns the exact
/// source code of that item, without any formatting changes. If span locations are stabilized,
/// this can be removed along with most of the [`CompressedString`] machinery.
fn source_excerpt<'a, T: ToTokens>(
    source: &'a String,
    item: &'a T,
    style: ResultStyle,
) -> Result<String> {
    // note: can't rely on span locations because this requires nightly and/or is otherwise
    // bugged
    let compressed_source = CompressedString::from(source);
    let item_tokens = match style {
        ResultStyle::Export => item.to_token_stream(),
        ResultStyle::ExportContent => get_content_tokens(&parse2::<Item>(item.to_token_stream())?),
    };
    let compressed_item = CompressedString::from(&item_tokens.to_string());
    let compressed_source_string = compressed_source.to_string();
    let compressed_item_string = compressed_item.to_string();
    let Some(found_start) = compressed_source_string.find(compressed_item_string.as_str()) else {
        return Err(Error::new(
            item.span(),
            "You have found a bug in docify! Please submit a new GitHub issue at \
            https://github.com/sam0x17/docify/issues/new?title=%60source_excerpt\
            %60%3A%20can%27t%20find%20item%20in%20source with a sample of the item \
            you are trying to embed.",
        ));
    };
    let start_c = compressed_source.chars[&found_start];
    let start_pos = start_c.original_pos;
    let start_pos = line_start_position(source, start_pos);
    let end_c = compressed_source.chars[&(found_start + compressed_item_string.len() - 1)];
    let end_pos = end_c.original_pos;
    let final_excerpt = &source[start_pos..min(end_pos + 1, source.len())];
    Ok(final_excerpt
        .lines()
        .filter(|line| !(DOCIFY_ATTRIBUTES.is_match(line) && !line.trim().starts_with("//")))
        .collect::<Vec<&str>>()
        .join("\n"))
}

/// Inner version of [`embed_internal`] that just returns the result as a [`String`].
fn embed_internal_str(tokens: impl Into<TokenStream2>, lang: MarkdownLanguage) -> Result<String> {
    let args = parse2::<EmbedArgs>(tokens.into())?;
    // return blank result if we can't properly resolve `caller_crate_root`
    let Some(root) = caller_crate_root() else {
        return Ok(String::from(""));
    };
    let file_path = root.join(args.file_path.value());
    let source_code = match fs::read_to_string(&file_path) {
        Ok(src) => src,
        Err(_) => {
            return Err(Error::new(
                args.file_path.span(),
                format!(
                    "Could not read the specified path '{}'.",
                    file_path.display(),
                ),
            ))
        }
    };
    let parsed = source_code.parse::<TokenStream2>()?;
    let source_file = parse2::<File>(parsed)?;

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
                    ident,
                    file_path.display(),
                ),
            ));
        }
        let mut results: Vec<String> = Vec::new();
        for (item, style) in visitor.results {
            let excerpt = source_excerpt(&source_code, &item, style)?;
            let formatted = fix_indentation(excerpt);
            let example = into_example(formatted.as_str(), lang);
            results.push(example);
        }
        results.join("\n")
    } else {
        into_example(source_code.as_str(), lang)
    };
    Ok(output)
}

/// Internal implementation behind [`macro@embed`].
fn embed_internal(tokens: impl Into<TokenStream2>, lang: MarkdownLanguage) -> Result<TokenStream2> {
    let output = embed_internal_str(tokens, lang)?;
    Ok(quote!(#output))
}

/// Used to parse args for [`macro@compile_markdown`].
#[derive(Parse)]
struct CompileMarkdownArgs {
    input: LitStr,
    #[prefix(Option<Token![,]> as comma)]
    #[parse_if(comma.is_some())]
    output: Option<LitStr>,
}

/// Internal implementation behind [`macro@compile_markdown`].
fn compile_markdown_internal(tokens: impl Into<TokenStream2>) -> Result<TokenStream2> {
    let args = parse2::<CompileMarkdownArgs>(tokens.into())?;
    if args.input.value().is_empty() {
        return Err(Error::new(args.input.span(), "Input path cannot be blank!"));
    }
    let input_path = std::path::PathBuf::from(&args.input.value());
    // return blank result if we can't properly resolve `caller_crate_root`
    let Some(root) = caller_crate_root() else {
        return Ok(quote!());
    };
    let input_path = root.join(input_path);
    if !input_path.exists() {
        return Err(Error::new(
            args.input.span(),
            format!(
                "Could not read the specified path '{}'.",
                input_path.display(),
            ),
        ));
    }
    if let Some(output) = args.output {
        if output.value().is_empty() {
            return Err(Error::new(
                output.span(),
                "If specified, output path cannot be blank!",
            ));
        }
        let output = root.join(output.value());
        if input_path.is_dir() {
            compile_markdown_dir(input_path, format!("{}", output.display()))?;
        } else {
            if cfg!(not(test)) {
                write_green(DOCIFYING);
                println!(
                    "{} {} {}",
                    prettify_path(&input_path).display(),
                    "=>", // TODO: fancy arrow
                    prettify_path(&output).display(),
                );
            }
            let Ok(source) = fs::read_to_string(&input_path) else {
                return Err(Error::new(
                    Span::call_site(),
                    format!("Failed to read markdown file at '{}'", input_path.display()),
                ));
            };
            let compiled = compile_markdown_source(source.as_str())?;
            let Ok(_) = overwrite_file(&output, &compiled) else {
                return Err(Error::new(
                    Span::call_site(),
                    format!("Failed to write to '{}'", output.display()),
                ));
            };
        }
        Ok(quote!())
    } else {
        if input_path.is_dir() {
            return Err(Error::new(
                args.input.span(),
                "Only individual files are supported with no output path, you specified a directory."
            ));
        }
        let Ok(source) = fs::read_to_string(&input_path) else {
            return Err(Error::new(
                Span::call_site(),
                format!("Failed to read markdown file at '{}'", input_path.display()),
            ));
        };
        let compiled = compile_markdown_source(source.as_str())?;
        Ok(quote!(#compiled))
    }
}

/// Takes in a `path` and re-writes it as a subpath in `target_dir`.
fn transpose_subpath<P1: AsRef<Path>, P2: AsRef<Path>, P3: AsRef<Path>>(
    input_dir: P1,
    path: P2,
    target_dir: P3,
) -> PathBuf {
    let prefix = common_path(input_dir, &path).unwrap();
    Path::join(
        target_dir.as_ref(),
        path.as_ref()
            .components()
            .skip(prefix.components().collect::<Vec<_>>().len())
            .collect::<PathBuf>(),
    )
}

/// Overwrites or creates a file at the specified path and populates it with the specified
/// data. Will only overwrite the file if the data is different from what is already there.
fn overwrite_file<P: AsRef<Path>, D: AsRef<[u8]>>(path: P, data: D) -> std::io::Result<()> {
    if path.as_ref().exists() {
        if let Ok(existing) = fs::read(path.as_ref()) {
            if existing == data.as_ref() {
                return Ok(());
            }
        }
    }
    let mut f = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(path)?;
    f.write_all(data.as_ref())?;
    f.flush()?;
    Ok(())
}

/// Docifies a directory of markdown files
fn compile_markdown_dir<P1: AsRef<Path>, P2: AsRef<Path>>(
    input_dir: P1,
    output_dir: P2,
) -> Result<()> {
    // recursively walk all files in output_dir
    for entry in WalkDir::new(&input_dir)
        .into_iter()
        .filter_map(std::result::Result::ok)
        .filter(|e| {
            if !e.file_type().is_file() && !e.file_type().is_symlink() {
                return false;
            }
            let Some(ext) = e.path().extension() else {
                return false;
            };
            if ext.eq_ignore_ascii_case("md") {
                return true;
            }
            false
        })
    {
        let src_path = entry.path();
        let dest_path = transpose_subpath(&input_dir, &src_path, &output_dir);
        if cfg!(not(test)) {
            write_green(DOCIFYING);
            println!(
                "{} {} {}",
                prettify_path(&src_path).display(),
                "=>", // TODO: fancy arrow
                prettify_path(&dest_path).display(),
            );
        }
        if let Some(parent) = dest_path.parent() {
            let Ok(_) = fs::create_dir_all(parent) else {
                return Err(Error::new(
                    Span::call_site(),
                    format!("Failed to create output directory '{}'", parent.display()),
                ));
            };
        }
        let Ok(source) = fs::read_to_string(src_path) else {
            return Err(Error::new(
                Span::call_site(),
                format!("Failed to read markdown file at '{}'", src_path.display()),
            ));
        };
        let compiled = compile_markdown_source(source.as_str())?;
        if let Some(parent) = dest_path.parent() {
            let Ok(_) = fs::create_dir_all(parent) else {
                return Err(Error::new(
                    Span::call_site(),
                    format!("Failed to create directory '{}'", parent.display()),
                ));
            };
        }
        let Ok(_) = overwrite_file(&dest_path, &compiled) else {
            return Err(Error::new(
                Span::call_site(),
                format!("Failed to write to '{}'", dest_path.display()),
            ));
        };
    }
    Ok(())
}

/// Docifies the specified markdown source string
fn compile_markdown_source<S: AsRef<str>>(source: S) -> Result<String> {
    let source = source.as_ref();
    if source.is_empty() {
        return Ok(String::from(""));
    }
    let mut claimed: Vec<bool> = source.chars().map(|_| false).collect();
    for m in MARKDOWN_CODEBLOCK.find_iter(source) {
        let entity = SourceEntity::new(m.start(), m.end());
        entity.claim(&mut claimed);
    }
    let mut output: Vec<String> = Vec::new();
    let mut prev_end = 0;
    for m in HTML_COMMENT.find_iter(source) {
        let entity = SourceEntity::new(m.start(), m.end());
        if entity.is_claimed(&claimed) {
            // skip HTML comments that are inside of codeblocks
            continue;
        }
        // push prefix
        output.push(String::from(&source[prev_end..m.start()]));
        // get comment
        let orig_comment = &source[m.start()..m.end()];
        // strip <!-- -->
        let comment = &orig_comment[4..(orig_comment.len() - 3)].trim();
        if comment.starts_with("docify") {
            let args = parse2::<EmbedCommentCall>(comment.parse()?)?.args;
            let compiled = embed_internal_str(args.to_token_stream(), MarkdownLanguage::Rust)?;
            output.push(compiled);
        } else {
            output.push(String::from(orig_comment));
        }
        prev_end = m.end();
    }
    // push remaining portion of document if applicable
    if prev_end < source.len() - 1 {
        output.push(String::from(&source[prev_end..]));
    }
    Ok(output.join(""))
}

/// Allows you to use [`docify::embed!(..)`](`macro@embed``) within markdown source files via
/// HTML comments and compiles the result for you (at compile-time).
///
/// The macro supports embed syntax within markdown files like the following:
/// ```markdown
/// # This is some markdown
/// <!-- docify::embed!("some/rust/file.rs", some_ident) -->
/// ```
///
/// Which would expand to the `some_ident` exported item in `some/rust/file.rs` expanding into
/// a Rust codeblock as a replacement for the HTML comment, i.e.:
///
/// ````markdown
/// # This is some markdown
/// ```rust
/// fn hello_world() {
///     println!("hello!");
/// }
/// ```
/// ````
///
/// There are two supported arguments, of the form:
/// ```ignore
/// docify::compile_markdown!("input_path", "output_path");
/// ```
///
/// If `input_path` is a directory, then all markdown files (recursively) found within
/// `input_path` will be processed (expanded) and placed in their respective locations relative
/// to `output_path`.
///
/// If `input_path` is a file and `output_path` is specified, then `input_path` will be loaded
/// as a markdown file, processed, and saved to `output_path` (which must be a file path, not a
/// directory).
///
/// If only `input_path` is specified, then it is assumed to be a file, which is loaded as
/// markdown, processed, and the result is returned as a string literal.
///
/// While files are compiling, terminal output is produced such as:
/// ```txt
/// Docifying fixtures/subfolder/file_2.md => test_bin/subfolder/file_2.md
/// ```
///
/// ## Conventions
///
/// We encourage crate authors to feature-gate their `compile_markdown!` calls like we do for
/// the `README.md` file in this crate:
///
/// ```ignore
/// #[cfg(all(doc, feature = "generate-readme"))]
/// compile_markdown!("README.docify.md", "README.md");
/// ```
///
/// This way the `README.md` will not regenerate itself every time a user of your crate runs
/// `cargo doc` unless they explicitly enable the `generate-readme` feature for your crate.
///
/// Another convention we encourage, shown above, is naming template files `foo.docify.md` so
/// they can exist alongside the generated `foo.md` file without collisions.
#[proc_macro]
pub fn compile_markdown(tokens: TokenStream) -> TokenStream {
    match compile_markdown_internal(tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[cfg(test)]
mod tests;
