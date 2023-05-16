//! This crate contains the proc macros used by [docify](https://crates.io/crates/docify).

use colored::*;
use derive_syn_parse::Parse;
use lazy_static::lazy_static;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use regex::Regex;
use std::{
    collections::HashMap,
    env,
    fs::{self, OpenOptions},
    io::Write,
    path::{Path, PathBuf},
};
use syn::{
    parse2,
    spanned::Spanned,
    token::Paren,
    visit::{self, Visit},
    AttrStyle, Attribute, Error, File, Ident, Item, LitStr, Meta, Result, Token,
};
use walkdir::WalkDir;

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
    const EMPTY: &Vec<Attribute> = &Vec::new();
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
        _ => EMPTY,
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
/// - `source_path`: the file path (relative to the workspace root) that contains the item you
///   would like to embed, represented as a string literal. If you wish to embed an entire
///   file, simply specify only a `source_path` with no other arguments and the entire file
///   will be embedded as a doc example. If the path cannot be read for whatever reason, a
///   compile error will be issued. The `source_path` _does  not_ have to be a file that is
///   part of the current compilation unit/project/workspace, though typically it should be.
///   The only requirement is that it must contain valid Rust source code.
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
/// Pretty formatting is provided by the [prettyplease](https://crates.io/crates/prettyplease)
/// crate, and should match `rustfmt` output almost exactly. The reason this must be used is,
/// except with the case of importing an entire file verbatim, we need to parse the source file
/// with `syn`, which garbles indentation and newlines in many cases, so to fix this, we must
/// use a formatter.
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
        let Some(item_ident) = &self.item_ident else { return };
        tokens.extend(quote!(,));
        tokens.extend(item_ident.to_token_stream());
    }
}

mod keywords {
    use syn::custom_keyword;

    custom_keyword!(docify);
    custom_keyword!(embed);
}

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

#[derive(Copy, Clone, Eq, PartialEq)]
enum MarkdownLanguage {
    Ignore,
    Rust,
    Blank,
}

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

struct ItemVisitor {
    search: Ident,
    results: Vec<Item>,
}

impl<'ast> Visit<'ast> for ItemVisitor {
    fn visit_item(&mut self, node: &'ast Item) {
        let mut i = 0;
        let attrs = item_attributes(node);
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
                None => match name_ident(node) {
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum EntityType {
    LineComment,
    MultiLineComment,
    DocComment,
    DocCommentAttr,
    // StringLiteral,
}

#[derive(Clone, PartialEq, Eq)]
struct SourceEntity {
    start: usize,
    end: usize,
    entity_type: EntityType,
}

impl SourceEntity {
    pub fn new(start: usize, end: usize, entity_type: EntityType) -> SourceEntity {
        SourceEntity {
            start,
            end,
            entity_type,
        }
    }

    pub fn claim(&self, claimed: &mut Vec<Option<SourceEntity>>) {
        for i in self.start..(self.end + 1) {
            claimed[i] = Some(self.clone());
        }
    }

    pub fn is_claimed(&self, claimed: &Vec<Option<SourceEntity>>) -> bool {
        claimed[(self.start + self.end) / 2].is_some()
    }

    // pub fn value<'a>(&self, source: &'a String) -> &'a str {
    //     &source.as_str()[self.start..self.end]
    // }

    // pub fn contains(&self, x: usize) -> bool {
    //     x >= self.start && x < self.end
    // }
}

struct CompressedString {
    chars: HashMap<usize, OffsetChar>,
    chars_arr: Vec<OffsetChar>,
}

impl CompressedString {
    fn to_string(&self) -> String {
        self.chars_arr.iter().map(|c| c.char).collect()
    }
}

impl From<&String> for CompressedString {
    fn from(value: &String) -> Self {
        lazy_static! {
            static ref DOC_COMMENT: Regex = Regex::new(r"///.*").unwrap();
            static ref DOC_COMMENT_ATTR: Regex = Regex::new(r#"#\[doc = ".*"]"#).unwrap();
            static ref LINE_COMMENT: Regex = Regex::new(r"//.*").unwrap();
            static ref MULTI_LINE_COMMENT: Regex = Regex::new(r"/\*[\s\S]*?\*/").unwrap();
            // static ref STRING_LIT: Regex = Regex::new(r#"("([^"\\]|\\[\s\S])*")"#).unwrap();
        }
        let mut entities: Vec<SourceEntity> = Vec::new();
        let mut claimed: Vec<Option<SourceEntity>> = value.chars().map(|_| None).collect();
        for m in DOC_COMMENT.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end(), EntityType::DocComment);
            entity.claim(&mut claimed);
            entities.push(entity);
        }
        for m in DOC_COMMENT_ATTR.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end(), EntityType::DocCommentAttr);
            if !entity.is_claimed(&claimed) {
                entity.claim(&mut claimed);
                entities.push(entity);
            }
        }
        for m in MULTI_LINE_COMMENT.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end(), EntityType::MultiLineComment);
            if !entity.is_claimed(&claimed) {
                entity.claim(&mut claimed);
                entities.push(entity);
            }
        }
        for m in LINE_COMMENT.find_iter(value) {
            let entity = SourceEntity::new(m.start(), m.end(), EntityType::LineComment);
            if !entity.is_claimed(&claimed) {
                entity.claim(&mut claimed);
                entities.push(entity);
            }
        }
        let mut compressed = CompressedString {
            chars_arr: Vec::new(),
            chars: HashMap::new(),
        };
        let chars: Vec<char> = value.chars().collect();
        let mut cursor = 0;
        for (i, c) in chars.iter().enumerate() {
            if claimed[i].is_some() || c.is_whitespace() {
                continue;
            }
            let oc = OffsetChar::new(*c, i);
            compressed.chars.insert(cursor, oc);
            compressed.chars_arr.push(oc);
            cursor += 1;
        }
        compressed
    }
}

/// Finds and returns the specified [`Item`] within a source text string and returns the exact
/// source code of that item, without any formatting changes
fn source_excerpt<'a>(source: &'a String, item: &'a Item) -> Result<&'a str> {
    // note: can't rely on span locations because this requires nightly and it turns out the
    // spans for most items do not actually fully enclose them, sometimes just the ident, etc
    let compressed_source = CompressedString::from(source);
    let compressed_item = CompressedString::from(&item.to_token_stream().to_string());
    let compressed_source_string = compressed_source.to_string();
    let compressed_item_string = compressed_item.to_string();
    let Some(found_start) = compressed_source_string.find(compressed_item_string.as_str()) else {
        return Err(Error::new(
            item.span(),
            "You have found a bug in docify! Please submit a new GitHub issue at \
            https://github.com/sam0x17/docify/issues/new?title=%60source_excerpt\
            %60%3A%20can%27t%20find%20item%20in%20source with a sample of the item \
            you are trying to embed."
        ))
    };
    let start_c = compressed_source.chars[&found_start];
    let start_pos = start_c.original_pos;
    let end_c = compressed_source.chars[&(found_start + compressed_item_string.len() - 1)];
    let end_pos = end_c.original_pos;
    Ok(&source[(start_pos)..(end_pos + 1)])
}

fn embed_internal_str(tokens: impl Into<TokenStream2>, lang: MarkdownLanguage) -> Result<String> {
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
                    args.file_path.value()
                ),
            ));
        }
        let mut results: Vec<String> = Vec::new();
        for item in visitor.results {
            let excerpt = source_excerpt(&source_code, &item)?;
            let example = into_example(excerpt, lang);
            results.push(example);
        }
        results.join("\n")
    } else {
        into_example(source_code.as_str(), lang)
    };
    Ok(output)
}

fn embed_internal(tokens: impl Into<TokenStream2>, lang: MarkdownLanguage) -> Result<TokenStream2> {
    let output = embed_internal_str(tokens, lang)?;
    Ok(quote!(#output))
}

#[derive(Parse)]
struct CompileMarkdownArgs {
    input: LitStr,
    #[prefix(Option<Token![,]> as comma)]
    #[parse_if(comma.is_some())]
    output: Option<LitStr>,
}

fn compile_markdown_internal(tokens: impl Into<TokenStream2>) -> Result<TokenStream2> {
    let args = parse2::<CompileMarkdownArgs>(tokens.into())?;
    let input_path = std::path::PathBuf::from(&args.input.value());
    if !input_path.exists() {
        return Err(Error::new(args.input.span(), "Path does not exist"));
    }
    if let Some(output) = args.output {
        if input_path.is_dir() {
            compile_markdown_dir(args.input.value(), output.value())?;
        } else {
            println!(
                "{} {} {} {}",
                "Docifying".green().bold(),
                input_path.display(),
                "=>", // TODO: fancy arrow
                output.value(),
            );
            let Ok(source) = fs::read_to_string(&input_path) else {
                return Err(Error::new(
                    Span::call_site(),
                    format!("Failed to read markdown file at '{}'", input_path.display())
                ));
            };
            let compiled = compile_markdown_source(source.as_str())?;
            let Ok(_) = overwrite_file(output.value(), &compiled) else {
                return Err(Error::new(
                    Span::call_site(),
                    format!("Failed to write to '{}'", output.value())
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
                format!("Failed to read markdown file at '{}'", input_path.display())
            ));
        };
        let compiled = compile_markdown_source(source.as_str())?;
        Ok(quote!(#compiled))
    }
}

fn transpose_subpath<P: AsRef<Path>>(path: P, target_dir: P) -> PathBuf {
    Path::join(
        target_dir.as_ref(),
        path.as_ref().components().skip(1).collect::<PathBuf>(),
    )
}

fn overwrite_file<P: AsRef<Path>, D: AsRef<[u8]>>(path: P, data: D) -> std::io::Result<()> {
    let mut f = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(path)?;
    f.write_all(data.as_ref())?;
    f.flush()?;
    Ok(())
}

fn compile_markdown_dir<P: AsRef<Path>>(input_dir: P, output_dir: P) -> Result<()> {
    // mkdir -p output_dir
    let Ok(_) = fs::create_dir_all(output_dir.as_ref()) else {
        return Err(Error::new(
            Span::call_site(),
            format!("Failed to create output directory '{}'", output_dir.as_ref().display())
        ));
    };
    // recursively walk all files in output_dir
    for entry in WalkDir::new(input_dir)
        .into_iter()
        .filter_map(std::result::Result::ok)
        .filter(|e| {
            if !e.file_type().is_file() && !e.file_type().is_symlink() {
                return false;
            }
            let Some(ext) = e.path().extension() else { return false };
            if ext.eq_ignore_ascii_case("md") {
                return true;
            }
            false
        })
    {
        let src_path = entry.path();
        let dest_path = transpose_subpath(src_path, output_dir.as_ref());
        println!(
            "{} {} {} {}",
            "Docifying".green().bold(),
            src_path.display(),
            "=>", // TODO: fancy arrow
            dest_path.display(),
        );
        let Ok(source) = fs::read_to_string(src_path) else {
            return Err(Error::new(
                Span::call_site(),
                format!("Failed to read markdown file at '{}'", src_path.display())
            ));
        };
        let compiled = compile_markdown_source(source.as_str())?;
        if let Some(parent) = dest_path.parent() {
            let Ok(_) = fs::create_dir_all(parent) else {
                return Err(Error::new(
                    Span::call_site(),
                    format!("Failed to create directory '{}'", parent.display())
                ));
            };
        }
        let Ok(_) = overwrite_file(&dest_path, &compiled) else {
            return Err(Error::new(
                Span::call_site(),
                format!("Failed to write to '{}'", dest_path.display())
            ));
        };
    }
    Ok(())
}

fn compile_markdown_source<S: AsRef<str>>(source: S) -> Result<String> {
    let source = source.as_ref();
    lazy_static! {
        static ref HTML_COMMENT: Regex = Regex::new(r"<!--[\s\S]*?-->").unwrap();
    }
    let mut output: Vec<String> = Vec::new();
    let mut prev_end = 0;
    for m in HTML_COMMENT.find_iter(source) {
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

#[proc_macro]
pub fn compile_markdown(tokens: TokenStream) -> TokenStream {
    match compile_markdown_internal(tokens) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

#[cfg(test)]
mod tests;
