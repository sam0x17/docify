use crate::AttributedThing;
use quote::{quote, ToTokens};
use syn::{Attribute, Expr, File, Item, Local};

pub(crate) fn remove_hidden_attribute(source_file: &mut File) {
    for item in &mut source_file.items {
        process_item(item);
    }
}

/// Process a syn::Item and its potential nested items
fn process_item(item: &mut Item) {
    match item {
        Item::Mod(item_mod) => {
            if let Some((_, items)) = &mut item_mod.content {
                items.retain(|i| !item_has_hidden_attribute(i));
                for i in items {
                    process_item(i);
                }
            }
        }
        Item::Fn(item_fn) => {
            // check locals -- no recursion needed.
            item_fn.block.stmts.retain(|s| {
                if let syn::Stmt::Local(l) = s {
                    !local_has_hidden_attribute(l)
                } else {
                    true
                }
            });

            // check exprs -- recursion needed.
            item_fn.block.stmts.retain(|s| {
                if let syn::Stmt::Expr(e, _) = s {
                    !expr_has_hidden_attribute(e)
                } else {
                    true
                }
            });
            item_fn
                .block
                .stmts
                .iter_mut()
                .filter_map(|s| {
                    if let syn::Stmt::Expr(e, _) = s {
                        Some(e)
                    } else {
                        None
                    }
                })
                .for_each(|_e| {
                    // we don't recursively check expressions for now.
                });

            // check items -- recursion needed.
            item_fn.block.stmts.retain(|s| {
                if let syn::Stmt::Item(i) = s {
                    !item_has_hidden_attribute(i)
                } else {
                    true
                }
            });
            item_fn
                .block
                .stmts
                .iter_mut()
                .filter_map(|s| {
                    if let syn::Stmt::Item(i) = s {
                        Some(i)
                    } else {
                        None
                    }
                })
                .for_each(|mut i| {
                    process_item(&mut i);
                });
        }
        _ => {}
    }
}

/// Checks if a syn::Item has a `#[docify::hidden]` attribute.
fn item_has_hidden_attribute(item: &Item) -> bool {
    item.item_attributes().iter().any(is_hidden_attribute)
}

/// Checks if a syn::Expr has a `#[docify::hidden]` attribute.
fn expr_has_hidden_attribute(expr: &Expr) -> bool {
    expr.item_attributes().iter().any(is_hidden_attribute)
}

/// Checks if a syn::Local has a `#[docify::hidden]` attribute.
fn local_has_hidden_attribute(local: &Local) -> bool {
    local.attrs.iter().any(is_hidden_attribute)
}

/// Determines if a given attribute is `#[docify::hidden]`.
fn is_hidden_attribute(attr: &Attribute) -> bool {
    if let syn::Meta::Path(ref path) = attr.meta {
        let valid_path = syn::parse2::<syn::Path>(quote!(docify::hidden)).expect("valid path; qed");
        path.to_token_stream().to_string() == valid_path.to_token_stream().to_string()
    } else {
        false
    }
}
