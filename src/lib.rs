//! Docify provides a simple set of rust macros, namely [`#[docify::export(..)]`](`export`)
//! and [`docify::embed!(..)`](`embed`), that allow you to dynamically embed tests and examples
//! at compile time from elsewhere in your workspace directly within rust docs comments, with
//! the option to make these examples runnable.
//!
//! The intent behind docify is to allow you to showcase your best examples and tests directly
//! in your docs, without having to update them in two places every time there is a change. It
//! also encourages a methodology where crate authors better document their tests, since they
//! can now showcase these directly in their doc comments.
//!
//! All-in-all this is a much better workflow than having doc examples isolated within your docs,
//! since you can avoid boilerplate from the surrounding code and just focus on showcasing the item
//! you want to highlight.
//!
//! ## Usage
//!
//! Using `docify` is simple. First mark the tests/examples/items that you wish to embed with
//! `#[docify::export]`, such as the following:
//!
//! ```
//! #[docify::export]
//! fn some_example() {
//!     assert_eq!(2 + 2, 4);
//!     assert_eq!(2 + 3, 5);
//!     assert_eq!(3 + 3, 6);
//! }
//! ```
//!
//! You can then embed this item directly in doc comments using the `docify::embed` macro:
//!
//! ```
//! /// These are some docs about an item. You can embed examples, tests, and
//! /// other items directly into docs using the following macro:
//! #[doc = docify::embed!("examples/samples.rs", some_example)]
//! /// More docs can go here, the example will embed itself inline exactly
//! /// where you reference it.
//! pub struct SomeItem;
//! ```
//!
//! This will result in the following expanded doc comments:
//!
//! ```
//! /// These are some docs about an item. You can embed examples, tests, and
//! /// other items directly into docs using the following macro:
//! /// ```ignore
//! /// fn some_example() {
//! ///     assert_eq!(2 + 2, 4);
//! ///     assert_eq!(2 + 3, 5);
//! ///     assert_eq!(3 + 3, 6);
//! /// }
//! /// ```
//! /// More docs can go here, the example will embed itself inline exactly
//! /// where you reference it.
//! pub struct SomeItem;
//! ```
//!
//! You can embed any item capable of having an attribute macro attached to it.
//!
//! For more documentation, features, and examples, check out [the docs](https://docs.rs/docify)!

pub use docify_macros::*;
