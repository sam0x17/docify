# Docify

[![Build Status](https://img.shields.io/github/actions/workflow/status/sam0x17/docify/ci.yaml)](https://github.com/sam0x17/docify/actions/workflows/ci.yaml?query=branch%3Amain)
[![MIT License](https://img.shields.io/github/license/sam0x17/docify)](https://github.com/sam0x17/docify/blob/main/LICENSE)
[![Crates.io](https://img.shields.io/crates/d/docify)](https://crates.io/crates/docify)
[![docs.rs](https://img.shields.io/docsrs/docify?label=docs)](https://docs.rs/docify/latest/docify/)

This crate provides a simple set of rust macros that allow you to dynamically embed tests and
examples from elsewhere in your workspace directly within rust docs comments, with the option
to make these examples runnable or ````ignore`.

The intent behind docify is to allow you to showcase your best examples and tests directly in
your docs, without having to update them in two places every time there is a change. It also
encourages a methodology where crate authors better document their tests, since they can now
showcase these directly in their doc comments.

All-in-all this is a much better workflow than having doc examples isolated within your docs,
since you can avoid boilerplate from the surrounding code and just focus on showcasing the item
you want to highlight.

## Usage

Using `docify` is simple. First mark the tests/examples/items that you wish to embed with
`#[docify::export]`, such as the following:

```rust
#[docify::export]
fn some_example() {
  assert_eq!(2 + 2, 4);
  assert_eq!(2 + 3, 5);
  assert_eq!(3 + 3, 6);
}
```

You can then embed this item directly in doc comments using the `docify::embed` macro:

```rust
/// These are some docs about an item. You can embed examples, tests, and
/// other items directly into docs using the following macro:
#[doc = docify::embed!("source/file/path.rs", some_example)]
/// More docs can go here, the example will embed itself inline exactly
/// where you reference it.
pub struct SomeItem;
```

This will result in the following expanded doc comments:

```rust
/// These are some docs about an item. You can embed examples, tests, and
/// other items directly into docs using the following macro:
/// ```ignore
/// fn some_example() {
///   assert_eq!(2 + 2, 4);
///   assert_eq!(2 + 3, 5);
///   assert_eq!(3 + 3, 6);
/// }
/// ```
/// More docs can go here, the example will embed itself inline exactly
/// where you reference it.
pub struct SomeItem;
```

You can embed any item capable of having an attribute macro attached to it.

For more documentation, features, and examples, check out [the docs](https://docs.rs/docify)!
