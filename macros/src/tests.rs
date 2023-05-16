use super::*;

#[test]
fn test_export_basic_parsing_valid() {
    export_internal(
        quote!(),
        quote!(
            struct SomeStruct;
        ),
    )
    .unwrap();
    export_internal(
        quote!(some_ident),
        quote!(
            struct SomeStruct;
        ),
    )
    .unwrap();
    export_internal(
        quote!(SomethingSomething),
        quote!(
            struct SomeStruct;
        ),
    )
    .unwrap();
}

#[test]
fn test_export_basic_parsing_invalid() {
    assert!(export_internal(
        quote!(),
        quote!(
            struct SomeStruct
        ),
    )
    .is_err());
    assert!(export_internal(
        quote!(something as something),
        quote!(
            struct SomeStruct;
        ),
    )
    .is_err());
    assert!(export_internal(
        quote!(something something),
        quote!(
            struct SomeStruct;
        ),
    )
    .is_err());
}

#[test]
fn test_compile_markdown_dir() {
    compile_markdown_dir("fixtures", "test_bin").unwrap();
}

#[test]
fn test_compile_markdown_source_positive() {
    assert_eq!(
        compile_markdown_source(
            "this is some markdown\n\
            this is some more markdown\n\
            # this is a title\n\
            <!-- docify::embed!(\"fixtures/file.rs\", some_fn) -->\n\
            this is some more text\n",
        )
        .unwrap(),
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        ```rust\n\
        fn some_fn() {\n    \
            println!(\"foo\");\n\
        }\n\
        ```\n\
        this is some more text\n"
    );
    assert!(compile_markdown_source(
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        <!-- docify::embed!(\"fixtures/file.rs\", some_other_fn) -->\n\
        this is some more text\n",
    )
    .unwrap()
    .contains("bar"));
    assert!(compile_markdown_source(
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        <!--docify::embed!(\"fixtures/file.rs\", some_other_fn) -->\n\
        this is some more text\n",
    )
    .unwrap()
    .contains("bar"));
    assert!(compile_markdown_source(
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        <!-- docify::embed!(\"fixtures/file.rs\", some_fn)-->\n\
        this is some more text\n",
    )
    .unwrap()
    .contains("foo"));
    assert!(compile_markdown_source(
        "this is some markdown\n\
        this is some more markdown\n\
        # this is a title\n\
        <!--docify::embed!(\"fixtures/file.rs\", some_fn)-->\n\
        this is some more text\n",
    )
    .unwrap()
    .contains("foo"));
    assert!(compile_markdown_source(
        "<!-- docify::embed!(\"fixtures/file.rs\", some_fn) --> this is some more text\n",
    )
    .unwrap()
    .ends_with("more text\n"));
    assert!(compile_markdown_source(
        "prefix<!-- docify::embed!(\"fixtures/file.rs\", some_fn) -->",
    )
    .unwrap()
    .starts_with("prefix"));
}

#[test]
fn test_compile_markdown_source_negative() {
    assert!(compile_markdown_source(
        "# this is a title\n\
        <!-- docify:embed!(\"fixtures/file.rs\", some_fn) -->\n\
        this is some more text\n",
    )
    .is_err());
    assert!(compile_markdown_source(
        "# this is a title\n\
        <!-- docify::em!(\"fixtures/file.rs\", some_fn) -->\n\
        this is some more text\n",
    )
    .is_err());
    assert!(compile_markdown_source(
        "# this is a title\n\
        <!-- docify -->\n\
        this is some more text\n",
    )
    .is_err());
}
