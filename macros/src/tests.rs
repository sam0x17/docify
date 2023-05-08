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
