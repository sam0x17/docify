#![allow(unused)]

#[docify::export]
struct MyCoolStruct {
    field1: u32,
    field2: bool,
}

#[docify::export]
#[test]
fn some_random_test() {
    assert_eq!(2 + 2, 4);
}

#[docify::export(test_with_custom_name)]
#[test]
fn another_test() {
    assert_eq!(2 + 3, 5);
}

trait DoSomething {
    fn do_something();
}

#[docify::export(SomeImpl)]
impl DoSomething for MyCoolStruct {
    fn do_something() {
        println!("foo!");
    }
}

#[docify::export(Duplicate)]
struct _StructOne;

#[docify::export(Duplicate)]
struct _StructTwo;

#[docify::export(Duplicate)]
struct _StructThree;

#[docify::export]
#[allow(unused)]
fn runnable() {
    assert_eq!(2 + 2, 4);
}

// This one gets embedded automatically in README.md and src/lib.rs!
#[docify::export]
fn some_example() {
    assert_eq!(2 + 2, 4);
    assert_eq!(2 + 3, 5);
    assert_eq!(3 + 3, 6);
}

/// Some doc comments
#[docify::export]
fn some_complex_example() {
    // some comments
    /// some doc comments
    assert_eq!(2 + 2, 4);
    assert_eq!(2 + 3, 5);
    /* some multi line
    comment that spans multiple
    "string literal in multi-line comment"
    // comment in a comment
    lines */
    // "string literal in comment"
    /// "string literal in doc comment"
    assert_eq!(3 + 3, 6);
}

#[rustfmt::skip]
mod bad {
    #[docify::export]
    fn 
    wonky_comment_example() { /* first comment */
       // this is a line comment
                // this is also a line comment
            /*
        some multilinestuff
    */
                            println!("hello world");
        }
}

#[docify::export]
#[test]
fn test_with_normal_ordering() {
    assert_eq!(2 + 2, 4);
}

#[test]
#[docify::export]
fn test_with_weird_ordering() {
    assert_eq!(2 + 2, 4);
}

fn main() {}
