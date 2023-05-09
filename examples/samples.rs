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

#[docify::export]
fn some_example() {
    assert_eq!(2 + 2, 4);
    assert_eq!(2 + 3, 5);
    assert_eq!(3 + 3, 6);
}

fn main() {}
