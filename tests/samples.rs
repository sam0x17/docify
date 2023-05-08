#[docify::export]
#[allow(unused)]
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
