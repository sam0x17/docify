#[docify::export]
#[test]
fn with_hidden() {
    #[docify::hidden]
    type Hidden = u32;

    #[docify::hidden]
    let b = 42;

    #[docify::hidden]
    fn nested() {}

    #[docify::hidden]
    let closure = || {
        println!("hidden");
    };

    let block = {
        type Shown = u32;
        #[docify::hidden]
        type Hidden = u32;

        let a = 42;
        #[docify::hidden]
        let b = 42;

        #[docify::hidden]
        fn nested() {}

        #[docify::hidden]
        let closure = || {
            println!("hidden");
        };

        7
    };
}

fn main() {}
