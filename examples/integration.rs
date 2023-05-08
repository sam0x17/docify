/// These are some docs
/// These are some more docs
#[doc = docify::embed!("examples/samples.rs", MyCoolStruct)]
/// even more here
#[allow(unused)]
pub struct SomeItem;

/// These are some docs
/// These are some more docs
#[doc = docify::embed!("examples/samples.rs", SomeImpl)]
/// even more here
#[allow(unused)]
pub struct TestExplicitName;

/// Some more docs
#[doc = docify::embed!("examples/samples.rs", some_random_test)]
#[doc = docify::embed!("examples/samples.rs", test_with_custom_name)]
pub struct MultipleEmbeds;

#[doc = docify::embed!("examples/samples.rs", Duplicate)]
pub struct Duplicates;

/// This example will actually run in rust docs
#[doc = docify::embed_run!("examples/samples.rs", runnable)]
pub struct Runnable;

/// This example runs a test
#[doc = docify::embed_run!("examples/samples.rs", test_with_custom_name)]
pub struct RunnableTest;

fn main() {}
