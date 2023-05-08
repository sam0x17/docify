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

fn main() {}
