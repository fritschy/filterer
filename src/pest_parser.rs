#[derive(pest_derive::Parser, Debug, Clone)]
#[grammar = "filter.pest"]
pub struct Filter;
