use serde_json::{Map, Value};
use leptos::{View, view, IntoView};

pub type CellFormatter = fn(&str, &Map<String, Value>) -> View;

#[derive(Clone, PartialEq)]
pub struct Column {
    pub name: String,
    pub hidden: bool,
    pub formatter: CellFormatter
}

fn default_formatter(value: &str, row: &Map<String, Value>) -> View {
    view! {
        <span>{value.to_string()}</span>
    }.into_view()
}

impl Column {
    pub fn default(name: String) -> Column {
        Column{
            name: name,
            hidden: false,
            formatter: default_formatter
        }
    }
    pub fn new(name: String, hidden: Option<bool>, formatter: Option<CellFormatter>) -> Column {
        Column{
            name: name,
            hidden: hidden.unwrap_or(false),
            formatter: formatter.unwrap_or(default_formatter)
        }
    }
}