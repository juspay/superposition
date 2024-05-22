use std::rc::Rc;

use leptos::{view, Callback, IntoView, View};
use serde_json::{Map, Value};

pub type CellFormatter = Box<Rc<dyn Fn(&str, &Map<String, Value>) -> View>>;

#[derive(Clone, Debug)]
pub struct TableSettings {
    pub redirect_prefix: Option<String>,
}

#[derive(Clone)]
pub struct Column {
    pub name: String,
    pub hidden: bool,
    pub formatter: CellFormatter,
}

impl PartialEq for Column {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.hidden == other.hidden
    }
}

fn default_formatter(value: &str, _row: &Map<String, Value>) -> View {
    view! { <span>{value.to_string()}</span> }.into_view()
}

impl Column {
    pub fn default(name: String) -> Column {
        Column {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(default_formatter)),
        }
    }
    pub fn new<NF>(name: String, hidden: Option<bool>, formatter: NF) -> Column
    where
        NF: Fn(&str, &Map<String, Value>) -> View + 'static,
    {
        Column {
            name,
            hidden: hidden.unwrap_or(false),
            formatter: Box::new(Rc::new(formatter)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TablePaginationProps {
    pub enabled: bool,
    pub current_page: i64,
    pub total_pages: i64,
    pub count: i64,
    pub on_next: Callback<i64>,
    pub on_prev: Callback<()>,
}

impl Default for TablePaginationProps {
    fn default() -> Self {
        TablePaginationProps {
            enabled: false,
            current_page: 0,
            total_pages: 0,
            count: 0,
            on_next: Callback::new(move |_| {}),
            on_prev: Callback::new(move |_| {}),
        }
    }
}
