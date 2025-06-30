use std::rc::Rc;

use leptos::{view, Callback, IntoView, View};
use serde_json::{Map, Value};
use superposition_types::SortBy;

pub type CellFormatter = Box<Rc<dyn Fn(&str, &Map<String, Value>) -> View>>;
pub type ColumnFormatter = Box<Rc<dyn Fn(&str) -> View>>;

#[derive(Clone, Debug)]
pub struct TableSettings {
    pub redirect_prefix: Option<String>,
}

#[derive(Clone)]
pub enum ColumnSortable {
    Yes {
        sort_fn: Callback<()>,
        sort_by: SortBy,
        currently_sorted: bool,
    },
    No,
}

#[derive(Clone)]
pub enum Expandable {
    Disabled,
    Enabled(usize),
}

#[derive(Clone)]
pub struct Column {
    pub name: String,
    pub hidden: bool,
    pub formatter: CellFormatter,
    pub sortable: ColumnSortable,
    pub expandable: Expandable,
    pub column_formatter: ColumnFormatter,
}

impl PartialEq for Column {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.hidden == other.hidden
    }
}

pub fn default_formatter(value: &str, _row: &Map<String, Value>) -> View {
    view! { <span>{value.to_string()}</span> }.into_view()
}

fn to_title_case(input: &str) -> String {
    let mut words = Vec::new();
    let mut current_word = String::new();

    for (i, c) in input.char_indices() {
        if c == '_' {
            if !current_word.is_empty() {
                words.push(current_word.clone());
                current_word.clear();
            }
        } else if c.is_uppercase()
            && i != 0
            && !input.chars().nth(i - 1).unwrap().is_uppercase()
        {
            if !current_word.is_empty() {
                words.push(current_word.clone());
                current_word.clear();
            }
            current_word.push(c);
        } else {
            current_word.push(c);
        }
    }

    if !current_word.is_empty() {
        words.push(current_word);
    }

    words
        .into_iter()
        .map(|w| {
            let mut c = w.chars();
            match c.next() {
                Some(f) => {
                    f.to_uppercase().collect::<String>() + &c.as_str().to_lowercase()
                }
                None => String::new(),
            }
        })
        .collect::<Vec<String>>()
        .join(" ")
}

pub fn default_column_formatter(value: &str) -> View {
    let column_name = to_title_case(value);
    view! { <span class="font-medium text-sm text-black">{column_name}</span> }
        .into_view()
}

impl Column {
    pub fn default(name: String) -> Column {
        Column {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(default_formatter)),
            sortable: ColumnSortable::No,
            expandable: Expandable::Enabled(100),
            column_formatter: Box::new(Rc::new(default_column_formatter)),
        }
    }
    pub fn default_with_cell_formatter<NF>(name: String, formatter: NF) -> Column
    where
        NF: Fn(&str, &Map<String, Value>) -> View + 'static,
    {
        Column {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(formatter)),
            sortable: ColumnSortable::No,
            expandable: Expandable::Enabled(100),
            column_formatter: Box::new(Rc::new(default_column_formatter)),
        }
    }
    pub fn default_no_collapse(name: String) -> Column {
        Column {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(default_formatter)),
            sortable: ColumnSortable::No,
            expandable: Expandable::Disabled,
            column_formatter: Box::new(Rc::new(default_column_formatter)),
        }
    }
    pub fn default_with_sort(name: String, sortable: ColumnSortable) -> Column {
        Column {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(default_formatter)),
            sortable,
            expandable: Expandable::Enabled(100),
            column_formatter: Box::new(Rc::new(default_column_formatter)),
        }
    }
    pub fn new<NF, CF>(
        name: String,
        hidden: bool,
        formatter: NF,
        sortable: ColumnSortable,
        expandable: Expandable,
        column_formatter: CF,
    ) -> Column
    where
        NF: Fn(&str, &Map<String, Value>) -> View + 'static,
        CF: Fn(&str) -> View + 'static,
    {
        Column {
            name,
            hidden,
            formatter: Box::new(Rc::new(formatter)),
            sortable,
            expandable,
            column_formatter: Box::new(Rc::new(column_formatter)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TablePaginationProps {
    pub enabled: bool,
    pub current_page: i64,
    pub total_pages: i64,
    pub count: i64,
    pub on_page_change: Callback<i64>,
}

impl Default for TablePaginationProps {
    fn default() -> Self {
        TablePaginationProps {
            enabled: false,
            current_page: 0,
            total_pages: 0,
            count: 0,
            on_page_change: Callback::new(|_| {}),
        }
    }
}
