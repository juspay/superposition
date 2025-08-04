use std::rc::Rc;

use leptos::{view, Callback, IntoView, View};
use serde_json::{Map, Value};
use superposition_types::SortBy;

use crate::utils::to_title_case;

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

pub fn default_column_formatter(value: &str) -> View {
    let column_name = to_title_case(value);
    view! { <span class="font-medium text-sm text-black">{column_name}</span> }
        .into_view()
}

impl Column {
    pub fn default(name: String) -> Self {
        Self {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(default_formatter)),
            sortable: ColumnSortable::No,
            expandable: Expandable::Enabled(100),
            column_formatter: Box::new(Rc::new(default_column_formatter)),
        }
    }
    pub fn default_with_cell_formatter<NF>(name: String, formatter: NF) -> Self
    where
        NF: Fn(&str, &Map<String, Value>) -> View + 'static,
    {
        Self {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(formatter)),
            sortable: ColumnSortable::No,
            expandable: Expandable::Enabled(100),
            column_formatter: Box::new(Rc::new(default_column_formatter)),
        }
    }
    pub fn default_no_collapse(name: String) -> Self {
        Self {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(default_formatter)),
            sortable: ColumnSortable::No,
            expandable: Expandable::Disabled,
            column_formatter: Box::new(Rc::new(default_column_formatter)),
        }
    }
    pub fn default_with_sort(name: String, sortable: ColumnSortable) -> Self {
        Self {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(default_formatter)),
            sortable,
            expandable: Expandable::Enabled(100),
            column_formatter: Box::new(Rc::new(default_column_formatter)),
        }
    }
    pub fn default_with_column_formatter<CF: Fn(&str) -> View + 'static>(
        name: String,
        column_formatter: CF,
    ) -> Self {
        Self {
            name,
            hidden: false,
            formatter: Box::new(Rc::new(default_formatter)),
            sortable: ColumnSortable::No,
            expandable: Expandable::Enabled(100),
            column_formatter: Box::new(Rc::new(column_formatter)),
        }
    }
    pub fn new<NF, CF>(
        name: String,
        hidden: bool,
        formatter: NF,
        sortable: ColumnSortable,
        expandable: Expandable,
        column_formatter: CF,
    ) -> Self
    where
        NF: Fn(&str, &Map<String, Value>) -> View + 'static,
        CF: Fn(&str) -> View + 'static,
    {
        Self {
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
