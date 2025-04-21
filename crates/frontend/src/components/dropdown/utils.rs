use serde_json::Value;
use superposition_types::{api::context::SortOn as SortContextOn, SortBy};

use crate::schema::HtmlDisplay;

pub trait DropdownOption {
    fn key(&self) -> String;
    fn label(&self) -> String;
}

impl DropdownOption for Value {
    fn key(&self) -> String {
        self.html_display().to_string()
    }
    fn label(&self) -> String {
        self.html_display().to_string()
    }
}

impl DropdownOption for SortContextOn {
    fn key(&self) -> String {
        self.to_string()
    }
    fn label(&self) -> String {
        self.label()
    }
}

impl DropdownOption for SortBy {
    fn key(&self) -> String {
        self.to_string()
    }
    fn label(&self) -> String {
        self.label()
    }
}
