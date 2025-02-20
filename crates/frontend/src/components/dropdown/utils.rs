use serde_json::Value;

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
