use serde_json::Value;

use crate::form_types::HtmlDisplay;

pub trait DropdownOption {
    fn key(&self) -> String;
    fn label(&self) -> String;
}

impl DropdownOption for Value {
    fn key(&self) -> String {
        format!("{}", self.html_display())
    }
    fn label(&self) -> String {
        format!("{}", self.html_display())
    }
}
