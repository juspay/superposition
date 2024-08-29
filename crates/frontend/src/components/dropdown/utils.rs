use serde_json::Value;

pub trait DropdownOption {
    fn key(&self) -> String;
    fn label(&self) -> String;
}

impl DropdownOption for Value {
    fn key(&self) -> String {
        format!("{}", self)
    }
    fn label(&self) -> String {
        format!("{}", self)
    }
}
