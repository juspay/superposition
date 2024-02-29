pub trait DropdownOption {
    fn key(&self) -> String;
    fn label(&self) -> String;
}
