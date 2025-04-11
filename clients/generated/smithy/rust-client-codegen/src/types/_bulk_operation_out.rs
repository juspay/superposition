// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct BulkOperationOut  {
    #[allow(missing_docs)] // documentation missing in model
    pub output: ::std::option::Option<::std::vec::Vec::<crate::types::ContextActionOut>>,
}
impl  BulkOperationOut  {
    #[allow(missing_docs)] // documentation missing in model
    /// 
    /// If no value was sent for this field, a default will be set. If you want to determine if no value was sent, use `.output.is_none()`.
    pub fn output(&self) -> &[crate::types::ContextActionOut] {
        self.output.as_deref()
        .unwrap_or_default()
    }
}
impl BulkOperationOut {
    /// Creates a new builder-style object to manufacture [`BulkOperationOut`](crate::types::BulkOperationOut).
    pub fn builder() -> crate::types::builders::BulkOperationOutBuilder {
        crate::types::builders::BulkOperationOutBuilder::default()
    }
}

/// A builder for [`BulkOperationOut`](crate::types::BulkOperationOut).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct BulkOperationOutBuilder {
    pub(crate) output: ::std::option::Option<::std::vec::Vec::<crate::types::ContextActionOut>>,
}
impl BulkOperationOutBuilder {
    /// Appends an item to `output`.
    ///
    /// To override the contents of this collection use [`set_output`](Self::set_output).
    ///
    pub fn output(mut self, input: crate::types::ContextActionOut) -> Self {
        let mut v = self.output.unwrap_or_default();
                        v.push(input);
                        self.output = ::std::option::Option::Some(v);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_output(mut self, input: ::std::option::Option<::std::vec::Vec::<crate::types::ContextActionOut>>) -> Self {
        self.output = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_output(&self) -> &::std::option::Option<::std::vec::Vec::<crate::types::ContextActionOut>> {
        &self.output
    }
    /// Consumes the builder and constructs a [`BulkOperationOut`](crate::types::BulkOperationOut).
    pub fn build(self) -> crate::types::BulkOperationOut {
        crate::types::BulkOperationOut {
            output: self.output
            ,
        }
    }
}

