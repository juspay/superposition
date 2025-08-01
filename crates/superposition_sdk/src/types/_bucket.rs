// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct Bucket  {
    #[allow(missing_docs)] // documentation missing in model
    pub experiment_id: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub variant_id: ::std::string::String,
}
impl  Bucket  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn experiment_id(&self) -> &str {
        use std::ops::Deref; self.experiment_id.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn variant_id(&self) -> &str {
        use std::ops::Deref; self.variant_id.deref()
    }
}
impl Bucket {
    /// Creates a new builder-style object to manufacture [`Bucket`](crate::types::Bucket).
    pub fn builder() -> crate::types::builders::BucketBuilder {
        crate::types::builders::BucketBuilder::default()
    }
}

/// A builder for [`Bucket`](crate::types::Bucket).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct BucketBuilder {
    pub(crate) experiment_id: ::std::option::Option<::std::string::String>,
    pub(crate) variant_id: ::std::option::Option<::std::string::String>,
}
impl BucketBuilder {
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn experiment_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.experiment_id = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_experiment_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.experiment_id = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_experiment_id(&self) -> &::std::option::Option<::std::string::String> {
        &self.experiment_id
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn variant_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.variant_id = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_variant_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.variant_id = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_variant_id(&self) -> &::std::option::Option<::std::string::String> {
        &self.variant_id
    }
    /// Consumes the builder and constructs a [`Bucket`](crate::types::Bucket).
    /// This method will fail if any of the following fields are not set:
    /// - [`experiment_id`](crate::types::builders::BucketBuilder::experiment_id)
    /// - [`variant_id`](crate::types::builders::BucketBuilder::variant_id)
    pub fn build(self) -> ::std::result::Result<crate::types::Bucket, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::types::Bucket {
                experiment_id: self.experiment_id
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("experiment_id", "experiment_id was not specified but it is required when building Bucket")
                    )?
                ,
                variant_id: self.variant_id
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("variant_id", "variant_id was not specified but it is required when building Bucket")
                    )?
                ,
            }
        )
    }
}

