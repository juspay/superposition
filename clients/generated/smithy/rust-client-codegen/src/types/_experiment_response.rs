// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct ExperimentResponse  {
    #[allow(missing_docs)] // documentation missing in model
    pub id: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub created_at: ::aws_smithy_types::DateTime,
    #[allow(missing_docs)] // documentation missing in model
    pub created_by: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified: ::aws_smithy_types::DateTime,
    #[allow(missing_docs)] // documentation missing in model
    pub name: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub override_keys: ::std::vec::Vec::<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub status: crate::types::ExperimentStatusType,
    #[allow(missing_docs)] // documentation missing in model
    pub traffic_percentage: i32,
    #[allow(missing_docs)] // documentation missing in model
    pub context: ::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>,
    #[allow(missing_docs)] // documentation missing in model
    pub variants: ::std::vec::Vec::<crate::types::Variant>,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified_by: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub chosen_variant: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub description: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub change_reason: ::std::string::String,
}
impl  ExperimentResponse  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn id(&self) -> &str {
        use std::ops::Deref; self.id.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn created_at(&self) -> &::aws_smithy_types::DateTime {
        &self.created_at
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn created_by(&self) -> &str {
        use std::ops::Deref; self.created_by.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified(&self) -> &::aws_smithy_types::DateTime {
        &self.last_modified
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn name(&self) -> &str {
        use std::ops::Deref; self.name.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn override_keys(&self) -> &[::std::string::String] {
        use std::ops::Deref; self.override_keys.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn status(&self) -> &crate::types::ExperimentStatusType {
        &self.status
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn traffic_percentage(&self) -> i32 {
        self.traffic_percentage
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn context(&self) -> &::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document> {
        &self.context
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn variants(&self) -> &[crate::types::Variant] {
        use std::ops::Deref; self.variants.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified_by(&self) -> &str {
        use std::ops::Deref; self.last_modified_by.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn chosen_variant(&self) -> ::std::option::Option<&str> {
        self.chosen_variant.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(&self) -> &str {
        use std::ops::Deref; self.description.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(&self) -> &str {
        use std::ops::Deref; self.change_reason.deref()
    }
}
impl ExperimentResponse {
    /// Creates a new builder-style object to manufacture [`ExperimentResponse`](crate::types::ExperimentResponse).
    pub fn builder() -> crate::types::builders::ExperimentResponseBuilder {
        crate::types::builders::ExperimentResponseBuilder::default()
    }
}

/// A builder for [`ExperimentResponse`](crate::types::ExperimentResponse).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct ExperimentResponseBuilder {
    pub(crate) id: ::std::option::Option<::std::string::String>,
    pub(crate) created_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) created_by: ::std::option::Option<::std::string::String>,
    pub(crate) last_modified: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) name: ::std::option::Option<::std::string::String>,
    pub(crate) override_keys: ::std::option::Option<::std::vec::Vec::<::std::string::String>>,
    pub(crate) status: ::std::option::Option<crate::types::ExperimentStatusType>,
    pub(crate) traffic_percentage: ::std::option::Option<i32>,
    pub(crate) context: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>,
    pub(crate) variants: ::std::option::Option<::std::vec::Vec::<crate::types::Variant>>,
    pub(crate) last_modified_by: ::std::option::Option<::std::string::String>,
    pub(crate) chosen_variant: ::std::option::Option<::std::string::String>,
    pub(crate) description: ::std::option::Option<::std::string::String>,
    pub(crate) change_reason: ::std::option::Option<::std::string::String>,
}
impl ExperimentResponseBuilder {
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.id = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.id = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_id(&self) -> &::std::option::Option<::std::string::String> {
        &self.id
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn created_at(mut self, input: ::aws_smithy_types::DateTime) -> Self {
        self.created_at = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_created_at(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
        self.created_at = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_created_at(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
        &self.created_at
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn created_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.created_by = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_created_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.created_by = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_created_by(&self) -> &::std::option::Option<::std::string::String> {
        &self.created_by
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn last_modified(mut self, input: ::aws_smithy_types::DateTime) -> Self {
        self.last_modified = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_last_modified(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
        self.last_modified = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_last_modified(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
        &self.last_modified
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn name(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.name = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_name(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.name = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_name(&self) -> &::std::option::Option<::std::string::String> {
        &self.name
    }
    /// Appends an item to `override_keys`.
    ///
    /// To override the contents of this collection use [`set_override_keys`](Self::set_override_keys).
    ///
    pub fn override_keys(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        let mut v = self.override_keys.unwrap_or_default();
                        v.push(input.into());
                        self.override_keys = ::std::option::Option::Some(v);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_override_keys(mut self, input: ::std::option::Option<::std::vec::Vec::<::std::string::String>>) -> Self {
        self.override_keys = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_override_keys(&self) -> &::std::option::Option<::std::vec::Vec::<::std::string::String>> {
        &self.override_keys
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn status(mut self, input: crate::types::ExperimentStatusType) -> Self {
        self.status = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_status(mut self, input: ::std::option::Option<crate::types::ExperimentStatusType>) -> Self {
        self.status = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_status(&self) -> &::std::option::Option<crate::types::ExperimentStatusType> {
        &self.status
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn traffic_percentage(mut self, input: i32) -> Self {
        self.traffic_percentage = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_traffic_percentage(mut self, input: ::std::option::Option<i32>) -> Self {
        self.traffic_percentage = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_traffic_percentage(&self) -> &::std::option::Option<i32> {
        &self.traffic_percentage
    }
    /// Adds a key-value pair to `context`.
    ///
    /// To override the contents of this collection use [`set_context`](Self::set_context).
    ///
    pub fn context(mut self, k: impl ::std::convert::Into<::std::string::String>, v: ::aws_smithy_types::Document) -> Self {
        let mut hash_map = self.context.unwrap_or_default();
                        hash_map.insert(k.into(), v);
                        self.context = ::std::option::Option::Some(hash_map);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_context(mut self, input: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>) -> Self {
        self.context = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_context(&self) -> &::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>> {
        &self.context
    }
    /// Appends an item to `variants`.
    ///
    /// To override the contents of this collection use [`set_variants`](Self::set_variants).
    ///
    pub fn variants(mut self, input: crate::types::Variant) -> Self {
        let mut v = self.variants.unwrap_or_default();
                        v.push(input);
                        self.variants = ::std::option::Option::Some(v);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_variants(mut self, input: ::std::option::Option<::std::vec::Vec::<crate::types::Variant>>) -> Self {
        self.variants = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_variants(&self) -> &::std::option::Option<::std::vec::Vec::<crate::types::Variant>> {
        &self.variants
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn last_modified_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.last_modified_by = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_last_modified_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.last_modified_by = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_last_modified_by(&self) -> &::std::option::Option<::std::string::String> {
        &self.last_modified_by
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn chosen_variant(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.chosen_variant = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_chosen_variant(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.chosen_variant = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_chosen_variant(&self) -> &::std::option::Option<::std::string::String> {
        &self.chosen_variant
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn description(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.description = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_description(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.description = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_description(&self) -> &::std::option::Option<::std::string::String> {
        &self.description
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn change_reason(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.change_reason = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_change_reason(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.change_reason = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_change_reason(&self) -> &::std::option::Option<::std::string::String> {
        &self.change_reason
    }
    /// Consumes the builder and constructs a [`ExperimentResponse`](crate::types::ExperimentResponse).
    /// This method will fail if any of the following fields are not set:
    /// - [`id`](crate::types::builders::ExperimentResponseBuilder::id)
    /// - [`created_at`](crate::types::builders::ExperimentResponseBuilder::created_at)
    /// - [`created_by`](crate::types::builders::ExperimentResponseBuilder::created_by)
    /// - [`last_modified`](crate::types::builders::ExperimentResponseBuilder::last_modified)
    /// - [`name`](crate::types::builders::ExperimentResponseBuilder::name)
    /// - [`override_keys`](crate::types::builders::ExperimentResponseBuilder::override_keys)
    /// - [`status`](crate::types::builders::ExperimentResponseBuilder::status)
    /// - [`traffic_percentage`](crate::types::builders::ExperimentResponseBuilder::traffic_percentage)
    /// - [`context`](crate::types::builders::ExperimentResponseBuilder::context)
    /// - [`variants`](crate::types::builders::ExperimentResponseBuilder::variants)
    /// - [`last_modified_by`](crate::types::builders::ExperimentResponseBuilder::last_modified_by)
    /// - [`description`](crate::types::builders::ExperimentResponseBuilder::description)
    /// - [`change_reason`](crate::types::builders::ExperimentResponseBuilder::change_reason)
    pub fn build(self) -> ::std::result::Result<crate::types::ExperimentResponse, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::types::ExperimentResponse {
                id: self.id
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("id", "id was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                created_at: self.created_at
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("created_at", "created_at was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                created_by: self.created_by
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("created_by", "created_by was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                last_modified: self.last_modified
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("last_modified", "last_modified was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                name: self.name
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("name", "name was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                override_keys: self.override_keys
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("override_keys", "override_keys was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                status: self.status
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("status", "status was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                traffic_percentage: self.traffic_percentage
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("traffic_percentage", "traffic_percentage was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                context: self.context
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("context", "context was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                variants: self.variants
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("variants", "variants was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                last_modified_by: self.last_modified_by
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("last_modified_by", "last_modified_by was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                chosen_variant: self.chosen_variant
                ,
                description: self.description
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("description", "description was not specified but it is required when building ExperimentResponse")
                    )?
                ,
                change_reason: self.change_reason
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("change_reason", "change_reason was not specified but it is required when building ExperimentResponse")
                    )?
                ,
            }
        )
    }
}

