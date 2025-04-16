// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct DimensionExt  {
    #[allow(missing_docs)] // documentation missing in model
    pub dimension: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub position: i32,
    #[allow(missing_docs)] // documentation missing in model
    pub schema: ::aws_smithy_types::Document,
    #[allow(missing_docs)] // documentation missing in model
    pub function_name: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub description: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub change_reason: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified_at: ::aws_smithy_types::DateTime,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified_by: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub created_at: ::aws_smithy_types::DateTime,
    #[allow(missing_docs)] // documentation missing in model
    pub created_by: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub dependencies: ::std::vec::Vec::<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub dependents: ::std::vec::Vec::<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub dependency_graph: ::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>,
    #[allow(missing_docs)] // documentation missing in model
    pub mandatory: ::std::option::Option<bool>,
}
impl  DimensionExt  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn dimension(&self) -> &str {
        use std::ops::Deref; self.dimension.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn position(&self) -> i32 {
        self.position
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn schema(&self) -> &::aws_smithy_types::Document {
        &self.schema
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn function_name(&self) -> ::std::option::Option<&str> {
        self.function_name.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(&self) -> &str {
        use std::ops::Deref; self.description.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(&self) -> &str {
        use std::ops::Deref; self.change_reason.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified_at(&self) -> &::aws_smithy_types::DateTime {
        &self.last_modified_at
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified_by(&self) -> &str {
        use std::ops::Deref; self.last_modified_by.deref()
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
    pub fn dependencies(&self) -> &[::std::string::String] {
        use std::ops::Deref; self.dependencies.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn dependents(&self) -> &[::std::string::String] {
        use std::ops::Deref; self.dependents.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn dependency_graph(&self) -> &::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document> {
        &self.dependency_graph
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn mandatory(&self) -> ::std::option::Option<bool> {
        self.mandatory
    }
}
impl DimensionExt {
    /// Creates a new builder-style object to manufacture [`DimensionExt`](crate::types::DimensionExt).
    pub fn builder() -> crate::types::builders::DimensionExtBuilder {
        crate::types::builders::DimensionExtBuilder::default()
    }
}

/// A builder for [`DimensionExt`](crate::types::DimensionExt).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct DimensionExtBuilder {
    pub(crate) dimension: ::std::option::Option<::std::string::String>,
    pub(crate) position: ::std::option::Option<i32>,
    pub(crate) schema: ::std::option::Option<::aws_smithy_types::Document>,
    pub(crate) function_name: ::std::option::Option<::std::string::String>,
    pub(crate) description: ::std::option::Option<::std::string::String>,
    pub(crate) change_reason: ::std::option::Option<::std::string::String>,
    pub(crate) last_modified_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) last_modified_by: ::std::option::Option<::std::string::String>,
    pub(crate) created_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) created_by: ::std::option::Option<::std::string::String>,
    pub(crate) dependencies: ::std::option::Option<::std::vec::Vec::<::std::string::String>>,
    pub(crate) dependents: ::std::option::Option<::std::vec::Vec::<::std::string::String>>,
    pub(crate) dependency_graph: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>,
    pub(crate) mandatory: ::std::option::Option<bool>,
}
impl DimensionExtBuilder {
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn dimension(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.dimension = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_dimension(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.dimension = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_dimension(&self) -> &::std::option::Option<::std::string::String> {
        &self.dimension
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn position(mut self, input: i32) -> Self {
        self.position = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_position(mut self, input: ::std::option::Option<i32>) -> Self {
        self.position = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_position(&self) -> &::std::option::Option<i32> {
        &self.position
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn schema(mut self, input: ::aws_smithy_types::Document) -> Self {
        self.schema = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_schema(mut self, input: ::std::option::Option<::aws_smithy_types::Document>) -> Self {
        self.schema = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_schema(&self) -> &::std::option::Option<::aws_smithy_types::Document> {
        &self.schema
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn function_name(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.function_name = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_function_name(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.function_name = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_function_name(&self) -> &::std::option::Option<::std::string::String> {
        &self.function_name
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
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn last_modified_at(mut self, input: ::aws_smithy_types::DateTime) -> Self {
        self.last_modified_at = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_last_modified_at(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
        self.last_modified_at = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_last_modified_at(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
        &self.last_modified_at
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
    /// Appends an item to `dependencies`.
    ///
    /// To override the contents of this collection use [`set_dependencies`](Self::set_dependencies).
    ///
    pub fn dependencies(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        let mut v = self.dependencies.unwrap_or_default();
                        v.push(input.into());
                        self.dependencies = ::std::option::Option::Some(v);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_dependencies(mut self, input: ::std::option::Option<::std::vec::Vec::<::std::string::String>>) -> Self {
        self.dependencies = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_dependencies(&self) -> &::std::option::Option<::std::vec::Vec::<::std::string::String>> {
        &self.dependencies
    }
    /// Appends an item to `dependents`.
    ///
    /// To override the contents of this collection use [`set_dependents`](Self::set_dependents).
    ///
    pub fn dependents(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        let mut v = self.dependents.unwrap_or_default();
                        v.push(input.into());
                        self.dependents = ::std::option::Option::Some(v);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_dependents(mut self, input: ::std::option::Option<::std::vec::Vec::<::std::string::String>>) -> Self {
        self.dependents = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_dependents(&self) -> &::std::option::Option<::std::vec::Vec::<::std::string::String>> {
        &self.dependents
    }
    /// Adds a key-value pair to `dependency_graph`.
    ///
    /// To override the contents of this collection use [`set_dependency_graph`](Self::set_dependency_graph).
    ///
    pub fn dependency_graph(mut self, k: impl ::std::convert::Into<::std::string::String>, v: ::aws_smithy_types::Document) -> Self {
        let mut hash_map = self.dependency_graph.unwrap_or_default();
                        hash_map.insert(k.into(), v);
                        self.dependency_graph = ::std::option::Option::Some(hash_map);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_dependency_graph(mut self, input: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>) -> Self {
        self.dependency_graph = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_dependency_graph(&self) -> &::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>> {
        &self.dependency_graph
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn mandatory(mut self, input: bool) -> Self {
        self.mandatory = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_mandatory(mut self, input: ::std::option::Option<bool>) -> Self {
        self.mandatory = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_mandatory(&self) -> &::std::option::Option<bool> {
        &self.mandatory
    }
    /// Consumes the builder and constructs a [`DimensionExt`](crate::types::DimensionExt).
    /// This method will fail if any of the following fields are not set:
    /// - [`dimension`](crate::types::builders::DimensionExtBuilder::dimension)
    /// - [`position`](crate::types::builders::DimensionExtBuilder::position)
    /// - [`schema`](crate::types::builders::DimensionExtBuilder::schema)
    /// - [`description`](crate::types::builders::DimensionExtBuilder::description)
    /// - [`change_reason`](crate::types::builders::DimensionExtBuilder::change_reason)
    /// - [`last_modified_at`](crate::types::builders::DimensionExtBuilder::last_modified_at)
    /// - [`last_modified_by`](crate::types::builders::DimensionExtBuilder::last_modified_by)
    /// - [`created_at`](crate::types::builders::DimensionExtBuilder::created_at)
    /// - [`created_by`](crate::types::builders::DimensionExtBuilder::created_by)
    /// - [`dependencies`](crate::types::builders::DimensionExtBuilder::dependencies)
    /// - [`dependents`](crate::types::builders::DimensionExtBuilder::dependents)
    /// - [`dependency_graph`](crate::types::builders::DimensionExtBuilder::dependency_graph)
    pub fn build(self) -> ::std::result::Result<crate::types::DimensionExt, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::types::DimensionExt {
                dimension: self.dimension
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("dimension", "dimension was not specified but it is required when building DimensionExt")
                    )?
                ,
                position: self.position
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("position", "position was not specified but it is required when building DimensionExt")
                    )?
                ,
                schema: self.schema
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("schema", "schema was not specified but it is required when building DimensionExt")
                    )?
                ,
                function_name: self.function_name
                ,
                description: self.description
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("description", "description was not specified but it is required when building DimensionExt")
                    )?
                ,
                change_reason: self.change_reason
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("change_reason", "change_reason was not specified but it is required when building DimensionExt")
                    )?
                ,
                last_modified_at: self.last_modified_at
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("last_modified_at", "last_modified_at was not specified but it is required when building DimensionExt")
                    )?
                ,
                last_modified_by: self.last_modified_by
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("last_modified_by", "last_modified_by was not specified but it is required when building DimensionExt")
                    )?
                ,
                created_at: self.created_at
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("created_at", "created_at was not specified but it is required when building DimensionExt")
                    )?
                ,
                created_by: self.created_by
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("created_by", "created_by was not specified but it is required when building DimensionExt")
                    )?
                ,
                dependencies: self.dependencies
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("dependencies", "dependencies was not specified but it is required when building DimensionExt")
                    )?
                ,
                dependents: self.dependents
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("dependents", "dependents was not specified but it is required when building DimensionExt")
                    )?
                ,
                dependency_graph: self.dependency_graph
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("dependency_graph", "dependency_graph was not specified but it is required when building DimensionExt")
                    )?
                ,
                mandatory: self.mandatory
                ,
            }
        )
    }
}

