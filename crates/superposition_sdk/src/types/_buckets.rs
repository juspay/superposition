// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct Buckets  {
    #[allow(missing_docs)] // documentation missing in model
    pub member: ::std::option::Option<crate::types::Bucket>,
}
impl  Buckets  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn member(&self) -> ::std::option::Option<&crate::types::Bucket> {
        self.member.as_ref()
    }
}
impl Buckets {
    /// Creates a new builder-style object to manufacture [`Buckets`](crate::types::Buckets).
    pub fn builder() -> crate::types::builders::BucketsBuilder {
        crate::types::builders::BucketsBuilder::default()
    }
}

/// A builder for [`Buckets`](crate::types::Buckets).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct BucketsBuilder {
    pub(crate) member: ::std::option::Option<crate::types::Bucket>,
}
impl BucketsBuilder {
    #[allow(missing_docs)] // documentation missing in model
    pub fn member(mut self, input: crate::types::Bucket) -> Self {
        self.member = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_member(mut self, input: ::std::option::Option<crate::types::Bucket>) -> Self {
        self.member = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_member(&self) -> &::std::option::Option<crate::types::Bucket> {
        &self.member
    }
    /// Consumes the builder and constructs a [`Buckets`](crate::types::Buckets).
    pub fn build(self) -> crate::types::Buckets {
        crate::types::Buckets {
            member: self.member
            ,
        }
    }
}

