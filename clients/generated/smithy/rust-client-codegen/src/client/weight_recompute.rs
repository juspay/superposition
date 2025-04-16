// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
impl super::Client {
    /// Constructs a fluent builder for the [`WeightRecompute`](crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder) operation.
                            ///
                            /// - The fluent builder is configurable:
    ///   - [`workspace_id(impl Into<String>)`](crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder::workspace_id) / [`set_workspace_id(Option<String>)`](crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder::set_workspace_id):<br>required: **true**<br>(undocumented)<br>
    ///   - [`org_id(impl Into<String>)`](crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder::org_id) / [`set_org_id(Option<String>)`](crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder::set_org_id):<br>required: **true**<br>(undocumented)<br>
    ///   - [`config_tags(impl Into<String>)`](crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder::config_tags) / [`set_config_tags(Option<String>)`](crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder::set_config_tags):<br>required: **false**<br>(undocumented)<br>
                            /// - On success, responds with [`WeightRecomputeOutput`](crate::operation::weight_recompute::WeightRecomputeOutput) with field(s):
    ///   - [`data(Option<Vec::<WeightRecomputeResponse>>)`](crate::operation::weight_recompute::WeightRecomputeOutput::data): (undocumented)
                            /// - On failure, responds with [`SdkError<WeightRecomputeError>`](crate::operation::weight_recompute::WeightRecomputeError)
    pub fn weight_recompute(&self) -> crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder {
                                crate::operation::weight_recompute::builders::WeightRecomputeFluentBuilder::new(self.handle.clone())
                            }
}

