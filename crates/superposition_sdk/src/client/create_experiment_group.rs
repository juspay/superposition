// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
impl super::Client {
    /// Constructs a fluent builder for the [`CreateExperimentGroup`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder) operation.
                            ///
                            /// - The fluent builder is configurable:
    ///   - [`workspace_id(impl Into<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::workspace_id) / [`set_workspace_id(Option<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::set_workspace_id):<br>required: **true**<br>(undocumented)<br>
    ///   - [`org_id(impl Into<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::org_id) / [`set_org_id(Option<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::set_org_id):<br>required: **true**<br>(undocumented)<br>
    ///   - [`name(impl Into<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::name) / [`set_name(Option<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::set_name):<br>required: **true**<br>(undocumented)<br>
    ///   - [`description(impl Into<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::description) / [`set_description(Option<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::set_description):<br>required: **true**<br>(undocumented)<br>
    ///   - [`change_reason(impl Into<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::change_reason) / [`set_change_reason(Option<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::set_change_reason):<br>required: **true**<br>Reason for creating this experiment group.<br>
    ///   - [`context(impl Into<String>, Document)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::context) / [`set_context(Option<HashMap::<String, Document>>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::set_context):<br>required: **true**<br>(undocumented)<br>
    ///   - [`traffic_percentage(i32)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::traffic_percentage) / [`set_traffic_percentage(Option<i32>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::set_traffic_percentage):<br>required: **true**<br>(undocumented)<br>
    ///   - [`member_experiment_ids(impl Into<String>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::member_experiment_ids) / [`set_member_experiment_ids(Option<Vec::<String>>)`](crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::set_member_experiment_ids):<br>required: **false**<br>List of experiment IDs that are members of this group.<br>
                            /// - On success, responds with [`CreateExperimentGroupOutput`](crate::operation::create_experiment_group::CreateExperimentGroupOutput) with field(s):
    ///   - [`id(String)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::id): (undocumented)
    ///   - [`context_hash(String)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::context_hash): (undocumented)
    ///   - [`name(String)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::name): (undocumented)
    ///   - [`description(String)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::description): (undocumented)
    ///   - [`change_reason(String)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::change_reason): (undocumented)
    ///   - [`context(HashMap::<String, Document>)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::context): (undocumented)
    ///   - [`traffic_percentage(i32)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::traffic_percentage): (undocumented)
    ///   - [`member_experiment_ids(Vec::<String>)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::member_experiment_ids): (undocumented)
    ///   - [`created_at(DateTime)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::created_at): (undocumented)
    ///   - [`created_by(String)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::created_by): (undocumented)
    ///   - [`last_modified_at(DateTime)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::last_modified_at): (undocumented)
    ///   - [`last_modified_by(String)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::last_modified_by): (undocumented)
    ///   - [`buckets(Vec::<Option<Bucket>>)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::buckets): (undocumented)
    ///   - [`group_type(GroupType)`](crate::operation::create_experiment_group::CreateExperimentGroupOutput::group_type): (undocumented)
                            /// - On failure, responds with [`SdkError<CreateExperimentGroupError>`](crate::operation::create_experiment_group::CreateExperimentGroupError)
    pub fn create_experiment_group(&self) -> crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder {
                                crate::operation::create_experiment_group::builders::CreateExperimentGroupFluentBuilder::new(self.handle.clone())
                            }
}

