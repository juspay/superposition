// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
impl super::Client {
    /// Constructs a fluent builder for the [`ListWorkspace`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder) operation.
                            ///
                            /// - The fluent builder is configurable:
    ///   - [`count(i32)`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::count) / [`set_count(Option<i32>)`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::set_count):<br>required: **false**<br>(undocumented)<br>
    ///   - [`page(i32)`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::page) / [`set_page(Option<i32>)`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::set_page):<br>required: **false**<br>(undocumented)<br>
    ///   - [`all(bool)`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::all) / [`set_all(Option<bool>)`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::set_all):<br>required: **false**<br>(undocumented)<br>
    ///   - [`org_id(impl Into<String>)`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::org_id) / [`set_org_id(Option<String>)`](crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::set_org_id):<br>required: **true**<br>(undocumented)<br>
                            /// - On success, responds with [`ListWorkspaceOutput`](crate::operation::list_workspace::ListWorkspaceOutput) with field(s):
    ///   - [`total_pages(i64)`](crate::operation::list_workspace::ListWorkspaceOutput::total_pages): (undocumented)
    ///   - [`total_items(i64)`](crate::operation::list_workspace::ListWorkspaceOutput::total_items): (undocumented)
    ///   - [`data(Vec::<WorkspaceResponse>)`](crate::operation::list_workspace::ListWorkspaceOutput::data): (undocumented)
                            /// - On failure, responds with [`SdkError<ListWorkspaceError>`](crate::operation::list_workspace::ListWorkspaceError)
    pub fn list_workspace(&self) -> crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder {
                                crate::operation::list_workspace::builders::ListWorkspaceFluentBuilder::new(self.handle.clone())
                            }
}

