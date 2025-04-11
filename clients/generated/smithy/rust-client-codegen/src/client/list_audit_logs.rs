// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
impl super::Client {
    /// Constructs a fluent builder for the [`ListAuditLogs`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder) operation.
                            ///
                            /// - The fluent builder is configurable:
    ///   - [`workspace_id(impl Into<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::workspace_id) / [`set_workspace_id(Option<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_workspace_id):<br>required: **true**<br>(undocumented)<br>
    ///   - [`org_id(impl Into<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::org_id) / [`set_org_id(Option<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_org_id):<br>required: **true**<br>(undocumented)<br>
    ///   - [`count(i32)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::count) / [`set_count(Option<i32>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_count):<br>required: **false**<br>(undocumented)<br>
    ///   - [`page(i32)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::page) / [`set_page(Option<i32>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_page):<br>required: **false**<br>(undocumented)<br>
    ///   - [`from_date(DateTime)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::from_date) / [`set_from_date(Option<DateTime>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_from_date):<br>required: **false**<br>(undocumented)<br>
    ///   - [`to_date(DateTime)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::to_date) / [`set_to_date(Option<DateTime>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_to_date):<br>required: **false**<br>(undocumented)<br>
    ///   - [`tables(impl Into<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::tables) / [`set_tables(Option<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_tables):<br>required: **false**<br>Comma serparated list of tables.<br>
    ///   - [`action(impl Into<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::action) / [`set_action(Option<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_action):<br>required: **false**<br>Comma serparated list of actions.<br>
    ///   - [`username(impl Into<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::username) / [`set_username(Option<String>)`](crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::set_username):<br>required: **false**<br>(undocumented)<br>
                            /// - On success, responds with [`ListAuditLogsOutput`](crate::operation::list_audit_logs::ListAuditLogsOutput) with field(s):
    ///   - [`total_pages(Option<i32>)`](crate::operation::list_audit_logs::ListAuditLogsOutput::total_pages): (undocumented)
    ///   - [`total_items(Option<i32>)`](crate::operation::list_audit_logs::ListAuditLogsOutput::total_items): (undocumented)
    ///   - [`data(Option<Vec::<AuditLogFull>>)`](crate::operation::list_audit_logs::ListAuditLogsOutput::data): (undocumented)
                            /// - On failure, responds with [`SdkError<ListAuditLogsError>`](crate::operation::list_audit_logs::ListAuditLogsError)
    pub fn list_audit_logs(&self) -> crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder {
                                crate::operation::list_audit_logs::builders::ListAuditLogsFluentBuilder::new(self.handle.clone())
                            }
}

