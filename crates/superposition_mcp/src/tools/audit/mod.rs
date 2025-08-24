// Individual tool modules
pub mod list_audit_logs;
pub mod list_versions;

// Re-export individual tools
pub use list_audit_logs::ListAuditLogsTool;
pub use list_versions::ListVersionsTool;

// Compose all audit tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    AuditTools,
    ListAuditLogsTool,
    ListVersionsTool
);
