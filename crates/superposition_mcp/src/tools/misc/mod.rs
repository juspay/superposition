// Individual tool modules
pub mod applicable_variants;
pub mod bulk_operation;
pub mod publish;
pub mod test;
pub mod update_override;
pub mod weight_recompute;

// Re-export individual tools
pub use applicable_variants::ApplicableVariantsTool;
pub use bulk_operation::BulkOperationTool;
pub use publish::PublishTool;
pub use test::TestTool;
pub use update_override::UpdateOverrideTool;
pub use weight_recompute::WeightRecomputeTool;

// Compose all misc tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    MiscTools,
    ApplicableVariantsTool,
    BulkOperationTool,
    PublishTool,
    TestTool,
    UpdateOverrideTool,
    WeightRecomputeTool
);