// Individual tool modules
pub mod create_experiment;
pub mod list_experiment;
pub mod get_experiment;
pub mod conclude_experiment;
pub mod discard_experiment;
pub mod pause_experiment;
pub mod resume_experiment;
pub mod ramp_experiment;
pub mod create_experiment_group;
pub mod get_experiment_group;
pub mod list_experiment_groups;
pub mod update_experiment_group;
pub mod delete_experiment_group;
pub mod update_overrides_experiment;

// Re-export individual tools
pub use create_experiment::CreateExperimentTool;
pub use list_experiment::ListExperimentTool;
pub use get_experiment::GetExperimentTool;
pub use conclude_experiment::ConcludeExperimentTool;
pub use discard_experiment::DiscardExperimentTool;
pub use pause_experiment::PauseExperimentTool;
pub use resume_experiment::ResumeExperimentTool;
pub use ramp_experiment::RampExperimentTool;
pub use create_experiment_group::CreateExperimentGroupTool;
pub use get_experiment_group::GetExperimentGroupTool;
pub use list_experiment_groups::ListExperimentGroupsTool;
pub use update_experiment_group::UpdateExperimentGroupTool;
pub use delete_experiment_group::DeleteExperimentGroupTool;
pub use update_overrides_experiment::UpdateOverridesExperimentTool;

// Compose experiment tools using the macro
use crate::compose_tools_group;
use crate::tools::MCPTool;

compose_tools_group!(
    ExperimentTools,
    CreateExperimentTool,
    ListExperimentTool,
    GetExperimentTool,
    ConcludeExperimentTool,
    DiscardExperimentTool,
    PauseExperimentTool,
    ResumeExperimentTool,
    RampExperimentTool,
    CreateExperimentGroupTool,
    GetExperimentGroupTool,
    ListExperimentGroupsTool,
    UpdateExperimentGroupTool,
    DeleteExperimentGroupTool,
    UpdateOverridesExperimentTool
);
