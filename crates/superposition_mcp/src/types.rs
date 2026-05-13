use serde::{Serialize, Deserialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GroupType {
    #[serde(rename = "USER_CREATED")]
    UserCreated,
    #[serde(rename = "SYSTEM_GENERATED")]
    SystemGenerated,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VariantType {
    #[serde(rename = "CONTROL")]
    Control,
    #[serde(rename = "EXPERIMENTAL")]
    Experimental,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExperimentType {
    #[serde(rename = "DEFAULT")]
    Default,
    #[serde(rename = "DELETE_OVERRIDES")]
    DeleteOverrides,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExperimentStatusType {
    #[serde(rename = "CREATED")]
    Created,
    #[serde(rename = "CONCLUDED")]
    Concluded,
    #[serde(rename = "INPROGRESS")]
    Inprogress,
    #[serde(rename = "DISCARDED")]
    Discarded,
    #[serde(rename = "PAUSED")]
    Paused,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FunctionRuntimeVersion {
    #[serde(rename = "1.0")]
    V1,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FunctionTypes {
    #[serde(rename = "VALUE_VALIDATION")]
    ValueValidation,
    #[serde(rename = "VALUE_COMPUTE")]
    ValueCompute,
    #[serde(rename = "CONTEXT_VALIDATION")]
    ContextValidation,
    #[serde(rename = "CHANGE_REASON_VALIDATION")]
    ChangeReasonValidation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OrgStatus {
    #[serde(rename = "Active")]
    Active,
    #[serde(rename = "Inactive")]
    Inactive,
    #[serde(rename = "PendingKyb")]
    PendingKyb,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HttpMethod {
    #[serde(rename = "GET")]
    Get,
    #[serde(rename = "POST")]
    Post,
    #[serde(rename = "PUT")]
    Put,
    #[serde(rename = "PATCH")]
    Patch,
    #[serde(rename = "DELETE")]
    Delete,
    #[serde(rename = "HEAD")]
    Head,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Version {
    #[serde(rename = "V1")]
    V1,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WorkspaceStatus {
    #[serde(rename = "ENABLED")]
    Enabled,
    #[serde(rename = "DISABLED")]
    Disabled,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DimensionMatchStrategy {
    #[serde(rename = "exact")]
    Exact,
    #[serde(rename = "subset")]
    Subset,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MergeStrategy {
    #[serde(rename = "MERGE")]
    Merge,
    #[serde(rename = "REPLACE")]
    Replace,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AuditAction {
    #[serde(rename = "INSERT")]
    Insert,
    #[serde(rename = "UPDATE")]
    Update,
    #[serde(rename = "DELETE")]
    Delete,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SortBy {
    #[serde(rename = "desc")]
    Desc,
    #[serde(rename = "asc")]
    Asc,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ContextFilterSortOn {
    #[serde(rename = "last_modified_at")]
    LastModifiedAt,
    #[serde(rename = "created_at")]
    CreatedAt,
    #[serde(rename = "weight")]
    Weight,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExperimentSortOn {
    #[serde(rename = "last_modified_at")]
    LastModifiedAt,
    #[serde(rename = "created_at")]
    CreatedAt,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExperimentGroupSortOn {
    #[serde(rename = "name")]
    Name,
    #[serde(rename = "created_at")]
    CreatedAt,
    #[serde(rename = "last_modified_at")]
    LastModifiedAt,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SecretSortOn {
    #[serde(rename = "name")]
    Name,
    #[serde(rename = "created_at")]
    CreatedAt,
    #[serde(rename = "last_modified_at")]
    LastModifiedAt,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VariableSortOn {
    #[serde(rename = "name")]
    Name,
    #[serde(rename = "created_at")]
    CreatedAt,
    #[serde(rename = "last_modified_at")]
    LastModifiedAt,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Stage {
    #[serde(rename = "draft")]
    Draft,
    #[serde(rename = "published")]
    Published,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ContextAction {
    #[serde(rename = "PUT")]
    Put(ContextPut),
    #[serde(rename = "REPLACE")]
    Replace(UpdateContextOverrideRequest),
    #[serde(rename = "DELETE")]
    Delete(String),
    #[serde(rename = "MOVE")]
    Move(ContextMoveBulkRequest),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ContextIdentifier {
    #[serde(rename = "id")]
    Id(String),
    #[serde(rename = "context")]
    Context(HashMap<String, serde_json::Value>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ContextActionOut {
    #[serde(rename = "PUT")]
    Put(ContextResponse),
    #[serde(rename = "REPLACE")]
    Replace(ContextResponse),
    #[serde(rename = "DELETE")]
    Delete(String),
    #[serde(rename = "MOVE")]
    Move(ContextResponse),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DimensionType {
    #[serde(rename = "REGULAR")]
    Regular,
    #[serde(rename = "LOCAL_COHORT")]
    LocalCohort(String),
    #[serde(rename = "REMOTE_COHORT")]
    RemoteCohort(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FunctionExecutionRequest {
    #[serde(rename = "value_validate")]
    ValueValidate(ValueValidationFunctionRequest),
    #[serde(rename = "value_compute")]
    ValueCompute(ValueComputeFunctionRequest),
    #[serde(rename = "context_validate")]
    ContextValidate(ContextValidationFunctionRequest),
    #[serde(rename = "change_reason_validate")]
    ChangeReasonValidate(ChangeReasonValidationFunctionRequest),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModifyMembersToGroupRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub change_reason: String,
    pub member_experiment_ids: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExperimentGroupResponse {
    pub id: String,
    pub context_hash: String,
    pub name: String,
    pub description: String,
    pub change_reason: String,
    pub context: HashMap<String, serde_json::Value>,
    pub traffic_percentage: i32,
    pub member_experiment_ids: Vec<String>,
    pub created_at: String,
    pub created_by: String,
    pub last_modified_at: String,
    pub last_modified_by: String,
    pub buckets: Vec<Bucket>,
    pub group_type: GroupType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bucket {
    pub experiment_id: String,
    pub variant_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApplicableVariantsInput {
    pub workspace_id: String,
    pub org_id: String,
    pub context: HashMap<String, serde_json::Value>,
    pub identifier: String,
    pub prefix: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApplicableVariantsOutput {
    pub data: Vec<Variant>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variant {
    pub id: String,
    pub variant_type: VariantType,
    pub context_id: Option<String>,
    pub override_id: Option<String>,
    pub overrides: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BulkOperationInput {
    pub workspace_id: String,
    pub org_id: String,
    pub config_tags: Option<String>,
    pub operations: Vec<ContextAction>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextPut {
    pub context: HashMap<String, serde_json::Value>,
    #[serde(rename = "override")]
    pub r#override: HashMap<String, serde_json::Value>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateContextOverrideRequest {
    pub context: ContextIdentifier,
    #[serde(rename = "override")]
    pub r#override: HashMap<String, serde_json::Value>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextMoveBulkRequest {
    pub id: String,
    pub request: ContextMove,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextMove {
    pub context: HashMap<String, serde_json::Value>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BulkOperationOutput {
    pub output: Vec<ContextActionOut>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextResponse {
    pub id: String,
    pub value: HashMap<String, serde_json::Value>,
    #[serde(rename = "override")]
    pub r#override: HashMap<String, serde_json::Value>,
    pub override_id: String,
    pub weight: String,
    pub description: String,
    pub change_reason: String,
    pub created_at: String,
    pub created_by: String,
    pub last_modified_at: String,
    pub last_modified_by: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConcludeExperimentInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub chosen_variant: String,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExperimentResponse {
    pub id: String,
    pub created_at: String,
    pub created_by: String,
    pub last_modified: String,
    pub name: String,
    pub experiment_type: ExperimentType,
    pub override_keys: Vec<String>,
    pub status: ExperimentStatusType,
    pub traffic_percentage: i32,
    pub context: HashMap<String, serde_json::Value>,
    pub variants: Vec<Variant>,
    pub last_modified_by: String,
    pub chosen_variant: Option<String>,
    pub description: String,
    pub change_reason: String,
    pub started_at: Option<String>,
    pub started_by: Option<String>,
    pub metrics_url: Option<String>,
    pub metrics: Option<serde_json::Value>,
    pub experiment_group_id: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateContextInput {
    pub workspace_id: String,
    pub org_id: String,
    pub config_tags: Option<String>,
    pub request: ContextPut,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateDefaultConfigInput {
    pub key: String,
    pub value: serde_json::Value,
    pub schema: HashMap<String, serde_json::Value>,
    pub description: String,
    pub change_reason: String,
    pub value_validation_function_name: Option<String>,
    pub value_compute_function_name: Option<String>,
    pub workspace_id: String,
    pub org_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefaultConfigResponse {
    pub key: String,
    pub value: serde_json::Value,
    pub schema: HashMap<String, serde_json::Value>,
    pub description: String,
    pub change_reason: String,
    pub value_validation_function_name: Option<String>,
    pub value_compute_function_name: Option<String>,
    pub created_at: String,
    pub created_by: String,
    pub last_modified_at: String,
    pub last_modified_by: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateDimensionInput {
    pub workspace_id: String,
    pub org_id: String,
    pub dimension: String,
    pub position: i32,
    pub schema: HashMap<String, serde_json::Value>,
    pub value_validation_function_name: Option<String>,
    pub description: String,
    pub change_reason: String,
    pub dimension_type: Option<DimensionType>,
    pub value_compute_function_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Unit {
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DimensionResponse {
    pub dimension: String,
    pub position: i32,
    pub schema: HashMap<String, serde_json::Value>,
    pub value_validation_function_name: Option<String>,
    pub description: String,
    pub change_reason: String,
    pub last_modified_at: String,
    pub last_modified_by: String,
    pub created_at: String,
    pub created_by: String,
    pub dependency_graph: HashMap<String, Vec<String>>,
    pub dimension_type: DimensionType,
    pub value_compute_function_name: Option<String>,
    pub mandatory: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateExperimentRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
    pub experiment_type: Option<ExperimentType>,
    pub context: HashMap<String, serde_json::Value>,
    pub variants: Vec<Variant>,
    pub description: String,
    pub change_reason: String,
    pub metrics: Option<serde_json::Value>,
    pub experiment_group_id: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateExperimentGroupRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
    pub description: String,
    pub change_reason: String,
    pub context: HashMap<String, serde_json::Value>,
    pub traffic_percentage: i32,
    pub member_experiment_ids: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateFunctionRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub function_name: String,
    pub description: String,
    pub change_reason: String,
    pub function: String,
    pub runtime_version: FunctionRuntimeVersion,
    pub function_type: FunctionTypes,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionResponse {
    pub function_name: String,
    pub published_code: Option<String>,
    pub draft_code: String,
    pub published_runtime_version: Option<FunctionRuntimeVersion>,
    pub draft_runtime_version: FunctionRuntimeVersion,
    pub published_at: Option<String>,
    pub draft_edited_at: String,
    pub published_by: Option<String>,
    pub draft_edited_by: String,
    pub last_modified_at: String,
    pub last_modified_by: String,
    pub change_reason: String,
    pub description: String,
    pub function_type: FunctionTypes,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateOrganisationRequest {
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub admin_email: String,
    pub sector: Option<String>,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrganisationResponse {
    pub id: String,
    pub name: String,
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub created_by: String,
    pub admin_email: String,
    pub status: OrgStatus,
    pub sector: Option<String>,
    pub created_at: String,
    pub updated_at: String,
    pub updated_by: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateSecretInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
    pub value: String,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecretResponse {
    pub name: String,
    pub description: String,
    pub change_reason: String,
    pub created_by: String,
    pub created_at: String,
    pub last_modified_by: String,
    pub last_modified_at: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateTypeTemplatesRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub type_name: String,
    pub type_schema: HashMap<String, serde_json::Value>,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeTemplatesResponse {
    pub type_name: String,
    pub type_schema: HashMap<String, serde_json::Value>,
    pub description: String,
    pub change_reason: String,
    pub created_by: String,
    pub created_at: String,
    pub last_modified_at: String,
    pub last_modified_by: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateVariableInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
    pub value: String,
    pub description: String,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableResponse {
    pub name: String,
    pub value: String,
    pub description: String,
    pub change_reason: String,
    pub created_by: String,
    pub created_at: String,
    pub last_modified_by: String,
    pub last_modified_at: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateWebhookInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
    pub description: String,
    pub enabled: bool,
    pub url: String,
    pub method: HttpMethod,
    pub version: Option<Version>,
    pub custom_headers: Option<HashMap<String, serde_json::Value>>,
    pub events: Vec<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WebhookResponse {
    pub name: String,
    pub description: String,
    pub enabled: bool,
    pub url: String,
    pub method: HttpMethod,
    pub version: Version,
    pub custom_headers: Option<HashMap<String, serde_json::Value>>,
    pub events: Vec<String>,
    pub max_retries: i32,
    pub last_triggered_at: Option<String>,
    pub change_reason: String,
    pub created_by: String,
    pub created_at: String,
    pub last_modified_by: String,
    pub last_modified_at: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateWorkspaceRequest {
    pub org_id: String,
    pub workspace_admin_email: String,
    pub workspace_name: String,
    pub workspace_status: Option<WorkspaceStatus>,
    pub metrics: Option<serde_json::Value>,
    pub allow_experiment_self_approval: Option<bool>,
    pub auto_populate_control: Option<bool>,
    pub enable_context_validation: Option<bool>,
    pub enable_change_reason_validation: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceResponse {
    pub workspace_name: String,
    pub organisation_id: String,
    pub organisation_name: String,
    pub workspace_schema_name: String,
    pub workspace_status: WorkspaceStatus,
    pub workspace_admin_email: String,
    pub config_version: Option<String>,
    pub created_by: String,
    pub last_modified_by: String,
    pub last_modified_at: String,
    pub created_at: String,
    pub mandatory_dimensions: Option<Vec<String>>,
    pub metrics: serde_json::Value,
    pub allow_experiment_self_approval: bool,
    pub auto_populate_control: bool,
    pub enable_context_validation: bool,
    pub enable_change_reason_validation: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteContextInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub config_tags: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteDefaultConfigInput {
    pub workspace_id: String,
    pub org_id: String,
    pub key: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteDimensionInput {
    pub workspace_id: String,
    pub org_id: String,
    pub dimension: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteExperimentGroupInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteFunctionInput {
    pub workspace_id: String,
    pub org_id: String,
    pub function_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteSecretInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteTypeTemplatesInput {
    pub workspace_id: String,
    pub org_id: String,
    pub type_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteVariableInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeleteWebhookInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscardExperimentInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetConfigInput {
    pub workspace_id: String,
    pub org_id: String,
    pub prefix: Option<Vec<String>>,
    pub version: Option<String>,
    pub if_modified_since: Option<String>,
    pub context: Option<HashMap<String, serde_json::Value>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetConfigOutput {
    pub contexts: Vec<ContextPartial>,
    pub overrides: HashMap<String, HashMap<String, serde_json::Value>>,
    pub default_configs: HashMap<String, serde_json::Value>,
    pub dimensions: HashMap<String, DimensionInfo>,
    pub version: String,
    pub last_modified: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextPartial {
    pub id: String,
    pub condition: HashMap<String, serde_json::Value>,
    pub priority: i32,
    pub weight: i32,
    pub override_with_keys: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DimensionInfo {
    pub schema: HashMap<String, serde_json::Value>,
    pub position: i32,
    pub dimension_type: DimensionType,
    pub dependency_graph: HashMap<String, Vec<String>>,
    pub value_compute_function_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetConfigJsonInput {
    pub workspace_id: String,
    pub org_id: String,
    pub if_modified_since: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetConfigJsonOutput {
    pub json_config: String,
    pub last_modified: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetConfigTomlInput {
    pub workspace_id: String,
    pub org_id: String,
    pub if_modified_since: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetConfigTomlOutput {
    pub toml_config: String,
    pub last_modified: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetContextInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetContextFromConditionInput {
    pub workspace_id: String,
    pub org_id: String,
    pub context: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetDefaultConfigInput {
    pub workspace_id: String,
    pub org_id: String,
    pub key: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetDimensionInput {
    pub workspace_id: String,
    pub org_id: String,
    pub dimension: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetExperimentInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetExperimentConfigInput {
    pub workspace_id: String,
    pub org_id: String,
    pub if_modified_since: Option<String>,
    pub prefix: Option<Vec<String>>,
    pub context: Option<HashMap<String, serde_json::Value>>,
    pub dimension_match_strategy: Option<DimensionMatchStrategy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetExperimentConfigOutput {
    pub last_modified: String,
    pub experiments: Vec<ExperimentResponse>,
    pub experiment_groups: Vec<ExperimentGroupResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetExperimentGroupInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetFunctionInput {
    pub workspace_id: String,
    pub org_id: String,
    pub function_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetOrganisationInput {
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetResolvedConfigInput {
    pub workspace_id: String,
    pub org_id: String,
    pub prefix: Option<Vec<String>>,
    pub version: Option<String>,
    pub show_reasoning: Option<bool>,
    pub merge_strategy: Option<MergeStrategy>,
    pub context_id: Option<String>,
    pub resolve_remote: Option<bool>,
    pub context: Option<HashMap<String, serde_json::Value>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetResolvedConfigOutput {
    pub config: serde_json::Value,
    pub version: String,
    pub last_modified: String,
    pub audit_id: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetResolvedConfigWithIdentifierInput {
    pub workspace_id: String,
    pub org_id: String,
    pub prefix: Option<Vec<String>>,
    pub version: Option<String>,
    pub show_reasoning: Option<bool>,
    pub merge_strategy: Option<MergeStrategy>,
    pub context_id: Option<String>,
    pub resolve_remote: Option<bool>,
    pub context: Option<HashMap<String, serde_json::Value>>,
    pub identifier: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetResolvedConfigWithIdentifierOutput {
    pub config: serde_json::Value,
    pub version: String,
    pub last_modified: String,
    pub audit_id: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetSecretInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetTypeTemplateInput {
    pub workspace_id: String,
    pub org_id: String,
    pub type_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetTypeTemplatesListInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetTypeTemplatesListOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<TypeTemplatesResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetVariableInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetVersionInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetVersionResponse {
    pub id: String,
    pub config: ConfigData,
    pub config_hash: String,
    pub created_at: String,
    pub description: String,
    pub tags: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigData {
    pub contexts: Vec<ContextPartial>,
    pub overrides: HashMap<String, HashMap<String, serde_json::Value>>,
    pub default_configs: HashMap<String, serde_json::Value>,
    pub dimensions: HashMap<String, DimensionInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetWebhookInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetWebhookByEventInput {
    pub workspace_id: String,
    pub org_id: String,
    pub event: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetWorkspaceInput {
    pub org_id: String,
    pub workspace_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListAuditLogsInput {
    pub workspace_id: String,
    pub org_id: String,
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub from_date: Option<String>,
    pub to_date: Option<String>,
    pub tables: Option<Vec<String>>,
    pub action: Option<Vec<AuditAction>>,
    pub username: Option<String>,
    pub sort_by: Option<SortBy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListAuditLogsOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<AuditLogFull>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditLogFull {
    pub id: String,
    pub table_name: String,
    pub user_name: String,
    pub timestamp: String,
    pub action: AuditAction,
    pub original_data: Option<serde_json::Value>,
    pub new_data: Option<serde_json::Value>,
    pub query: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListContextsInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
    pub prefix: Option<Vec<String>>,
    pub sort_on: Option<ContextFilterSortOn>,
    pub sort_by: Option<SortBy>,
    pub created_by: Option<Vec<String>>,
    pub last_modified_by: Option<Vec<String>>,
    pub plaintext: Option<String>,
    pub dimension_match_strategy: Option<DimensionMatchStrategy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListContextsOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<ContextResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListDefaultConfigsInput {
    pub workspace_id: String,
    pub org_id: String,
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListDefaultConfigsOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<DefaultConfigResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListDimensionsInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListDimensionsOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<DimensionResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListExperimentInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
    pub if_modified_since: Option<String>,
    pub status: Option<Vec<ExperimentStatusType>>,
    pub from_date: Option<String>,
    pub to_date: Option<String>,
    pub experiment_name: Option<String>,
    pub experiment_ids: Option<Vec<String>>,
    pub experiment_group_ids: Option<Vec<String>>,
    pub created_by: Option<Vec<String>>,
    pub sort_on: Option<ExperimentSortOn>,
    pub sort_by: Option<SortBy>,
    pub global_experiments_only: Option<bool>,
    pub dimension_match_strategy: Option<DimensionMatchStrategy>,
    pub prefix: Option<Vec<String>>,
    pub context: Option<HashMap<String, serde_json::Value>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListExperimentOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<ExperimentResponse>,
    pub last_modified: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListExperimentGroupsInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
    pub if_modified_since: Option<String>,
    pub name: Option<String>,
    pub created_by: Option<String>,
    pub last_modified_by: Option<String>,
    pub sort_on: Option<ExperimentGroupSortOn>,
    pub sort_by: Option<SortBy>,
    pub group_type: Option<Vec<GroupType>>,
    pub dimension_match_strategy: Option<DimensionMatchStrategy>,
    pub context: Option<HashMap<String, serde_json::Value>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListExperimentGroupsOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<ExperimentGroupResponse>,
    pub last_modified: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListFunctionInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
    pub function_type: Option<Vec<FunctionTypes>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListFunctionOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<FunctionResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListOrganisationInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListOrganisationOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<OrganisationResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListSecretsInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
    pub name: Option<Vec<String>>,
    pub created_by: Option<Vec<String>>,
    pub last_modified_by: Option<Vec<String>>,
    pub sort_on: Option<SecretSortOn>,
    pub sort_by: Option<SortBy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListSecretsOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<SecretResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListVariablesInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
    pub name: Option<Vec<String>>,
    pub created_by: Option<Vec<String>>,
    pub last_modified_by: Option<Vec<String>>,
    pub sort_on: Option<VariableSortOn>,
    pub sort_by: Option<SortBy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListVariablesOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<VariableResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListVersionsInput {
    pub workspace_id: String,
    pub org_id: String,
    pub count: Option<i32>,
    pub page: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListVersionsOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<ListVersionsMember>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListVersionsMember {
    pub id: String,
    pub config: ConfigData,
    pub created_at: String,
    pub description: String,
    pub tags: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListWebhookInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub workspace_id: String,
    pub org_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListWebhookOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<WebhookResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListWorkspaceInput {
    pub count: Option<i32>,
    pub page: Option<i32>,
    pub all: Option<bool>,
    pub org_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ListWorkspaceOutput {
    pub total_pages: i32,
    pub total_items: i32,
    pub data: Vec<WorkspaceResponse>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceSelectorRequest {
    pub org_id: String,
    pub workspace_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MoveContextInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub request: ContextMove,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PauseExperimentInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PublishInput {
    pub workspace_id: String,
    pub org_id: String,
    pub function_name: String,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RampExperimentInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub change_reason: String,
    pub traffic_percentage: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResumeExperimentInput {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RotateMasterEncryptionKeyOutput {
    pub workspaces_rotated: i64,
    pub total_secrets_re_encrypted: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RotateWorkspaceEncryptionKeyOutput {
    pub total_secrets_re_encrypted: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestInput {
    pub workspace_id: String,
    pub org_id: String,
    pub function_name: String,
    pub stage: Stage,
    pub request: FunctionExecutionRequest,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueValidationFunctionRequest {
    pub key: String,
    pub value: serde_json::Value,
    #[serde(rename = "type")]
    pub r#type: String,
    pub environment: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueComputeFunctionRequest {
    pub name: String,
    pub prefix: String,
    #[serde(rename = "type")]
    pub r#type: String,
    pub environment: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContextValidationFunctionRequest {
    pub environment: serde_json::Value,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChangeReasonValidationFunctionRequest {
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionExecutionResponse {
    pub fn_output: serde_json::Value,
    pub stdout: String,
    pub function_type: FunctionTypes,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateDefaultConfigInput {
    pub workspace_id: String,
    pub org_id: String,
    pub key: String,
    pub change_reason: String,
    pub value: Option<serde_json::Value>,
    pub schema: Option<HashMap<String, serde_json::Value>>,
    pub value_validation_function_name: Option<String>,
    pub description: Option<String>,
    pub value_compute_function_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateDimensionInput {
    pub workspace_id: String,
    pub org_id: String,
    pub dimension: String,
    pub schema: Option<HashMap<String, serde_json::Value>>,
    pub position: Option<i32>,
    pub value_validation_function_name: Option<String>,
    pub description: Option<String>,
    pub change_reason: String,
    pub value_compute_function_name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateExperimentGroupRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub change_reason: String,
    pub description: Option<String>,
    pub traffic_percentage: Option<i32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateFunctionRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub function_name: String,
    pub description: Option<String>,
    pub change_reason: String,
    pub function: Option<String>,
    pub runtime_version: Option<FunctionRuntimeVersion>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateOrganisationRequest {
    pub country_code: Option<String>,
    pub contact_email: Option<String>,
    pub contact_phone: Option<String>,
    pub admin_email: Option<String>,
    pub sector: Option<String>,
    pub id: String,
    pub status: Option<OrgStatus>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateOverrideInput {
    pub workspace_id: String,
    pub org_id: String,
    pub config_tags: Option<String>,
    pub request: UpdateContextOverrideRequest,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateOverrideRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub id: String,
    pub variant_list: Vec<VariantUpdateRequest>,
    pub description: Option<String>,
    pub change_reason: String,
    pub metrics: Option<serde_json::Value>,
    pub experiment_group_id: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariantUpdateRequest {
    pub id: String,
    pub overrides: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateSecretInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
    pub value: Option<String>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateTypeTemplatesRequest {
    pub workspace_id: String,
    pub org_id: String,
    pub type_name: String,
    pub type_schema: HashMap<String, serde_json::Value>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateVariableInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
    pub value: Option<String>,
    pub description: Option<String>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateWebhookInput {
    pub workspace_id: String,
    pub org_id: String,
    pub name: String,
    pub description: Option<String>,
    pub enabled: Option<bool>,
    pub url: Option<String>,
    pub method: Option<HttpMethod>,
    pub version: Option<Version>,
    pub custom_headers: Option<HashMap<String, serde_json::Value>>,
    pub events: Option<Vec<String>>,
    pub change_reason: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateWorkspaceRequest {
    pub org_id: String,
    pub workspace_name: String,
    pub workspace_admin_email: Option<String>,
    pub config_version: Option<String>,
    pub mandatory_dimensions: Option<Vec<String>>,
    pub workspace_status: Option<WorkspaceStatus>,
    pub metrics: Option<serde_json::Value>,
    pub allow_experiment_self_approval: Option<bool>,
    pub auto_populate_control: Option<bool>,
    pub enable_context_validation: Option<bool>,
    pub enable_change_reason_validation: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidateContextInput {
    pub workspace_id: String,
    pub org_id: String,
    pub context: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WeightRecomputeInput {
    pub workspace_id: String,
    pub org_id: String,
    pub config_tags: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WeightRecomputeOutput {
    pub data: Option<Vec<WeightRecomputeResponse>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WeightRecomputeResponse {
    pub id: String,
    pub condition: HashMap<String, serde_json::Value>,
    pub old_weight: String,
    pub new_weight: String,
}

