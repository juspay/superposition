use aws_smithy_types::Document;
use std::collections::HashMap;

/// Convert a `serde_json::Value` into an `aws_smithy_types::Document`.
pub fn json_to_doc(val: serde_json::Value) -> Document {
    match val {
        serde_json::Value::Null => Document::Null,
        serde_json::Value::Bool(b) => Document::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Document::Number(aws_smithy_types::Number::NegInt(i))
            } else if let Some(f) = n.as_f64() {
                Document::Number(aws_smithy_types::Number::Float(f))
            } else {
                Document::Null
            }
        }
        serde_json::Value::String(s) => Document::String(s),
        serde_json::Value::Array(arr) => {
            Document::Array(arr.into_iter().map(json_to_doc).collect())
        }
        serde_json::Value::Object(obj) => {
            let map: HashMap<String, Document> =
                obj.into_iter().map(|(k, v)| (k, json_to_doc(v))).collect();
            Document::Object(map)
        }
    }
}

/// Convert an `aws_smithy_types::Document` into a `serde_json::Value`.
pub fn doc_to_json(doc: &Document) -> serde_json::Value {
    match doc {
        Document::Null => serde_json::Value::Null,
        Document::Bool(b) => serde_json::Value::Bool(*b),
        Document::Number(n) => match n {
            aws_smithy_types::Number::PosInt(i) => serde_json::Value::Number((*i).into()),
            aws_smithy_types::Number::NegInt(i) => serde_json::Value::Number((*i).into()),
            aws_smithy_types::Number::Float(f) => serde_json::json!(*f),
        },
        Document::String(s) => serde_json::Value::String(s.clone()),
        Document::Array(arr) => {
            serde_json::Value::Array(arr.iter().map(doc_to_json).collect())
        }
        Document::Object(obj) => {
            let map: serde_json::Map<String, serde_json::Value> = obj
                .iter()
                .map(|(k, v)| (k.clone(), doc_to_json(v)))
                .collect();
            serde_json::Value::Object(map)
        }
    }
}

/// Convert a `HashMap<String, Document>` into a `serde_json::Value::Object`.
pub fn doc_map_to_json(map: &HashMap<String, Document>) -> serde_json::Value {
    let obj: serde_json::Map<String, serde_json::Value> = map
        .iter()
        .map(|(k, v)| (k.clone(), doc_to_json(v)))
        .collect();
    serde_json::Value::Object(obj)
}

/// Convert a `serde_json::Value` (expected Object) into `HashMap<String, Document>`.
pub fn json_to_doc_map(
    val: serde_json::Value,
) -> Result<HashMap<String, Document>, String> {
    match val {
        serde_json::Value::Object(obj) => {
            Ok(obj.into_iter().map(|(k, v)| (k, json_to_doc(v))).collect())
        }
        _ => Err("Expected a JSON object".to_string()),
    }
}

/// Format a `aws_smithy_types::DateTime` as an ISO 8601 string.
pub fn format_datetime(dt: &aws_smithy_types::DateTime) -> String {
    dt.fmt(aws_smithy_types::date_time::Format::DateTime)
        .unwrap_or_else(|_| "unknown".to_string())
}

/// Create an MCP error result from a string message.
pub fn mcp_err(msg: impl std::fmt::Display) -> rmcp::ErrorData {
    rmcp::ErrorData::internal_error(msg.to_string(), None)
}

macro_rules! context_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "id": $r.id,
            "value": $crate::helpers::doc_map_to_json(&$r.value),
            "override": $crate::helpers::doc_map_to_json(&$r.r#override),
            "override_id": $r.override_id,
            "weight": $r.weight,
            "description": $r.description,
            "change_reason": $r.change_reason,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "created_by": $r.created_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
            "last_modified_by": $r.last_modified_by,
        })
    }}
}

macro_rules! default_config_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "key": $r.key,
            "value": $crate::helpers::doc_to_json(&$r.value),
            "schema": $crate::helpers::doc_map_to_json(&$r.schema),
            "description": $r.description,
            "change_reason": $r.change_reason,
            "value_validation_function_name": $r.value_validation_function_name,
            "value_compute_function_name": $r.value_compute_function_name,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "created_by": $r.created_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
            "last_modified_by": $r.last_modified_by,
        })
    }}
}

macro_rules! dimension_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "dimension": $r.dimension,
            "position": $r.position,
            "schema": $crate::helpers::doc_map_to_json(&$r.schema),
            "description": $r.description,
            "change_reason": $r.change_reason,
            "dimension_type": format!("{:?}", $r.dimension_type),
            "mandatory": $r.mandatory,
            "dependency_graph": $r.dependency_graph,
            "value_validation_function_name": $r.value_validation_function_name,
            "value_compute_function_name": $r.value_compute_function_name,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "created_by": $r.created_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
            "last_modified_by": $r.last_modified_by,
        })
    }}
}

macro_rules! experiment_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "id": $r.id,
            "name": $r.name,
            "status": format!("{:?}", $r.status),
            "experiment_type": format!("{:?}", $r.experiment_type),
            "traffic_percentage": $r.traffic_percentage,
            "context": $crate::helpers::doc_map_to_json(&$r.context),
            "description": $r.description,
            "change_reason": $r.change_reason,
            "override_keys": $r.override_keys,
            "variants": $r.variants.iter().map(|v| serde_json::json!({
                "id": v.id,
                "variant_type": format!("{:?}", v.variant_type),
                "overrides": $crate::helpers::doc_map_to_json(&v.overrides),
                "context_id": v.context_id,
                "override_id": v.override_id,
            })).collect::<Vec<_>>(),
            "chosen_variant": $r.chosen_variant,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "created_by": $r.created_by,
            "last_modified": $crate::helpers::format_datetime(&$r.last_modified),
            "last_modified_by": $r.last_modified_by,
            "experiment_group_id": $r.experiment_group_id,
        })
    }}
}

macro_rules! experiment_group_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "id": $r.id,
            "name": $r.name,
            "description": $r.description,
            "change_reason": $r.change_reason,
            "context": $crate::helpers::doc_map_to_json(&$r.context),
            "context_hash": $r.context_hash,
            "traffic_percentage": $r.traffic_percentage,
            "member_experiment_ids": $r.member_experiment_ids,
            "group_type": format!("{:?}", $r.group_type),
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "created_by": $r.created_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
            "last_modified_by": $r.last_modified_by,
        })
    }}
}

macro_rules! function_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "function_name": $r.function_name,
            "function_type": format!("{:?}", $r.function_type),
            "published_code": $r.published_code,
            "draft_code": $r.draft_code,
            "published_runtime_version": $r.published_runtime_version.as_ref().map(|v| format!("{:?}", v)),
            "draft_runtime_version": format!("{:?}", $r.draft_runtime_version),
            "published_at": $r.published_at.as_ref().map($crate::helpers::format_datetime),
            "draft_edited_at": $crate::helpers::format_datetime(&$r.draft_edited_at),
            "published_by": $r.published_by,
            "draft_edited_by": $r.draft_edited_by,
            "description": $r.description,
            "change_reason": $r.change_reason,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
            "last_modified_by": $r.last_modified_by,
        })
    }}
}

macro_rules! organisation_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "id": $r.id,
            "name": $r.name,
            "admin_email": $r.admin_email,
            "status": format!("{:?}", $r.status),
            "country_code": $r.country_code,
            "contact_email": $r.contact_email,
            "contact_phone": $r.contact_phone,
            "sector": $r.sector,
            "created_by": $r.created_by,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "updated_at": $crate::helpers::format_datetime(&$r.updated_at),
            "updated_by": $r.updated_by,
        })
    }}
}

macro_rules! workspace_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "workspace_name": $r.workspace_name,
            "organisation_id": $r.organisation_id,
            "organisation_name": $r.organisation_name,
            "workspace_schema_name": $r.workspace_schema_name,
            "workspace_status": format!("{:?}", $r.workspace_status),
            "workspace_admin_email": $r.workspace_admin_email,
            "config_version": $r.config_version,
            "mandatory_dimensions": $r.mandatory_dimensions,
            "created_by": $r.created_by,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "last_modified_by": $r.last_modified_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
        })
    }}
}

macro_rules! type_template_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "type_name": $r.type_name,
            "type_schema": $crate::helpers::doc_map_to_json(&$r.type_schema),
            "description": $r.description,
            "change_reason": $r.change_reason,
            "created_by": $r.created_by,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "last_modified_by": $r.last_modified_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
        })
    }}
}

macro_rules! audit_log_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "id": $r.id,
            "table_name": $r.table_name,
            "user_name": $r.user_name,
            "timestamp": $crate::helpers::format_datetime(&$r.timestamp),
            "action": format!("{:?}", $r.action),
            "original_data": $r.original_data.as_ref().map($crate::helpers::doc_to_json),
            "new_data": $r.new_data.as_ref().map($crate::helpers::doc_to_json),
            "query": $r.query,
        })
    }}
}

macro_rules! variable_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "name": $r.name,
            "value": $r.value,
            "description": $r.description,
            "change_reason": $r.change_reason,
            "created_by": $r.created_by,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "last_modified_by": $r.last_modified_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
        })
    }}
}

macro_rules! webhook_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "name": $r.name,
            "description": $r.description,
            "enabled": $r.enabled,
            "url": $r.url,
            "method": format!("{:?}", $r.method),
            "version": format!("{:?}", $r.version),
            "events": $r.events,
            "max_retries": $r.max_retries,
            "custom_headers": $r.custom_headers.as_ref().map($crate::helpers::doc_map_to_json),
            "last_triggered_at": $r.last_triggered_at.as_ref().map($crate::helpers::format_datetime),
            "change_reason": $r.change_reason,
            "created_by": $r.created_by,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "last_modified_by": $r.last_modified_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
        })
    }}
}

macro_rules! secret_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "name": $r.name,
            "description": $r.description,
            "change_reason": $r.change_reason,
            "created_by": $r.created_by,
            "created_at": $crate::helpers::format_datetime(&$r.created_at),
            "last_modified_by": $r.last_modified_by,
            "last_modified_at": $crate::helpers::format_datetime(&$r.last_modified_at),
        })
    }}
}

pub(crate) use audit_log_to_json;
pub(crate) use context_to_json;
pub(crate) use default_config_to_json;
pub(crate) use dimension_to_json;
pub(crate) use experiment_group_to_json;
pub(crate) use experiment_to_json;
pub(crate) use function_to_json;
pub(crate) use organisation_to_json;
pub(crate) use secret_to_json;
pub(crate) use type_template_to_json;
pub(crate) use variable_to_json;
pub(crate) use webhook_to_json;
pub(crate) use workspace_to_json;
