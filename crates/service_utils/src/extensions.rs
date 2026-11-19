use actix_web::HttpRequest;

use crate::helpers::{MAX_COMPONENT_LEN, validate_and_sanitize_identifier};
use crate::service::types::{OrganisationId, WorkspaceId};

pub trait HttpRequestExt {
    fn get_header(&self, header_name: &str) -> Option<&str>;
    fn get_path_param(&self, param: &str) -> Option<&str>;
    fn get_query_param(&self, query_param: &str) -> Option<&str>;

    fn get_organisation_id(&self) -> Result<OrganisationId, String>;
    fn get_workspace_id(&self) -> Result<WorkspaceId, String>;
}

impl HttpRequestExt for HttpRequest {
    fn get_path_param(&self, param: &str) -> Option<&str> {
        let p = self
            .match_pattern()?
            .split('/')
            .position(|mp| mp == param)?;
        self.path().split('/').nth(p)
    }

    fn get_header(&self, header_name: &str) -> Option<&str> {
        self.headers()
            .get(header_name)
            .and_then(|header_value| header_value.to_str().ok())
    }

    fn get_query_param(&self, query_param: &str) -> Option<&str> {
        let param = format!("{query_param}=");
        self.query_string()
            .split('&')
            .find(|segment| segment.contains(&param))
            .and_then(|query_param| query_param.split('=').nth(1))
    }

    fn get_organisation_id(&self) -> Result<OrganisationId, String> {
        let Some(raw) = self
            .get_header("x-org-id")
            .or_else(|| self.get_path_param("{org_id}"))
            .or_else(|| self.get_query_param("org"))
        else {
            return Err(
                    "The parameter org id is required, and must be passed through headers/url params/query params."
                        .to_string(),
                );
        };
        let sanitized = validate_and_sanitize_identifier(raw, MAX_COMPONENT_LEN, false)
            .map_err(|e| {
            log::warn!("rejecting invalid organisation id: {}", e);
            e.to_string()
        })?;
        Ok(OrganisationId(sanitized))
    }

    fn get_workspace_id(&self) -> Result<WorkspaceId, String> {
        let Some(raw) = self
            .get_header("x-workspace")
            .or_else(|| self.get_header("x-tenant"))
            .or_else(|| self.get_path_param("{workspace}"))
            .or_else(|| self.get_path_param("{tenant}"))
            .or_else(|| self.get_query_param("workspace"))
            .or_else(|| self.get_query_param("tenant"))
        else {
            return Err(
                    "The parameter workspace id is required, and must be passed through headers/url params/query params."
                        .to_string(),
                );
        };
        let sanitized = validate_and_sanitize_identifier(raw, MAX_COMPONENT_LEN, false)
            .map_err(|e| {
            log::warn!("rejecting invalid workspace id: {}", e);
            e.to_string()
        })?;
        Ok(WorkspaceId(sanitized))
    }
}
