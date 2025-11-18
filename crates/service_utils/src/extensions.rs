use actix_web::HttpRequest;

use crate::service::types::{OrganisationId, WorkspaceId};

pub trait HttpRequestExt {
    fn get_header(&self, header_name: &str) -> Option<&str>;
    fn get_path_param(&self, param: &str) -> Option<&str>;
    fn get_query_param(&self, query_param: &str) -> Option<&str>;

    fn get_organisation_id(&self) -> Option<OrganisationId>;
    fn get_workspace_id(&self) -> Option<WorkspaceId>;
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

    fn get_organisation_id(&self) -> Option<OrganisationId> {
        self.get_header("x-org-id")
            .or_else(|| self.get_path_param("{org_id}"))
            .or_else(|| self.get_query_param("org"))
            .map(String::from)
            .map(OrganisationId)
    }

    fn get_workspace_id(&self) -> Option<WorkspaceId> {
        self.get_header("x-workspace")
            .or_else(|| self.get_header("x-tenant"))
            .or_else(|| self.get_path_param("{workspace}"))
            .or_else(|| self.get_path_param("{tenant}"))
            .or_else(|| self.get_query_param("workspace"))
            .or_else(|| self.get_query_param("tenant"))
            .map(String::from)
            .map(WorkspaceId)
    }
}
