use std::future::{ready, Ready};

use crate::{
    extensions::ServiceRequestExt,
    service::types::{
        AppState, OrganisationId, SchemaName, WorkspaceContext, WorkspaceId,
    },
};
use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    error::{self},
    web::Data,
    Error, HttpMessage,
};
use futures_util::future::LocalBoxFuture;
use regex::Regex;
use std::rc::Rc;

pub struct OrgWorkspaceMiddlewareFactory {
    enable_org_id: bool,
    enable_workspace_id: bool,
}

impl OrgWorkspaceMiddlewareFactory {
    pub fn new(enable_org_id: bool, enable_workspace_id: bool) -> Self {
        Self {
            enable_org_id,
            enable_workspace_id,
        }
    }
}

impl<S, B> Transform<S, ServiceRequest> for OrgWorkspaceMiddlewareFactory
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = OrgWorkspaceMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(OrgWorkspaceMiddleware {
            service: Rc::new(service),
            enable_org_id: self.enable_org_id,
            enable_workspace_id: self.enable_workspace_id,
        }))
    }
}

pub struct OrgWorkspaceMiddleware<S> {
    service: Rc<S>,
    enable_org_id: bool,
    enable_workspace_id: bool,
}

impl<S, B> Service<ServiceRequest> for OrgWorkspaceMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let srv = self.service.clone();
        let enable_org_id = self.enable_org_id;
        let enable_workspace_id = self.enable_workspace_id;

        Box::pin(async move {
            let app_state = match req.app_data::<Data<AppState>>() {
                Some(val) => val,
                None => {
                    log::error!("app state not set");
                    return Err(error::ErrorInternalServerError(""));
                }
            };

            let base = match app_state.service_prefix.as_str() {
                "" | "/" => "".to_owned(),
                prefix => "/".to_owned() + prefix,
            };

            let request_path = req.uri().path().replace(&base, "");
            let request_pattern = req
                .match_pattern()
                .map(|a| a.replace(&base, ""))
                .unwrap_or_else(|| request_path.clone());
            let pkg_regex = Regex::new(".*/pkg/.+")
                .map_err(|err| error::ErrorInternalServerError(err.to_string()))?;
            let assets_regex = Regex::new(".*/assets/.+")
                .map_err(|err| error::ErrorInternalServerError(err.to_string()))?;
            let is_excluded: bool = app_state
                .tenant_middleware_exclusion_list
                .contains(&request_pattern)
                || pkg_regex.is_match(&request_path)
                || assets_regex.is_match(&request_path);

            if !is_excluded {
                let workspace_id = match (enable_workspace_id, req.get_workspace_id()) {
                    (true, None) => return Err(error::ErrorBadRequest("The parameter workspace id is required, and must be passed through headers/url params/query params.")),
                    (true, Some(WorkspaceId(workspace_id))) => workspace_id,
                    (false, _) => String::from("test"),
                };

                let org = req.get_organisation_id();
                // TODO: validate the workspace
                let schema_name =  match (enable_org_id, &org) {
                    (true, None) => return Err(error::ErrorBadRequest("The parameter org id is required, and must be passed through headers/url params/query params.")),
                    (true, Some(OrganisationId(org_id))) => {
                        let schema = format!("{org_id}_{workspace_id}");
                        SchemaName(schema)
                    },
                    (false, _) => SchemaName("public".into()),
                };

                let organisation = org.unwrap_or_default();
                let workspace = WorkspaceId(workspace_id.to_string());

                req.extensions_mut().insert(schema_name.clone());
                req.extensions_mut().insert(workspace.clone());
                req.extensions_mut().insert(organisation.clone());
                req.extensions_mut().insert(WorkspaceContext {
                    organisation_id: organisation,
                    workspace_id: workspace,
                    schema_name,
                });
            }

            let res = srv.call(req).await?;

            Ok(res)
        })
    }
}
