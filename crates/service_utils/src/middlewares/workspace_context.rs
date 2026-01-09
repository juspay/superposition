use std::future::{ready, Ready};
use std::rc::Rc;

use actix_web::{
    body::EitherBody,
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    error,
    web::Data,
    Error, HttpMessage,
};
use futures_util::future::LocalBoxFuture;
use regex::Regex;
use superposition_macros::{bad_argument, unexpected_error};

use crate::helpers::get_workspace;
use crate::{
    extensions::HttpRequestExt,
    service::types::{
        AppState, OrganisationId, SchemaName, WorkspaceContext, WorkspaceId,
    },
};

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
    type Response = ServiceResponse<EitherBody<B>>;
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
    type Response = ServiceResponse<EitherBody<B>>;
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
                let organisation = match (
                    enable_org_id,
                    req.request().get_organisation_id(),
                ) {
                    (true, None) => {
                        let error: Error = bad_argument!(
                            "The parameter org id is required, and must be passed through headers/url params/query params."
                        ).into();
                        return Ok(req.into_response(
                            error.error_response().map_into_right_body(),
                        ));
                    }
                    (true, Some(org_id)) => org_id,
                    (false, _) => OrganisationId::default(),
                };

                let workspace = req.request().get_workspace_id();

                let schema_name = match (enable_workspace_id, &workspace) {
                    (true, None) => {
                        let error: Error = bad_argument!(
                            "The parameter workspace id is required, and must be passed through headers/url params/query params."
                        ).into();
                        return Ok(req.into_response(
                            error.error_response().map_into_right_body(),
                        ));
                    }
                    (true, Some(workspace_id)) => {
                        let schema = format!("{}_{}", *organisation, **workspace_id);
                        let schema_name = SchemaName(schema);
                        let workspace_settings = {
                            let mut db_conn = app_state
                                .db_pool
                                .get()
                                .map_err(|err| unexpected_error!("{}", err))?;

                            get_workspace(&schema_name, &mut db_conn)?
                        };
                        req.extensions_mut().insert(workspace_settings);
                        schema_name
                    }
                    (false, _) => SchemaName::default(),
                };

                let workspace_id =
                    workspace.unwrap_or_else(|| WorkspaceId(String::from("test")));

                req.extensions_mut().insert(schema_name.clone());
                req.extensions_mut().insert(workspace_id.clone());
                req.extensions_mut().insert(organisation.clone());
                req.extensions_mut().insert(WorkspaceContext {
                    organisation_id: organisation,
                    workspace_id,
                    schema_name,
                });
            }

            let res = srv.call(req).await?.map_into_left_body();

            Ok(res)
        })
    }
}
