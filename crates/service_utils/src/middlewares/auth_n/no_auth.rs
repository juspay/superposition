use actix_web::{
    cookie::{time::Duration, Cookie},
    dev::ServiceRequest,
    error,
    web::{Data, Json, Path},
    HttpRequest, HttpResponse, Scope,
};
use diesel::{Connection, ExpressionMethods, QueryDsl, RunQueryDsl};
use futures_util::future::LocalBoxFuture;
use superposition_types::{
    database::superposition_schema::superposition::organisations, User,
};

use crate::service::types::AppState;

use super::{
    authentication::{Authenticator, Login},
    SwitchOrgParams,
};

pub struct DisabledAuthenticator {
    path_prefix: String,
}

impl DisabledAuthenticator {
    pub fn new(path_prefix: String) -> Self {
        Self { path_prefix }
    }
}

impl Authenticator for DisabledAuthenticator {
    fn get_path_prefix(&self) -> String {
        self.path_prefix.clone()
    }

    fn authenticate(
        &self,
        _: &ServiceRequest,
        _: &Login,
    ) -> Result<User, actix_web::HttpResponse> {
        Ok(User::default())
    }

    fn routes(&self) -> actix_web::Scope {
        Scope::new("no_auth")
    }

    fn get_organisations(&self, req: &actix_web::HttpRequest) -> HttpResponse {
        let app_state = match req.app_data::<Data<AppState>>() {
            Some(state) => state,
            None => {
                log::info!(
                    "DbConnection-FromRequest: Unable to get app_data from request"
                );
                return error::ErrorInternalServerError(
                    "Unable to get app_data from request",
                )
                .into();
            }
        };

        let result = match app_state.db_pool.get() {
            Ok(mut conn) => {
                conn.set_prepared_statement_cache_size(
                    diesel::connection::CacheSize::Disabled,
                );
                let orgs = organisations::table
                    .order(organisations::created_at.desc())
                    .select(organisations::id)
                    .get_results::<String>(&mut conn);

                match orgs {
                    Ok(orgs) => Ok(orgs),
                    Err(e) => {
                        log::error!("Failed to fetch organisations: {:?}", e);
                        Err("Failed to fetch organisations")
                    }
                }
            }
            Err(e) => {
                log::info!("Unable to get db connection from pool, error: {e}");
                Err("Unable to get db connection from pool")
            }
        };

        match result {
            Ok(resp) => HttpResponse::Ok().json(Json(resp)),
            Err(resp) => error::ErrorInternalServerError(resp).into(),
        }
    }

    fn switch_organisation(
        &self,
        _: &HttpRequest,
        path_params: &Path<SwitchOrgParams>,
    ) -> LocalBoxFuture<'static, actix_web::Result<HttpResponse>> {
        let path = if self.path_prefix.as_str() == "" {
            String::from("/")
        } else {
            self.path_prefix.clone()
        };
        let cookie = Cookie::build(Login::Org.to_string(), "org_token")
            .path(path)
            .http_only(true)
            .max_age(Duration::days(1))
            .finish();

        let org_id = path_params.organisation_id.clone();
        let path_prefix = self.path_prefix.clone();

        Box::pin(async move {
            Ok(HttpResponse::Found()
                .cookie(cookie)
                .insert_header((
                    "Location",
                    format!("{path_prefix}/admin/{org_id}/workspaces"),
                ))
                .finish())
        })
    }
}
