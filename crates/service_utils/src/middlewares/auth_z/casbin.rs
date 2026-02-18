mod handlers;

use std::sync::Mutex;

use aws_sdk_kms::Client;
use casbin::{CoreApi, Enforcer, MgmtApi};
use diesel_adapter::DieselAdapter;
use futures_util::future::LocalBoxFuture;
use superposition_types::User;

use crate::{
    db::utils::get_database_url,
    helpers::get_from_env_or_default,
    service::types::{AppEnv, Resource, WorkspaceContext},
};

use super::authorization::Authorizer;

pub use handlers::endpoints;

pub struct CasbinPolicyEngine {
    pub(self) enforcer: Mutex<Enforcer>,
}

impl Authorizer for CasbinPolicyEngine {
    fn is_allowed(
        &self,
        workspace_context: &WorkspaceContext,
        user: &User,
        resource: &Resource,
        action: &str,
        attributes: Option<&[&str]>,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        let sub = user.get_username();
        let workspace = resource.workspace_for(workspace_context);
        let resource = resource.to_string();
        let action = action.to_string();
        let attributes = attributes
            .map(|v| v.iter().map(|s| s.to_string()).collect::<Vec<_>>())
            .unwrap_or(vec!["*".to_string()]);

        Box::pin(async move {
            let mut enforcer = self.enforcer.lock().map_err(|e| {
                log::error!("Failed to acquire Casbin enforcer lock: {}", e);
                e.to_string()
            })?;

            enforcer.load_policy().await.map_err(|e| {
                log::error!("Failed to load Casbin policy: {}", e);
                e.to_string()
            })?;

            for attr in attributes {
                let allowed = enforcer
                    .enforce((&sub, &workspace, &resource, &action, &attr))
                    .map_err(|e| {
                        log::error!("Casbin enforcement error: {}", e);
                        e.to_string()
                    })?;

                if !allowed {
                    return Ok(false);
                }
            }
            Ok(true)
        })
    }

    fn on_org_creation(
        &self,
        organisation_id: &str,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        let org_id = organisation_id.to_string();
        Box::pin(async move {
            let mut enforcer = self.enforcer.try_lock().map_err(|e| {
                log::error!("Failed to acquire Casbin enforcer lock: {}", e);
                e.to_string()
            })?;
            enforcer.load_policy().await.map_err(|e| {
                log::error!("Failed to load Casbin policy: {}", e);
                e.to_string()
            })?;

            let policy_added = enforcer
                .add_policy(vec![
                    "admin".to_string(),
                    format!("{}_{}", org_id, "*"),
                    "*".to_string(),
                    "*".to_string(),
                    "*".to_string(),
                ])
                .await
                .map_err(|e| {
                    log::error!("Failed to add Casbin policy: {}", e);
                    e.to_string()
                })?;

            Ok(policy_added)
        })
    }

    // fn get_permitted_attributes(
    //     &self,
    //     workspace_request: &WorkspaceContext,
    //     user: &User,
    //     resource: &ResourceContext,
    //     action: &Action,
    // ) -> Result<Vec<String>, String> {
    //     let sub = &user.username;
    //     let obj = format!("{}:{}", resource.kind, resource.id);
    //     let act = action.0.clone();

    //     let mut attrs = vec![];

    //     // Check user's effective roles + their own policies
    //     for role in self
    //         .get_roles_for_user(sub)
    //         .map_err(|e| e.to_string())?
    //         .into_iter()
    //         .chain(std::iter::once(sub.to_string()))
    //     {
    //         let policies = self.get_filtered_policy(0, vec![act, obj, act]);

    //         for p in policies {
    //             if p.len() >= 4 {
    //                 if let Ok(json_attrs) = from_str::<Vec<String>>(&p[3]) {
    //                     attrs.extend(json_attrs);
    //                 }
    //             }
    //         }
    //     }

    //     Ok(attrs)
    // }
}

const MODEL_CONF_PATH: &str =
    "crates/service_utils/src/middlewares/auth_z/casbin/model.conf";

impl CasbinPolicyEngine {
    pub async fn new(
        kms_client: &Option<Client>,
        app_env: &AppEnv,
        db_pool_size: Option<u32>,
    ) -> Result<Self, String> {
        let db_url = get_database_url(kms_client, app_env, Some("CASBIN")).await;
        let db_pool_size = db_pool_size
            .unwrap_or_else(|| get_from_env_or_default("CASBIN_DB_POOL_SIZE", 2));
        let adapter = DieselAdapter::new(db_url.clone(), db_pool_size).map_err(|e| {
            log::error!("Failed to create Casbin adapter: {e}");
            e.to_string()
        })?;
        let enforcer = Enforcer::new(MODEL_CONF_PATH, adapter).await.map_err(|e| {
            log::error!("Failed to create Casbin enforcer: {e}");
            e.to_string()
        })?;

        Ok(Self {
            enforcer: Mutex::new(enforcer),
        })
    }

    pub async fn management(
        kms_client: &Option<Client>,
        app_env: &AppEnv,
    ) -> Result<Self, String> {
        Self::new(kms_client, app_env, Some(1)).await
    }
}
