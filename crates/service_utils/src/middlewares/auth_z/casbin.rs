mod handlers;

use aws_sdk_kms::Client;
use casbin::{CoreApi, Enforcer, MgmtApi};
use chrono::{DateTime, Utc};
use diesel_adapter::DieselAdapter;
use futures_util::future::LocalBoxFuture;
use superposition_types::{Resource, User};
use tokio::sync::RwLock;

use crate::{
    db::utils::get_database_url,
    helpers::{get_from_env_or_default, get_from_env_unsafe},
    middlewares::auth_z::AuthZDomain,
    service::types::{AppEnv, SchemaName},
};

use super::authorization::Authorizer;

pub use handlers::{admin_endpoints, org_endpoints, workspace_endpoints};

pub struct CasbinPolicyEngine {
    enforcer: RwLock<Enforcer>,
    last_policy_load_time: RwLock<DateTime<Utc>>,
    policy_refresh_interval: u64,
}

impl CasbinPolicyEngine {
    /// Acquire an exclusive lock on the enforcer with automatic refresh check.
    /// Returns the enforcer for mutation while ensuring policies are fresh.
    pub async fn enforcer<T, F>(&self, f: F) -> Result<T, String>
    where
        F: AsyncFnOnce(&mut Enforcer) -> Result<T, String>,
    {
        let mut enforcer = self.enforcer.write().await;

        // Check if refresh is needed while holding the write lock
        let now = Utc::now();
        let last_load_time = self.last_policy_load_time.read().await;
        let needs_refresh = now.signed_duration_since(*last_load_time).num_seconds()
            >= self.policy_refresh_interval as i64;
        drop(last_load_time);

        if needs_refresh {
            enforcer.load_policy().await.map_err(|e| {
                log::error!("Failed to refresh Casbin policies: {}", e);
                e.to_string()
            })?;

            let mut last_load_time = self.last_policy_load_time.write().await;
            *last_load_time = Utc::now();
        }

        let resp = f(&mut enforcer).await?;

        enforcer.save_policy().await.map_err(|e| {
            log::error!("Failed to save Casbin policies: {}", e);
            e.to_string()
        })?;

        Ok(resp)
    }

    pub async fn refresh_policies(self: &CasbinPolicyEngine) -> Result<(), String> {
        let mut enforcer = self.enforcer.write().await;
        enforcer.load_policy().await.map_err(|e| e.to_string())?;
        let mut last_load_time = self.last_policy_load_time.write().await;
        *last_load_time = Utc::now();
        Ok(())
    }

    pub(self) async fn add_org_policy(
        enforcer: &mut Enforcer,
        organisation_id: String,
        org_admin_email: String,
    ) -> Result<bool, String> {
        let org_schema = format!("{}_{}", organisation_id, "*");

        let policy_added = enforcer
            .add_policy(vec![
                "admin".to_string(),
                org_schema.clone(),
                "*".to_string(),
                "*".to_string(),
                "*".to_string(),
            ])
            .await
            .map_err(|e| e.to_string())?;

        if !policy_added {
            return Ok(false);
        }

        enforcer
            .add_grouping_policy(vec![
                org_admin_email.to_string(),
                "admin".to_string(),
                org_schema,
            ])
            .await
            .map_err(|e| e.to_string())
    }

    pub(self) async fn add_workspace_admin(
        enforcer: &mut Enforcer,
        schema_name: &str,
        workspace_admin_email: String,
    ) -> Result<bool, String> {
        enforcer
            .add_grouping_policy(vec![
                workspace_admin_email,
                "admin".to_string(),
                schema_name.to_string(),
            ])
            .await
            .map_err(|e| e.to_string())
    }
}

impl Authorizer for CasbinPolicyEngine {
    fn is_allowed(
        &self,
        domain: &AuthZDomain,
        user: &User,
        resource: &Resource,
        action: &str,
        attributes: Option<&[&str]>,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        let sub = user.get_email();
        let domain = domain.to_string();
        let resource = resource.to_string();
        let action = action.to_string();
        let attributes = attributes
            .map(|v| v.iter().map(|s| s.to_string()).collect::<Vec<_>>())
            .unwrap_or(vec!["*".to_string()]);

        Box::pin(async move {
            // Check if policy refresh is needed (for distributed systems)
            let needs_refresh = {
                let now = Utc::now();
                let last_load_time = self.last_policy_load_time.read().await;
                now.signed_duration_since(*last_load_time).num_seconds()
                    >= self.policy_refresh_interval as i64
            };

            // Refresh policies if needed
            if needs_refresh {
                if let Err(e) = self.refresh_policies().await {
                    log::warn!("Failed to refresh Casbin policies: {}", e);
                }
            }

            let enforcer = self.enforcer.read().await;

            for attr in attributes {
                let allowed = enforcer
                    .enforce((&sub, &domain, &resource, &action, &attr))
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
        organisation_id: String,
        org_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async move {
            self.enforcer(async |enforcer| {
                Self::add_org_policy(enforcer, organisation_id, org_admin_email).await
            })
            .await
        })
    }

    fn on_workspace_creation(
        &self,
        schema_name: SchemaName,
        workspace_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async move {
            self.enforcer(async |enforcer| {
                Self::add_workspace_admin(enforcer, &schema_name, workspace_admin_email)
                    .await
            })
            .await
        })
    }

    fn on_workspace_admin_update(
        &self,
        schema_name: SchemaName,
        old_admin_email: String,
        new_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async move {
            self.enforcer(async |enforcer| {
                // Remove old admin
                if let Err(e) = enforcer
                    .remove_grouping_policy(vec![
                        old_admin_email,
                        "admin".to_string(),
                        schema_name.to_string(),
                    ])
                    .await
                {
                    log::error!(
                        "Failed to remove old admin from Casbin grouping policy: {}",
                        e
                    );
                    return Err(e.to_string());
                }

                // Add new admin
                Self::add_workspace_admin(enforcer, &schema_name, new_admin_email).await
            })
            .await
        })
    }

    // async fn get_permitted_attributes<A: Action>(
    //     &self,
    //     domain: &AuthZDomain,
    //     user: &User,
    //     resource: &Resource,
    //     action: &AuthZ<A>,
    // ) -> Result<Vec<String>, String> {
    //     let sub = &user.get_email();
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

        let mut enforcer =
            Enforcer::new(MODEL_CONF_PATH, adapter).await.map_err(|e| {
                log::error!("Failed to create Casbin enforcer: {e}");
                e.to_string()
            })?;

        enforcer.enable_auto_save(false);

        if let Ok(root_admin_email) = get_from_env_unsafe::<String>("ROOT_ADMIN_EMAIL") {
            let result = enforcer
                .add_grouping_policy(vec![
                    root_admin_email,
                    "admin".to_string(),
                    "*".to_string(),
                ])
                .await
                .map_err(|e| {
                    log::error!("Failed to add root admin policy: {e}");
                    e.to_string()
                })?;

            if result {
                log::info!("Root admin policy added");
            } else {
                log::info!("Root admin policy already exists");
            }

            enforcer.save_policy().await.map_err(|e| {
                log::error!(
                    "Failed to save Casbin policies after adding root admin: {e}"
                );
                e.to_string()
            })?;
        }

        let now = Utc::now();
        enforcer.load_policy().await.map_err(|e| {
            log::error!("Failed to load Casbin policy: {e}");
            e.to_string()
        })?;

        Ok(Self {
            enforcer: RwLock::new(enforcer),
            last_policy_load_time: RwLock::new(now),
            policy_refresh_interval: get_from_env_or_default(
                "CASBIN_POLICY_REFRESH_INTERVAL",
                60,
            ),
        })
    }

    pub async fn management(
        kms_client: &Option<Client>,
        app_env: &AppEnv,
    ) -> Result<Self, String> {
        Self::new(kms_client, app_env, Some(1)).await
    }
}
