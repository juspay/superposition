mod handlers;

use std::sync::RwLock;

use aws_sdk_kms::Client;
use casbin::{CoreApi, Enforcer, MgmtApi};
use chrono::{DateTime, Utc};
use diesel_adapter::DieselAdapter;
use futures_util::future::LocalBoxFuture;
use superposition_types::{Resource, User};

use crate::{
    db::utils::get_database_url,
    helpers::get_from_env_or_default,
    middlewares::auth_z::AuthZDomain,
    service::types::{AppEnv, SchemaName},
};

use super::authorization::Authorizer;

pub use handlers::{admin_endpoints, org_endpoints, workspace_endpoints};

pub struct CasbinPolicyEngine {
    pub(self) enforcer: RwLock<Enforcer>,
    pub(self) last_policy_load_time: RwLock<DateTime<Utc>>,
    policy_refresh_interval: u64,
}

impl CasbinPolicyEngine {
    pub async fn refresh_policies(
        self: &CasbinPolicyEngine,
        enforcer: &mut Enforcer,
    ) -> Result<(), String> {
        enforcer.load_policy().await.map_err(|e| e.to_string())?;
        self.last_policy_load_time
            .write()
            .map_err(|e| e.to_string())?
            .clone_from(&Utc::now());
        Ok(())
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
            // Check if policy refresh is needed (without holding any locks)
            let needs_refresh = {
                let now = Utc::now();
                if let Ok(last_load_time) = self.last_policy_load_time.read() {
                    now.signed_duration_since(*last_load_time).num_seconds()
                        >= self.policy_refresh_interval as i64
                } else {
                    false
                }
            };

            // If refresh is needed, acquire enforcer write lock FIRST to maintain consistent lock ordering
            if needs_refresh {
                if let Ok(mut enforcer) = self.enforcer.write() {
                    // Double-check pattern: verify still needs refresh after acquiring lock
                    let still_needs_refresh = {
                        if let Ok(last_load_time) = self.last_policy_load_time.read() {
                            let now = Utc::now();
                            now.signed_duration_since(*last_load_time).num_seconds()
                                >= self.policy_refresh_interval as i64
                        } else {
                            false
                        }
                    };

                    if still_needs_refresh {
                        if let Err(e) = enforcer.load_policy().await {
                            log::error!("Failed to load Casbin policy: {}", e);
                        } else {
                            // Update the last policy load time
                            if let Ok(mut last_load_time) =
                                self.last_policy_load_time.write()
                            {
                                *last_load_time = Utc::now();
                            }
                        }
                    }
                }
            }

            let enforcer = self.enforcer.read().map_err(|e| {
                log::error!("Failed to acquire Casbin enforcer lock: {}", e);
                e.to_string()
            })?;

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
            let mut enforcer = self.enforcer.write().map_err(|e| {
                log::error!("Failed to acquire Casbin enforcer lock: {}", e);
                e.to_string()
            })?;
            self.refresh_policies(&mut enforcer).await?;

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
                .map_err(|e| {
                    log::error!("Failed to add Casbin policy: {}", e);
                    e.to_string()
                })?;

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
                .map_err(|e| {
                    log::error!("Failed to add Casbin grouping policy: {}", e);
                    e.to_string()
                })
        })
    }

    fn on_workspace_creation(
        &self,
        schema_name: SchemaName,
        workspace_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async move {
            let mut enforcer = self.enforcer.write().map_err(|e| {
                log::error!("Failed to acquire Casbin enforcer lock: {}", e);
                e.to_string()
            })?;
            self.refresh_policies(&mut enforcer).await?;

            enforcer
                .add_grouping_policy(vec![
                    workspace_admin_email.to_string(),
                    "admin".to_string(),
                    schema_name.to_string(),
                ])
                .await
                .map_err(|e| {
                    log::error!("Failed to add Casbin grouping policy: {}", e);
                    e.to_string()
                })
        })
    }

    fn on_workspace_admin_update(
        &self,
        schema_name: SchemaName,
        old_admin_email: String,
        new_admin_email: String,
    ) -> LocalBoxFuture<'_, Result<bool, String>> {
        Box::pin(async move {
            let mut enforcer = self.enforcer.write().map_err(|e| {
                log::error!("Failed to acquire Casbin enforcer lock: {}", e);
                e.to_string()
            })?;
            self.refresh_policies(&mut enforcer).await?;

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
            enforcer
                .add_grouping_policy(vec![
                    new_admin_email,
                    "admin".to_string(),
                    schema_name.to_string(),
                ])
                .await
                .map_err(|e| {
                    log::error!(
                        "Failed to add new admin to Casbin grouping policy: {}",
                        e
                    );
                    e.to_string()
                })
        })
    }

    // fn get_permitted_attributes(
    //     &self,
    //     domain: &AuthZDomain,
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

        let mut enforcer =
            Enforcer::new(MODEL_CONF_PATH, adapter).await.map_err(|e| {
                log::error!("Failed to create Casbin enforcer: {e}");
                e.to_string()
            })?;

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
