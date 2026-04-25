use std::{
    panic::{AssertUnwindSafe, catch_unwind},
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use fred::{
    clients::RedisPool,
    interfaces::{KeysInterface, LuaInterface},
    prelude::RedisClient,
    types::{Expiration, SetOptions},
};
use tokio::{
    task::JoinHandle,
    time::{Duration, MissedTickBehavior, interval},
};
use uuid::Uuid;

const REDIS_LOCK_PREFIX: &str = "workspace_lock";
const MAX_HEARTBEAT_CONSECUTIVE_FAILURES: u32 = 3;

const REDIS_UNLOCK_SCRIPT: &str = r#"
    if redis.call("GET", KEYS[1]) == ARGV[1] then
        return redis.call("DEL", KEYS[1])
    else
        return 0
    end
"#;

const REDIS_RENEW_SCRIPT: &str = r#"
    if redis.call("GET", KEYS[1]) == ARGV[1] then
        return redis.call("PEXPIRE", KEYS[1], ARGV[2])
    else
        return 0
    end
"#;

pub(crate) struct WorkspaceLockTarget {
    pub(crate) redis_key: String,
    pub(crate) pg_lock_key: i64,
}

impl WorkspaceLockTarget {
    pub(crate) fn new(org_id: &str, workspace_id: &str) -> Self {
        Self {
            redis_key: format!("{}:{}:{}", REDIS_LOCK_PREFIX, org_id, workspace_id),
            pg_lock_key: compute_lock_key(org_id, workspace_id),
        }
    }
}

#[derive(Debug)]
pub(crate) enum AcquireLockError {
    Redis(String),
    Diesel(diesel::result::Error),
    LockContended { backend: &'static str },
}

impl std::fmt::Display for AcquireLockError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Redis(msg) => write!(f, "redis error: {}", msg),
            Self::Diesel(e) => write!(f, "database error: {}", e),
            Self::LockContended { backend } => {
                write!(f, "{} lock is already held by another request", backend)
            }
        }
    }
}

impl From<diesel::result::Error> for AcquireLockError {
    fn from(e: diesel::result::Error) -> Self {
        Self::Diesel(e)
    }
}

pub(crate) struct RedisLockGuard {
    lease_healthy: Arc<AtomicBool>,
    pool: RedisPool,
    heartbeat_task: Option<JoinHandle<()>>,
    lock_key: Option<String>,
    lock_value: Option<String>,
}

impl RedisLockGuard {
    pub(crate) fn ensure_healthy(&self) -> Result<(), String> {
        if self.lease_healthy.load(Ordering::Acquire) {
            return Ok(());
        }

        let key = self.lock_key.as_deref().unwrap_or("unknown");
        Err(format!(
            "workspace redis lock lease was lost for {}; aborting write",
            key
        ))
    }

    pub(crate) async fn release(&mut self) -> Result<(), String> {
        if let Some(task) = self.heartbeat_task.take() {
            task.abort();
        }

        let (Some(lock_key), Some(lock_value)) =
            (self.lock_key.take(), self.lock_value.take())
        else {
            return Ok(());
        };

        release_redis_lock(&self.pool, lock_key, lock_value).await
    }
}

impl Drop for RedisLockGuard {
    fn drop(&mut self) {
        if let Some(task) = self.heartbeat_task.take() {
            task.abort();
        }

        let (Some(lock_key), Some(lock_value)) =
            (self.lock_key.take(), self.lock_value.take())
        else {
            return;
        };

        let pool = self.pool.clone();
        actix_web::rt::spawn(async move {
            if let Err(e) = release_redis_lock(&pool, lock_key, lock_value).await {
                log::error!("failed to release redis lock in drop fallback: {}", e);
            }
        });
    }
}

async fn release_redis_lock(
    pool: &RedisPool,
    lock_key: String,
    lock_value: String,
) -> Result<(), String> {
    let client = connected_client(pool)?;
    let result = client
        .eval::<i64, _, _, _>(
            REDIS_UNLOCK_SCRIPT,
            vec![lock_key.clone()],
            vec![lock_value],
        )
        .await
        .map_err(|e| e.to_string())?;

    match result {
        1 => {
            log::debug!("released redis lock: {}", lock_key);
            Ok(())
        }
        _ => {
            log::warn!(
                "redis lock {} already expired or was stolen \
                 (value mismatch — our token no longer present)",
                lock_key
            );
            Ok(())
        }
    }
}

async fn renew_redis_lock(
    pool: &RedisPool,
    lock_key: &str,
    lock_value: &str,
    lock_ttl: Duration,
) -> Result<bool, String> {
    let client = connected_client(pool)?;
    let result = client
        .eval::<i64, _, _, _>(
            REDIS_RENEW_SCRIPT,
            vec![lock_key.to_string()],
            vec![lock_value.to_string(), lock_ttl.as_millis().to_string()],
        )
        .await
        .map_err(|e| e.to_string())?;

    Ok(result == 1)
}

fn connected_client(pool: &RedisPool) -> Result<RedisClient, String> {
    catch_unwind(AssertUnwindSafe(|| pool.next_connected()))
        .map_err(|_| "redis pool has no connected clients available".to_string())
        .cloned()
}

fn redis_lock_heartbeat_interval(lock_ttl: Duration) -> Duration {
    let ttl_ms = lock_ttl.as_millis();
    let interval_ms = std::cmp::max(ttl_ms / 3, 1);

    Duration::from_millis(interval_ms as u64)
}

fn spawn_redis_lock_heartbeat(
    lease_healthy: Arc<AtomicBool>,
    pool: RedisPool,
    lock_key: String,
    lock_value: String,
    lock_ttl: Duration,
) -> JoinHandle<()> {
    actix_web::rt::spawn(async move {
        let mut ticker = interval(redis_lock_heartbeat_interval(lock_ttl));
        ticker.set_missed_tick_behavior(MissedTickBehavior::Delay);
        ticker.tick().await;
        let mut consecutive_failures = 0_u32;

        loop {
            ticker.tick().await;

            match renew_redis_lock(&pool, &lock_key, &lock_value, lock_ttl).await {
                Ok(true) => {
                    consecutive_failures = 0;
                    log::debug!("renewed redis lock lease: {}", lock_key);
                }
                Ok(false) => {
                    lease_healthy.store(false, Ordering::Release);
                    log::error!(
                        "stopped renewing redis lock {} because ownership was lost",
                        lock_key
                    );
                    break;
                }
                Err(e) => {
                    consecutive_failures += 1;
                    log::error!(
                        "failed to renew redis lock {} ({}/{}): {}",
                        lock_key,
                        consecutive_failures,
                        MAX_HEARTBEAT_CONSECUTIVE_FAILURES,
                        e
                    );

                    if consecutive_failures >= MAX_HEARTBEAT_CONSECUTIVE_FAILURES {
                        lease_healthy.store(false, Ordering::Release);
                        log::error!(
                            "giving up on redis lock {} after {} consecutive heartbeat failures",
                            lock_key,
                            consecutive_failures
                        );
                        break;
                    }
                }
            }
        }
    })
}

pub(crate) async fn acquire_redis_lock(
    pool: &RedisPool,
    target: &WorkspaceLockTarget,
    lock_ttl: Duration,
    heartbeat_enabled: bool,
) -> Result<RedisLockGuard, AcquireLockError> {
    let lock_value = Uuid::new_v4().to_string();

    let client = connected_client(pool).map_err(AcquireLockError::Redis)?;
    let result: Option<String> = client
        .set(
            target.redis_key.clone(),
            lock_value.clone(),
            Some(Expiration::PX(lock_ttl.as_millis() as i64)),
            Some(SetOptions::NX),
            false,
        )
        .await
        .map_err(|e| AcquireLockError::Redis(e.to_string()))?;

    if result.is_some() {
        let lease_healthy = Arc::new(AtomicBool::new(true));
        let heartbeat_task = heartbeat_enabled.then(|| {
            spawn_redis_lock_heartbeat(
                lease_healthy.clone(),
                pool.clone(),
                target.redis_key.clone(),
                lock_value.clone(),
                lock_ttl,
            )
        });

        return Ok(RedisLockGuard {
            lease_healthy,
            pool: pool.clone(),
            heartbeat_task,
            lock_key: Some(target.redis_key.clone()),
            lock_value: Some(lock_value),
        });
    }

    Err(AcquireLockError::LockContended { backend: "redis" })
}

fn compute_lock_key(org_id: &str, workspace_id: &str) -> i64 {
    const FNV_OFFSET_BASIS: u64 = 14695981039346656037;
    const FNV_PRIME: u64 = 1099511628211;

    let mut hash = FNV_OFFSET_BASIS;
    for byte in org_id
        .bytes()
        .chain(b":".iter().copied())
        .chain(workspace_id.bytes())
    {
        hash ^= byte as u64;
        hash = hash.wrapping_mul(FNV_PRIME);
    }
    hash as i64
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lock_target_redis_key_format() {
        let target = WorkspaceLockTarget::new("acme", "production");
        assert_eq!(target.redis_key, "workspace_lock:acme:production");
    }

    #[test]
    fn test_lock_target_pg_key_matches_compute() {
        let target = WorkspaceLockTarget::new("acme", "production");
        assert_eq!(target.pg_lock_key, compute_lock_key("acme", "production"));
    }

    #[test]
    fn test_lock_key_consistency() {
        let key1 = compute_lock_key("org1", "workspace1");
        let key2 = compute_lock_key("org1", "workspace1");
        assert_eq!(key1, key2);
    }

    #[test]
    fn test_lock_key_uniqueness() {
        let key1 = compute_lock_key("org1", "workspace1");
        let key2 = compute_lock_key("org1", "workspace2");
        let key3 = compute_lock_key("org2", "workspace1");
        let key4 = compute_lock_key("org2", "workspace2");

        let keys = [key1, key2, key3, key4];
        for i in 0..keys.len() {
            for j in (i + 1)..keys.len() {
                assert_ne!(keys[i], keys[j]);
            }
        }
    }

    #[test]
    fn test_lock_key_separator_prevents_ambiguity() {
        let key1 = compute_lock_key("org1", "workspace");
        let key2 = compute_lock_key("org", "1workspace");
        assert_ne!(key1, key2);
    }

    #[test]
    fn test_lock_keys_are_unique_for_sample_matrix() {
        let orgs = ["org-a", "org-b", "org-c"];
        let workspaces = ["ws-1", "ws-2", "ws-3", "ws-4"];

        let mut keys = std::collections::HashSet::new();
        for org in orgs {
            for workspace in workspaces {
                let inserted = keys.insert(compute_lock_key(org, workspace));
                assert!(
                    inserted,
                    "lock key should be unique for ({org}, {workspace})"
                );
            }
        }
    }

    #[test]
    fn test_lock_target_keeps_backends_in_sync() {
        let target = WorkspaceLockTarget::new("merchant-org", "payments-prod");

        assert_eq!(
            target.redis_key,
            "workspace_lock:merchant-org:payments-prod"
        );
        assert_eq!(
            target.pg_lock_key,
            compute_lock_key("merchant-org", "payments-prod")
        );
    }

    #[test]
    fn test_heartbeat_interval_stays_below_ttl() {
        let ttl = std::hint::black_box(Duration::from_millis(30_000));
        let interval = std::hint::black_box(redis_lock_heartbeat_interval(ttl));

        assert!(interval < ttl);
        assert!(!interval.is_zero());
    }

    #[test]
    fn test_max_heartbeat_failures_is_positive() {
        let max_failures = std::hint::black_box(MAX_HEARTBEAT_CONSECUTIVE_FAILURES);

        assert!(max_failures > 0);
    }
}
