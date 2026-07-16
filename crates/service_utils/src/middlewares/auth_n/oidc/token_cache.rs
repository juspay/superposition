//! In-memory cache for Basic-auth token exchanges (password + client_credentials).
//!
//! Basic auth is validated on every request, and each validation otherwise
//! round-trips to the IdP's token endpoint. This cache short-circuits repeat
//! requests bearing the same credentials with the previously-resolved
//! principal, until shortly before the exchanged token would expire.

use std::{
    collections::{HashMap, hash_map::DefaultHasher},
    hash::{Hash, Hasher},
    sync::RwLock,
    time::{Duration, Instant},
};

use superposition_types::User;

/// Drop a cached principal this many seconds before the exchanged token's
/// actual expiry, to stay ahead of clock skew and in-flight latency.
const CACHE_REFRESH_SAFETY_MARGIN_SECS: Duration = Duration::from_secs(30);

/// Fallback TTL when the IdP omits `expires_in` from the token response
/// (`expires_in` is only RECOMMENDED by RFC 6749 §5.1, not required).
const FALLBACK_TTL_SECS: Duration = Duration::from_secs(60);

/// Which Basic grant produced an entry. Part of the key so a `username` can
/// never collide with a `client_id`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum CachedGrant {
    Password,
    ClientCredentials,
    ApiToken,
}

/// Cache key: `(scope, grant, principal_id, hash(secret))`.
/// - `scope` is the org for SaaS (per-org realms) and `None` for the
///   single-realm Simple authenticator.
/// - hashing the secret keeps the key compact and makes rotation an automatic
///   miss (a new secret hashes differently), and avoids holding plaintext.
type CacheKey = (Option<String>, CachedGrant, String, u64);

struct CacheEntry {
    user: User,
    expires_at: Instant,
}

#[derive(Default)]
pub(super) struct TokenExchangeCache {
    entries: RwLock<HashMap<CacheKey, CacheEntry>>,
}

impl TokenExchangeCache {
    pub(super) fn new() -> Self {
        Self::default()
    }

    /// Builds a cache key, hashing the secret so it is never stored in plaintext.
    pub(super) fn key(
        scope: Option<String>,
        grant: CachedGrant,
        principal_id: &str,
        secret: &str,
    ) -> CacheKey {
        (scope, grant, principal_id.to_string(), hash_secret(secret))
    }

    /// Returns the cached principal if present and not yet expired.
    pub(super) fn get(&self, key: &CacheKey) -> Option<User> {
        let entries = self.entries.read().unwrap_or_else(|e| e.into_inner());
        entries
            .get(key)
            .filter(|entry| Instant::now() < entry.expires_at)
            .map(|entry| entry.user.clone())
    }

    /// Caches `user` with a TTL of the token's `expires_in` minus the safety
    /// margin (or the fallback when the IdP omitted `expires_in`). Expired
    /// entries are pruned lazily on write so the map cannot grow unbounded.
    pub(super) fn insert(&self, key: CacheKey, user: User, expires_in: Option<Duration>) {
        let ttl = expires_in
            .unwrap_or(FALLBACK_TTL_SECS)
            .saturating_sub(CACHE_REFRESH_SAFETY_MARGIN_SECS);
        let expires_at = Instant::now() + ttl;

        let mut entries = self.entries.write().unwrap_or_else(|e| e.into_inner());
        let now = Instant::now();
        entries.retain(|_, entry| entry.expires_at > now);
        entries.insert(key, CacheEntry { user, expires_at });
    }
}

fn hash_secret(secret: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    secret.hash(&mut hasher);
    hasher.finish()
}
