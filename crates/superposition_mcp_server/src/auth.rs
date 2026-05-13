use base64::Engine;
use secrecy::{ExposeSecret, SecretString};

#[derive(Debug, Clone)]
pub enum AuthValue {
    Bearer(SecretString),
    Basic { user: String, pass: SecretString },
}

tokio::task_local! {
    pub static SUPERPOSITION_AUTH: AuthValue;
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum AuthParseError {
    #[error("missing Authorization header")]
    Missing,
    #[error("malformed Authorization header")]
    Malformed,
    #[error("unsupported authentication scheme")]
    UnsupportedScheme,
}

impl AuthValue {
    /// Parse an `Authorization` header value.
    /// Supports `Bearer <token>` and `Basic <base64(user:pass)>`.
    pub fn parse_header(value: Option<&str>) -> Result<AuthValue, AuthParseError> {
        let raw = value.ok_or(AuthParseError::Missing)?.trim();
        let (scheme, rest) = raw.split_once(' ').ok_or(AuthParseError::Malformed)?;
        let scheme = scheme.to_ascii_lowercase();
        let rest = rest.trim();
        match scheme.as_str() {
            "bearer" => {
                if rest.is_empty() {
                    return Err(AuthParseError::Malformed);
                }
                Ok(AuthValue::Bearer(SecretString::new(rest.to_string().into())))
            }
            "basic" => {
                let decoded = base64::engine::general_purpose::STANDARD
                    .decode(rest)
                    .map_err(|_| AuthParseError::Malformed)?;
                let decoded = String::from_utf8(decoded).map_err(|_| AuthParseError::Malformed)?;
                let (user, pass) = decoded.split_once(':').ok_or(AuthParseError::Malformed)?;
                Ok(AuthValue::Basic {
                    user: user.to_string(),
                    pass: SecretString::new(pass.to_string().into()),
                })
            }
            _ => Err(AuthParseError::UnsupportedScheme),
        }
    }

    /// Bearer-token string (only meaningful for Bearer variant).
    pub fn bearer(&self) -> Option<&str> {
        match self {
            AuthValue::Bearer(t) => Some(t.expose_secret()),
            _ => None,
        }
    }

    /// (user, pass) for Basic variant.
    pub fn basic(&self) -> Option<(&str, &str)> {
        match self {
            AuthValue::Basic { user, pass } => Some((user, pass.expose_secret())),
            _ => None,
        }
    }
}

impl From<crate::config::StaticCreds> for AuthValue {
    fn from(c: crate::config::StaticCreds) -> Self {
        match c {
            crate::config::StaticCreds::Bearer(t) => AuthValue::Bearer(t),
            crate::config::StaticCreds::Basic { user, pass } => AuthValue::Basic { user, pass },
        }
    }
}

use aws_smithy_runtime_api::client::identity::{
    http::{Login, Token},
    Identity, IdentityFuture, ResolveIdentity, SharedIdentityResolver,
};
use aws_smithy_runtime_api::client::runtime_components::RuntimeComponents;
use aws_smithy_types::config_bag::ConfigBag;

/// Resolves bearer-token identity from the task-local, falling back to a static value if provided.
#[derive(Debug)]
pub struct BearerResolver {
    pub fallback: Option<SecretString>,
}

impl ResolveIdentity for BearerResolver {
    fn resolve_identity<'a>(
        &'a self,
        _runtime_components: &'a RuntimeComponents,
        _config_bag: &'a ConfigBag,
    ) -> IdentityFuture<'a> {
        IdentityFuture::ready({
            let token = SUPERPOSITION_AUTH
                .try_with(|v| v.bearer().map(|s| s.to_string()))
                .ok()
                .flatten()
                .or_else(|| self.fallback.as_ref().map(|s| s.expose_secret().to_string()));

            match token {
                Some(t) => Ok(Identity::new(Token::new(t, None), None)),
                None => Err("no bearer credential in task-local or fallback".into()),
            }
        })
    }
}

/// Resolves basic-auth identity from the task-local, falling back to a static value if provided.
#[derive(Debug)]
pub struct BasicResolver {
    pub fallback: Option<(String, SecretString)>,
}

impl ResolveIdentity for BasicResolver {
    fn resolve_identity<'a>(
        &'a self,
        _runtime_components: &'a RuntimeComponents,
        _config_bag: &'a ConfigBag,
    ) -> IdentityFuture<'a> {
        IdentityFuture::ready({
            let login = SUPERPOSITION_AUTH
                .try_with(|v| v.basic().map(|(u, p)| (u.to_string(), p.to_string())))
                .ok()
                .flatten()
                .or_else(|| {
                    self.fallback
                        .as_ref()
                        .map(|(u, p)| (u.clone(), p.expose_secret().to_string()))
                });

            match login {
                Some((u, p)) => Ok(Identity::new(Login::new(u, p, None), None)),
                None => Err("no basic credential in task-local or fallback".into()),
            }
        })
    }
}

pub fn shared_bearer(fallback: Option<SecretString>) -> SharedIdentityResolver {
    SharedIdentityResolver::new(BearerResolver { fallback })
}

pub fn shared_basic(fallback: Option<(String, SecretString)>) -> SharedIdentityResolver {
    SharedIdentityResolver::new(BasicResolver { fallback })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_bearer() {
        let v = AuthValue::parse_header(Some("Bearer abc123")).unwrap();
        assert_eq!(v.bearer(), Some("abc123"));
    }

    #[test]
    fn parses_bearer_case_insensitive_scheme() {
        let v = AuthValue::parse_header(Some("bearer abc")).unwrap();
        assert_eq!(v.bearer(), Some("abc"));
    }

    #[test]
    fn parses_basic() {
        let creds = base64::engine::general_purpose::STANDARD.encode("alice:s3cret");
        let v = AuthValue::parse_header(Some(&format!("Basic {}", creds))).unwrap();
        assert_eq!(v.basic(), Some(("alice", "s3cret")));
    }

    #[test]
    fn rejects_missing() {
        assert_eq!(AuthValue::parse_header(None).unwrap_err(), AuthParseError::Missing);
    }

    #[test]
    fn rejects_empty_bearer() {
        assert_eq!(
            AuthValue::parse_header(Some("Bearer ")).unwrap_err(),
            AuthParseError::Malformed
        );
    }

    #[test]
    fn rejects_unknown_scheme() {
        assert_eq!(
            AuthValue::parse_header(Some("Digest xyz")).unwrap_err(),
            AuthParseError::UnsupportedScheme
        );
    }

    #[test]
    fn rejects_malformed_basic_no_colon() {
        let creds = base64::engine::general_purpose::STANDARD.encode("no-colon-here");
        let v = AuthValue::parse_header(Some(&format!("Basic {}", creds)));
        assert_eq!(v.unwrap_err(), AuthParseError::Malformed);
    }
}
