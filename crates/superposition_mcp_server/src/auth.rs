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
