use secrecy::SecretString;

#[derive(Debug, Clone)]
pub enum StaticCreds {
    Bearer(SecretString),
    Basic { user: String, pass: SecretString },
}

impl StaticCreds {
    pub fn is_bearer(&self) -> bool {
        matches!(self, StaticCreds::Bearer(_))
    }
    pub fn is_basic(&self) -> bool {
        matches!(self, StaticCreds::Basic { .. })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Defaults {
    pub workspace_id: Option<String>,
    pub org_id: Option<String>,
}

#[derive(Debug, Clone)]
pub struct Config {
    pub endpoint: String,
    pub creds: Option<StaticCreds>,
    pub defaults: Defaults,
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ConfigError {
    #[error("SUPERPOSITION_ENDPOINT is required")]
    MissingEndpoint,
    #[error("provide either SUPERPOSITION_BEARER_TOKEN or SUPERPOSITION_BASIC_USER+SUPERPOSITION_BASIC_PASS, not both")]
    ConflictingCreds,
    #[error("SUPERPOSITION_BASIC_USER and SUPERPOSITION_BASIC_PASS must be set together")]
    IncompleteBasic,
    #[error("stdio mode requires credentials (bearer or basic)")]
    StdioRequiresCreds,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    Stdio,
    HttpPassthrough,
    HttpWithStaticFallback,
}

pub fn load(mode: Mode, env: &dyn EnvLookup) -> Result<Config, ConfigError> {
    let endpoint = env.get("SUPERPOSITION_ENDPOINT").ok_or(ConfigError::MissingEndpoint)?;

    let bearer = env.get("SUPERPOSITION_BEARER_TOKEN");
    let basic_user = env.get("SUPERPOSITION_BASIC_USER");
    let basic_pass = env.get("SUPERPOSITION_BASIC_PASS");

    let creds = match (bearer, basic_user, basic_pass) {
        (Some(_), Some(_), _) | (Some(_), _, Some(_)) => return Err(ConfigError::ConflictingCreds),
        (Some(b), None, None) => Some(StaticCreds::Bearer(SecretString::new(b.into()))),
        (None, Some(u), Some(p)) => Some(StaticCreds::Basic { user: u, pass: SecretString::new(p.into()) }),
        (None, Some(_), None) | (None, None, Some(_)) => return Err(ConfigError::IncompleteBasic),
        (None, None, None) => None,
    };

    if matches!(mode, Mode::Stdio) && creds.is_none() {
        return Err(ConfigError::StdioRequiresCreds);
    }

    Ok(Config {
        endpoint,
        creds,
        defaults: Defaults {
            workspace_id: env.get("SUPERPOSITION_WORKSPACE_ID"),
            org_id: env.get("SUPERPOSITION_ORG_ID"),
        },
    })
}

pub trait EnvLookup {
    fn get(&self, key: &str) -> Option<String>;
}

pub struct ProcessEnv;
impl EnvLookup for ProcessEnv {
    fn get(&self, key: &str) -> Option<String> {
        std::env::var(key).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct MapEnv(HashMap<String, String>);
    impl EnvLookup for MapEnv {
        fn get(&self, key: &str) -> Option<String> {
            self.0.get(key).cloned()
        }
    }
    fn env(pairs: &[(&str, &str)]) -> MapEnv {
        MapEnv(pairs.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect())
    }

    #[test]
    fn stdio_requires_endpoint() {
        let e = env(&[("SUPERPOSITION_BEARER_TOKEN", "t")]);
        assert_eq!(load(Mode::Stdio, &e).unwrap_err(), ConfigError::MissingEndpoint);
    }

    #[test]
    fn stdio_requires_creds() {
        let e = env(&[("SUPERPOSITION_ENDPOINT", "https://api.example.com")]);
        assert_eq!(load(Mode::Stdio, &e).unwrap_err(), ConfigError::StdioRequiresCreds);
    }

    #[test]
    fn stdio_accepts_bearer() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BEARER_TOKEN", "t"),
        ]);
        let c = load(Mode::Stdio, &e).unwrap();
        assert!(c.creds.unwrap().is_bearer());
    }

    #[test]
    fn stdio_accepts_basic() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BASIC_USER", "u"),
            ("SUPERPOSITION_BASIC_PASS", "p"),
        ]);
        let c = load(Mode::Stdio, &e).unwrap();
        assert!(c.creds.unwrap().is_basic());
    }

    #[test]
    fn rejects_both_bearer_and_basic() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BEARER_TOKEN", "t"),
            ("SUPERPOSITION_BASIC_USER", "u"),
            ("SUPERPOSITION_BASIC_PASS", "p"),
        ]);
        assert_eq!(load(Mode::Stdio, &e).unwrap_err(), ConfigError::ConflictingCreds);
    }

    #[test]
    fn rejects_incomplete_basic() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BASIC_USER", "u"),
        ]);
        assert_eq!(load(Mode::Stdio, &e).unwrap_err(), ConfigError::IncompleteBasic);
    }

    #[test]
    fn http_passthrough_accepts_no_creds() {
        let e = env(&[("SUPERPOSITION_ENDPOINT", "https://api.example.com")]);
        let c = load(Mode::HttpPassthrough, &e).unwrap();
        assert!(c.creds.is_none());
    }

    #[test]
    fn defaults_populate_from_env() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BEARER_TOKEN", "t"),
            ("SUPERPOSITION_WORKSPACE_ID", "w1"),
            ("SUPERPOSITION_ORG_ID", "o1"),
        ]);
        let c = load(Mode::Stdio, &e).unwrap();
        assert_eq!(c.defaults.workspace_id.as_deref(), Some("w1"));
        assert_eq!(c.defaults.org_id.as_deref(), Some("o1"));
    }
}
