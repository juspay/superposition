//! Configuration for the observability subsystem, parsed from env vars.

use std::{net::IpAddr, str::FromStr, time::Duration};

#[derive(Debug, Clone)]
pub struct ObservabilityConfig {
    pub enabled: bool,
    pub bind: IpAddr,
    pub port: u16,
    pub label: LabelConfig,
    pub collect_interval: Duration,
    pub instance_id: String,
    pub service_name: String,
    pub service_version: String,
    pub deployment_environment: Option<String>,
    pub otlp_endpoint: Option<String>,
}

#[derive(Debug, Clone, Copy)]
pub struct LabelConfig {
    pub with_org_label: bool,
    pub with_workspace_label: bool,
}

impl Default for LabelConfig {
    fn default() -> Self {
        Self {
            with_org_label: true,
            with_workspace_label: true,
        }
    }
}

impl ObservabilityConfig {
    /// Parse from the process environment via `std::env::var`.
    pub fn from_env() -> Result<Self, String> {
        Self::from_source(|k| std::env::var(k).ok())
    }

    /// Generic over the env source for testability.
    ///
    /// `get(key)` returns `Some(value)` when the key is set, `None` when absent.
    /// This keeps tests pure (no process-global env mutations) and parallel-safe.
    pub fn from_source<F>(get: F) -> Result<Self, String>
    where
        F: Fn(&str) -> Option<String>,
    {
        fn parse_bool(
            name: &str,
            raw: Option<String>,
            default: bool,
        ) -> Result<bool, String> {
            match raw {
                Some(v) => v
                    .parse::<bool>()
                    .map_err(|_| format!("{name} must be true or false")),
                None => Ok(default),
            }
        }

        fn get_str(
            get: &impl Fn(&str) -> Option<String>,
            key: &str,
            default: &str,
        ) -> String {
            get(key).unwrap_or_else(|| default.to_owned())
        }

        fn get_opt(get: &impl Fn(&str) -> Option<String>, key: &str) -> Option<String> {
            get(key).filter(|s| !s.is_empty())
        }

        let enabled = parse_bool(
            "SUPERPOSITION_METRICS_ENABLED",
            get("SUPERPOSITION_METRICS_ENABLED"),
            true,
        )?;
        let bind =
            IpAddr::from_str(&get_str(&get, "SUPERPOSITION_METRICS_BIND", "0.0.0.0"))
                .map_err(|e| format!("SUPERPOSITION_METRICS_BIND: {e}"))?;
        let port: u16 = get_str(&get, "SUPERPOSITION_METRICS_PORT", "9091")
            .parse()
            .map_err(|e| format!("SUPERPOSITION_METRICS_PORT: {e}"))?;
        let with_org_label = parse_bool(
            "SUPERPOSITION_METRICS_LABEL_ORG",
            get("SUPERPOSITION_METRICS_LABEL_ORG"),
            true,
        )?;
        let with_workspace_label = parse_bool(
            "SUPERPOSITION_METRICS_LABEL_WORKSPACE",
            get("SUPERPOSITION_METRICS_LABEL_WORKSPACE"),
            true,
        )?;
        let collect_interval = humantime::parse_duration(&get_str(
            &get,
            "SUPERPOSITION_METRICS_COLLECT_INTERVAL",
            "10s",
        ))
        .map_err(|e| format!("SUPERPOSITION_METRICS_COLLECT_INTERVAL: {e}"))?;

        // instance_id: env var takes precedence, then /etc/hostname, then "unknown".
        let instance_id = get_opt(&get, "SUPERPOSITION_INSTANCE_ID")
            .or_else(hostname_or_none)
            .unwrap_or_else(|| "unknown".to_owned());

        // service.name: OTEL standard env var.
        let service_name = get_str(&get, "OTEL_SERVICE_NAME", "superposition");

        // service.version: always the build-time crate version.
        let service_version = env!("CARGO_PKG_VERSION").to_owned();

        let deployment_environment =
            get_opt(&get, "APP_ENV").or_else(|| get_opt(&get, "DEPLOYMENT_ENV"));

        let otlp_endpoint = get_opt(&get, "OTEL_EXPORTER_OTLP_ENDPOINT");

        Ok(Self {
            enabled,
            bind,
            port,
            label: LabelConfig {
                with_org_label,
                with_workspace_label,
            },
            collect_interval,
            instance_id,
            service_name,
            service_version,
            deployment_environment,
            otlp_endpoint,
        })
    }
}

fn hostname_or_none() -> Option<String> {
    // Avoid pulling in a hostname crate; read /etc/hostname on Linux/macOS.
    std::fs::read_to_string("/etc/hostname")
        .ok()
        .map(|s| s.trim().to_owned())
        .filter(|s| !s.is_empty())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn lookup(map: HashMap<&str, &str>) -> impl Fn(&str) -> Option<String> {
        let owned: HashMap<String, String> = map
            .into_iter()
            .map(|(k, v)| (k.to_owned(), v.to_owned()))
            .collect();
        move |k| owned.get(k).cloned()
    }

    #[test]
    fn defaults_when_unset() {
        let cfg = ObservabilityConfig::from_source(|_| None).unwrap();
        assert!(cfg.enabled);
        assert_eq!(cfg.port, 9091);
        assert_eq!(cfg.bind.to_string(), "0.0.0.0");
        assert!(cfg.label.with_org_label);
        assert!(cfg.label.with_workspace_label);
        assert_eq!(cfg.collect_interval, Duration::from_secs(10));
        assert_eq!(cfg.service_name, "superposition");
        assert_eq!(cfg.otlp_endpoint, None);
    }

    #[test]
    fn explicit_overrides() {
        let cfg = ObservabilityConfig::from_source(lookup(HashMap::from([
            ("SUPERPOSITION_METRICS_ENABLED", "false"),
            ("SUPERPOSITION_METRICS_PORT", "9999"),
            ("SUPERPOSITION_METRICS_BIND", "127.0.0.1"),
            ("SUPERPOSITION_METRICS_LABEL_WORKSPACE", "false"),
            ("SUPERPOSITION_METRICS_COLLECT_INTERVAL", "30s"),
            ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://collector:4318"),
            ("OTEL_SERVICE_NAME", "sp-test"),
        ])))
        .unwrap();
        assert!(!cfg.enabled);
        assert_eq!(cfg.port, 9999);
        assert_eq!(cfg.bind.to_string(), "127.0.0.1");
        assert!(cfg.label.with_org_label); // default still true
        assert!(!cfg.label.with_workspace_label);
        assert_eq!(cfg.collect_interval, Duration::from_secs(30));
        assert_eq!(cfg.otlp_endpoint.as_deref(), Some("http://collector:4318"));
        assert_eq!(cfg.service_name, "sp-test");
    }

    #[test]
    fn malformed_port_errors() {
        let err = ObservabilityConfig::from_source(lookup(HashMap::from([(
            "SUPERPOSITION_METRICS_PORT",
            "not-a-number",
        )])))
        .unwrap_err();
        assert!(err.contains("SUPERPOSITION_METRICS_PORT"));
    }
}
