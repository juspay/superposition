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
        Self { with_org_label: true, with_workspace_label: true }
    }
}

impl ObservabilityConfig {
    pub fn from_env() -> Result<Self, String> {
        fn env_bool(key: &str, default: bool) -> Result<bool, String> {
            match std::env::var(key) {
                Ok(v) => v.parse::<bool>().map_err(|_| format!("{key} must be true or false")),
                Err(_) => Ok(default),
            }
        }
        fn env_str(key: &str, default: &str) -> String {
            std::env::var(key).unwrap_or_else(|_| default.to_owned())
        }
        fn env_opt(key: &str) -> Option<String> {
            std::env::var(key).ok().filter(|s| !s.is_empty())
        }

        let enabled = env_bool("SUPERPOSITION_METRICS_ENABLED", true)?;
        let bind = IpAddr::from_str(&env_str("SUPERPOSITION_METRICS_BIND", "0.0.0.0"))
            .map_err(|e| format!("SUPERPOSITION_METRICS_BIND: {e}"))?;
        let port: u16 = env_str("SUPERPOSITION_METRICS_PORT", "9091")
            .parse()
            .map_err(|e| format!("SUPERPOSITION_METRICS_PORT: {e}"))?;
        let with_org_label = env_bool("SUPERPOSITION_METRICS_LABEL_ORG", true)?;
        let with_workspace_label = env_bool("SUPERPOSITION_METRICS_LABEL_WORKSPACE", true)?;
        let collect_interval =
            humantime::parse_duration(&env_str("SUPERPOSITION_METRICS_COLLECT_INTERVAL", "10s"))
                .map_err(|e| format!("SUPERPOSITION_METRICS_COLLECT_INTERVAL: {e}"))?;
        let instance_id = env_opt("SUPERPOSITION_INSTANCE_ID")
            .or_else(hostname_or_none)
            .unwrap_or_else(|| "unknown".to_owned());
        let service_name = env_str("OTEL_SERVICE_NAME", "superposition");
        let service_version = env!("CARGO_PKG_VERSION").to_owned();
        let deployment_environment = env_opt("APP_ENV").or_else(|| env_opt("DEPLOYMENT_ENV"));
        let otlp_endpoint = env_opt("OTEL_EXPORTER_OTLP_ENDPOINT");

        Ok(Self {
            enabled,
            bind,
            port,
            label: LabelConfig { with_org_label, with_workspace_label },
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

    /// Tests run with a process-global lock to avoid env races. We do not pull
    /// in a new dev-dep for this; a single Mutex + snapshot/restore is enough.
    fn with_env<F: FnOnce()>(vars: &[(&str, Option<&str>)], f: F) {
        use std::sync::Mutex;
        static LOCK: Mutex<()> = Mutex::new(());
        let _guard = LOCK.lock().unwrap();
        let prev: Vec<_> =
            vars.iter().map(|(k, _)| (k.to_string(), std::env::var(k).ok())).collect();
        for (k, v) in vars {
            match v {
                // SAFETY: single-threaded test execution guaranteed by the
                // Mutex above and --test-threads=1 at the call site.
                Some(v) => unsafe { std::env::set_var(k, v) },
                None => unsafe { std::env::remove_var(k) },
            }
        }
        f();
        for (k, v) in prev {
            match v {
                Some(v) => unsafe { std::env::set_var(&k, &v) },
                None => unsafe { std::env::remove_var(&k) },
            }
        }
    }

    #[test]
    fn defaults_when_unset() {
        with_env(
            &[
                ("SUPERPOSITION_METRICS_ENABLED", None),
                ("SUPERPOSITION_METRICS_PORT", None),
                ("SUPERPOSITION_METRICS_BIND", None),
                ("SUPERPOSITION_METRICS_LABEL_ORG", None),
                ("SUPERPOSITION_METRICS_LABEL_WORKSPACE", None),
                ("SUPERPOSITION_METRICS_COLLECT_INTERVAL", None),
                ("OTEL_EXPORTER_OTLP_ENDPOINT", None),
                ("OTEL_SERVICE_NAME", None),
            ],
            || {
                let cfg = ObservabilityConfig::from_env().unwrap();
                assert!(cfg.enabled);
                assert_eq!(cfg.port, 9091);
                assert_eq!(cfg.bind.to_string(), "0.0.0.0");
                assert!(cfg.label.with_org_label);
                assert!(cfg.label.with_workspace_label);
                assert_eq!(cfg.collect_interval, Duration::from_secs(10));
                assert_eq!(cfg.service_name, "superposition");
                assert_eq!(cfg.otlp_endpoint, None);
            },
        );
    }

    #[test]
    fn explicit_overrides() {
        with_env(
            &[
                ("SUPERPOSITION_METRICS_ENABLED", Some("false")),
                ("SUPERPOSITION_METRICS_PORT", Some("9999")),
                ("SUPERPOSITION_METRICS_BIND", Some("127.0.0.1")),
                ("SUPERPOSITION_METRICS_LABEL_WORKSPACE", Some("false")),
                ("SUPERPOSITION_METRICS_COLLECT_INTERVAL", Some("30s")),
                ("OTEL_EXPORTER_OTLP_ENDPOINT", Some("http://collector:4318")),
                ("OTEL_SERVICE_NAME", Some("sp-test")),
            ],
            || {
                let cfg = ObservabilityConfig::from_env().unwrap();
                assert!(!cfg.enabled);
                assert_eq!(cfg.port, 9999);
                assert_eq!(cfg.bind.to_string(), "127.0.0.1");
                assert!(cfg.label.with_org_label); // default still true
                assert!(!cfg.label.with_workspace_label);
                assert_eq!(cfg.collect_interval, Duration::from_secs(30));
                assert_eq!(cfg.otlp_endpoint.as_deref(), Some("http://collector:4318"));
                assert_eq!(cfg.service_name, "sp-test");
            },
        );
    }

    #[test]
    fn malformed_port_errors() {
        with_env(
            &[("SUPERPOSITION_METRICS_PORT", Some("not-a-number"))],
            || {
                let err = ObservabilityConfig::from_env().unwrap_err();
                assert!(err.contains("SUPERPOSITION_METRICS_PORT"));
            },
        );
    }
}
