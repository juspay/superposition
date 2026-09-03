mod aws;
mod gcp;
mod openbao;

use crate::helpers::get_from_env_or_default;

#[derive(Debug, PartialEq)]
enum ProviderKind {
    Aws,
    Gcp,
    OpenBao,
}

fn parse_provider_kind(raw: &str) -> ProviderKind {
    match raw.to_uppercase().as_str() {
        "" | "AWS" => ProviderKind::Aws,
        "GCP" => ProviderKind::Gcp,
        "OPENBAO" => ProviderKind::OpenBao,
        other => panic!("Unknown KMS_PROVIDER '{other}', expected AWS, GCP, or OPENBAO"),
    }
}

#[async_trait::async_trait]
pub trait SecretProvider: Send + Sync {
    async fn get_secret(&self, key: &str) -> String;
    async fn get_secret_opt(&self, key: &str) -> Option<String>;
}

pub type KmsProvider = Box<dyn SecretProvider>;

pub async fn new_client() -> KmsProvider {
    let raw: String = get_from_env_or_default("KMS_PROVIDER", String::new());
    match parse_provider_kind(&raw) {
        ProviderKind::Gcp => Box::new(gcp::new_client().await),
        ProviderKind::OpenBao => Box::new(openbao::new_client().await),
        ProviderKind::Aws => Box::new(aws::new_client().await),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defaults_to_aws_when_unset() {
        assert_eq!(parse_provider_kind(""), ProviderKind::Aws);
    }

    #[test]
    fn selects_aws_case_insensitively() {
        assert_eq!(parse_provider_kind("aws"), ProviderKind::Aws);
        assert_eq!(parse_provider_kind("AWS"), ProviderKind::Aws);
    }

    #[test]
    fn selects_gcp_case_insensitively() {
        assert_eq!(parse_provider_kind("gcp"), ProviderKind::Gcp);
        assert_eq!(parse_provider_kind("GCP"), ProviderKind::Gcp);
    }

    #[test]
    fn selects_openbao_case_insensitively() {
        assert_eq!(parse_provider_kind("openbao"), ProviderKind::OpenBao);
        assert_eq!(parse_provider_kind("OPENBAO"), ProviderKind::OpenBao);
    }

    #[test]
    #[should_panic(expected = "Unknown KMS_PROVIDER 'BOGUS'")]
    fn panics_on_unrecognized_values() {
        parse_provider_kind("bogus");
    }
}
