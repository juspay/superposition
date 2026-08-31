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

#[derive(Clone)]
pub enum KmsProvider {
    Aws(aws_sdk_kms::Client),
    Gcp(gcp::Client),
    OpenBao(openbao::Client),
}

pub async fn new_client() -> KmsProvider {
    let raw: String = get_from_env_or_default("KMS_PROVIDER", String::new());
    match parse_provider_kind(&raw) {
        ProviderKind::Gcp => KmsProvider::Gcp(gcp::new_client().await),
        ProviderKind::OpenBao => KmsProvider::OpenBao(openbao::new_client().await),
        ProviderKind::Aws => KmsProvider::Aws(aws::new_client().await),
    }
}

pub async fn decrypt(provider: KmsProvider, key: &str) -> String {
    match provider {
        KmsProvider::Aws(client) => aws::decrypt(client, key).await,
        KmsProvider::Gcp(client) => gcp::decrypt(client, key).await,
        KmsProvider::OpenBao(client) => openbao::decrypt(client, key).await,
    }
}

pub async fn decrypt_opt(provider: KmsProvider, key: &str) -> Option<String> {
    match provider {
        KmsProvider::Aws(client) => aws::decrypt_opt(client, key).await,
        KmsProvider::Gcp(client) => gcp::decrypt_opt(client, key).await,
        KmsProvider::OpenBao(client) => openbao::decrypt_opt(client, key).await,
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
