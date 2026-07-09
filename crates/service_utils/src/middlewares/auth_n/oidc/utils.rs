use openidconnect::{
    self as oidcrs, AdditionalClaims, ClientId, ClientSecret, GenderClaim, IdTokenClaims,
    IssuerUrl, Nonce, RedirectUrl,
    core::{CoreClient, CoreProviderMetadata},
};
use superposition_types::User;

pub(super) fn verify_presence(n: Option<&Nonce>) -> Result<(), String> {
    if n.is_some() {
        Ok(())
    } else {
        Err("missing nonce claim".to_string())
    }
}

pub(super) fn presence_no_check(_: Option<&Nonce>) -> Result<(), String> {
    Ok(())
}

pub(super) fn try_user_from<A: AdditionalClaims, B: GenderClaim>(
    claims: &IdTokenClaims<A, B>,
) -> Result<User, String> {
    let email = claims
        .email()
        .ok_or(String::from("Email not found"))?
        .to_string();
    let username = claims
        .preferred_username()
        .map(|u| u.to_string())
        .or_else(|| claims.email().map(|e| e.to_string()))
        .ok_or(String::from("Username not found"))?;

    Ok(User::new(email, username))
}

pub(super) async fn fetch_provider_metadata(
    issuer_url: IssuerUrl,
) -> Result<CoreProviderMetadata, Box<dyn std::error::Error>> {
    let provider_metadata = CoreProviderMetadata::discover_async(
        issuer_url,
        oidcrs::reqwest::async_http_client,
    )
    .await?;

    Ok(provider_metadata)
}

pub(super) fn build_client(
    provider_metadata: CoreProviderMetadata,
    client_id: ClientId,
    client_secret: ClientSecret,
    redirect_url: RedirectUrl,
) -> CoreClient {
    CoreClient::from_provider_metadata(provider_metadata, client_id, Some(client_secret))
        .set_redirect_uri(redirect_url)
}
