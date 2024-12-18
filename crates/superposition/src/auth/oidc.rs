use actix_web::{
    cookie::{time::Duration, Cookie},
    dev::ServiceRequest,
    error::ErrorBadRequest,
    get,
    http::header,
    web::{self, Data, Path, Query},
    HttpRequest, HttpResponse, Responder,
};
use oidc::{
    core::{CoreClient, CoreProviderMetadata, CoreResponseType},
    AuthenticationFlow, AuthorizationCode, ClientId, ClientSecret, CsrfToken, IssuerUrl,
    Nonce, RedirectUrl, TokenResponse,
};
use openidconnect::{
    self as oidc,
    core::{
        CoreGenderClaim, CoreJsonWebKeyType, CoreJweContentEncryptionAlgorithm,
        CoreJwsSigningAlgorithm, CoreTokenType,
    },
    AdditionalClaims, EmptyExtraTokenFields, IdTokenFields, ResourceOwnerPassword,
    ResourceOwnerUsername, Scope, StandardTokenResponse, TokenUrl,
};
use serde::{Deserialize, Serialize};
use service_utils::helpers::get_from_env_unsafe;

use super::authenticator::Authenticator;

#[derive(Clone)]
pub struct OIDCAuthenticator {
    client: CoreClient,
    provider_metadata: CoreProviderMetadata,
    client_id: String,
    client_secret: String,
    base_url: String,
}

#[derive(Deserialize)]
struct LoginParams {
    code: AuthorizationCode,
    state: CsrfToken,
}

#[derive(Deserialize, Serialize)]
struct ProtectionCookie {
    csrf: CsrfToken,
    nonce: Nonce,
}

impl ProtectionCookie {
    fn from_req(req: &HttpRequest) -> Option<Self> {
        req.cookie("protection")
            .and_then(|c| serde_json::from_str(c.value()).ok())
    }
}

fn verify_presence(n: Option<&Nonce>) -> Result<(), String> {
    if n.is_some() {
        Ok(())
    } else {
        Err("missing nonce claim".to_string())
    }
}

fn no_check(_: Option<&Nonce>) -> Result<(), String> {
    Ok(())
}

#[derive(Serialize, Debug, Deserialize)]
pub struct ExtraClaims {
    accounts: Vec<String>,
    switch_pass: String,
}

impl AdditionalClaims for ExtraClaims {}

pub type CoreIdTokenFields = IdTokenFields<
    ExtraClaims,
    EmptyExtraTokenFields,
    CoreGenderClaim,
    CoreJweContentEncryptionAlgorithm,
    CoreJwsSigningAlgorithm,
    CoreJsonWebKeyType,
>;

pub type TokenRes = StandardTokenResponse<CoreIdTokenFields, CoreTokenType>;

impl OIDCAuthenticator {
    pub async fn new(
        idp_url: url::Url,
        base_url: String,
        client_id: String,
        client_secret: String,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let issuer_url = IssuerUrl::from_url(idp_url);

        // Discover OpenID Provider metadata
        let provider_metadata = CoreProviderMetadata::discover_async(
            issuer_url,
            oidc::reqwest::async_http_client,
        )
        .await?;

        // Create client
        let client = CoreClient::from_provider_metadata(
            provider_metadata.clone(),
            ClientId::new(client_id.clone()),
            Some(ClientSecret::new(client_secret.clone())),
        )
        .set_redirect_uri(RedirectUrl::new(format!("{}/oidc/login", base_url))?);

        Ok(Self {
            client,
            provider_metadata,
            client_id,
            client_secret,
            base_url,
        })
    }

    fn new_redirect(&self) -> HttpResponse {
        let (auth_url, csrf_token, nonce) = self
            .client
            .authorize_url(
                AuthenticationFlow::<CoreResponseType>::AuthorizationCode,
                CsrfToken::new_random,
                Nonce::new_random,
            )
            .add_scope(oidc::Scope::new("email".to_string()))
            .add_scope(oidc::Scope::new("profile".to_string()))
            .url();
        let protection = ProtectionCookie {
            csrf: csrf_token,
            nonce,
        };
        let pcookie =
            Cookie::build("protection", serde_json::to_string(&protection).unwrap())
                .max_age(Duration::days(7))
                // .http_only(true)
                // .same_site(SameSite::Strict)
                .path("/")
                .finish();
        HttpResponse::Found()
            .insert_header((header::LOCATION, auth_url.to_string()))
            .cookie(pcookie)
            // Deletes the cookie.
            .cookie(
                Cookie::build("user", "")
                    .max_age(Duration::seconds(0))
                    .finish(),
            )
            .finish()
    }

    fn decode_token(&self, cookie: &str) -> Option<TokenRes> {
        let ctr = serde_json::from_str::<TokenRes>(cookie)
            .map_err(|e| eprintln!("error while decoding token: {}", e))
            .ok()?;
        ctr.id_token()?
            .claims(&self.client.id_token_verifier(), verify_presence)
            .map_err(|e| eprintln!("error while getting claims: {}", e))
            .ok()?;
        Some(ctr)
    }
}

impl Authenticator for OIDCAuthenticator {
    fn authenticate(&self, request: &ServiceRequest) -> Result<(), HttpResponse> {
        let token: Option<TokenRes> = request
            .cookie("user")
            .and_then(|c| self.decode_token(c.value()));
        let p = &request.path();
        let excep = p.matches("login").count() > 0
        // Implies it's a local/un-forwarded request.
        || !request.headers().contains_key(header::USER_AGENT)
        || p.matches("health").count() > 0
        || p.matches("ready").count() > 0;
        if token.is_some() || excep {
            Ok(())
        } else {
            Err(self.new_redirect())
        }
    }

    fn routes(&self) -> actix_web::Scope {
        web::scope("oidc")
            .app_data(Data::new(self.to_owned()))
            .service(oidc_login)
            .service(oidc_accounts)
            .service(oidc_account_switch)
    }
}

#[get("/login")]
async fn oidc_login(
    auth: Data<OIDCAuthenticator>,
    req: HttpRequest,
    params: Query<LoginParams>,
) -> impl Responder {
    let pc = match ProtectionCookie::from_req(&req) {
        Some(pc) => pc,
        _ => {
            eprintln!(
                "OIDC: Missing/Bad protection-cookie: {:?}, redirecting...",
                req.cookie("protection")
            );
            return auth.new_redirect();
        }
    };

    if params.state.secret() != pc.csrf.secret() {
        eprintln!(
            "OIDC: Bad csrf, expected: {} recieved: {}",
            pc.csrf.secret(),
            params.state.secret()
        );
        return auth.new_redirect();
    }

    // Exchange the code with a token.
    let response = auth
        .client
        .exchange_code(params.code.clone())
        .request_async(oidc::reqwest::async_http_client)
        .await
        .map_err(|e| eprintln!("Failed to exchange auth-code for token: {}", e))
        .and_then(|tr| {
            tr.id_token()
                .ok_or_else(|| eprintln!("No identity-token!"))
                .and_then(|t| {
                    t.claims(&auth.client.id_token_verifier(), &pc.nonce)
                        .map_err(|e| eprintln!("Couldn't verify claims: {}", e))
                })?;
            Ok(tr)
        });

    match response {
        Ok(r) => {
            let token = serde_json::to_string(&r).unwrap();
            let cookie = Cookie::build("user", token)
                .path("/")
                .http_only(true)
                .max_age(Duration::days(1))
                .finish();
            HttpResponse::Found()
                .cookie(cookie)
                .insert_header(("Location", "/admin/accounts"))
                .finish()
        }
        _ => auth.new_redirect(),
    }
}

#[get("/accounts")]
async fn oidc_accounts(auth: Data<OIDCAuthenticator>, req: HttpRequest) -> HttpResponse {
    let accounts = req
        .cookie("user")
        .and_then(|user_cookie| auth.decode_token(user_cookie.value()))
        .and_then(|token_response| {
            token_response
                .id_token()
                .unwrap()
                .claims(&auth.client.id_token_verifier(), verify_presence)
                .ok()
                .map(|a| a.additional_claims().accounts.clone())
        });

    match accounts {
        Some(accounts) => HttpResponse::Ok().json(serde_json::json!(accounts)),
        None => auth.new_redirect(),
    }
}

#[derive(Deserialize)]
pub struct SwitchAccountPath {
    account_id: String,
}

#[get("/accounts/switch/{account_id}")]
async fn oidc_account_switch(
    auth: Data<OIDCAuthenticator>,
    req: HttpRequest,
    path: Path<SwitchAccountPath>,
) -> actix_web::Result<HttpResponse> {
    let issuer_endpoint = get_from_env_unsafe::<String>("OIDC_ISSUER_ENDPOINT_FORMAT")
        .unwrap()
        .replace("<realm>", &path.account_id);
    let token_endpoint = get_from_env_unsafe::<String>("OIDC_TOKEN_ENDPOINT_FORMAT")
        .unwrap()
        .replace("<realm>", &path.account_id);

    let provider =
        auth.provider_metadata
            .clone()
            .set_issuer(IssuerUrl::new(issuer_endpoint).map_err(|e| {
                ErrorBadRequest(format!("Unable to create issuer url {e}"))
            })?)
            .set_token_endpoint(TokenUrl::new(token_endpoint).ok());

    let client = CoreClient::from_provider_metadata(
        provider,
        ClientId::new(auth.client_id.clone()),
        Some(ClientSecret::new(auth.client_secret.clone())),
    )
    .set_redirect_uri(RedirectUrl::new(format!("{}/", auth.base_url.clone())).unwrap());

    let (username, switch_pass) = req
        .cookie("user")
        .and_then(|user_cookie| auth.decode_token(user_cookie.value()))
        .and_then(|token_response| {
            token_response
                .id_token()
                .unwrap()
                .claims(&auth.client.id_token_verifier(), verify_presence)
                .ok()
                .map(|a| {
                    (
                        a.preferred_username().cloned(),
                        a.additional_claims().switch_pass.clone(),
                    )
                })
        })
        .ok_or_else(|| ErrorBadRequest("Cookie incorrect"))?;

    let user = ResourceOwnerUsername::new(
        username
            .ok_or_else(|| ErrorBadRequest("Username not found"))?
            .to_string(),
    );
    let pass = ResourceOwnerPassword::new(switch_pass);
    let response = client
        .exchange_password(&user, &pass)
        .add_scope(Scope::new(String::from("openid")))
        .request_async(oidc::reqwest::async_http_client)
        .await
        .map_err(|e| eprintln!("Failed to switch organisation for token: {}", e))
        .and_then(|tr| {
            tr.id_token()
                .ok_or_else(|| eprintln!("No identity-token!"))
                .and_then(|t| {
                    t.claims(&client.id_token_verifier(), no_check)
                        .map_err(|e| eprintln!("Couldn't verify claims: {}", e))
                })?;
            Ok(tr)
        });

    match response {
        Ok(r) => {
            let token = serde_json::to_string(&r).unwrap();
            let cookie = Cookie::build("org_user", token)
                .path("/")
                .http_only(true)
                .max_age(Duration::days(1))
                .finish();
            Ok(HttpResponse::Found()
                .cookie(cookie)
                .insert_header(("Location", "/"))
                .finish())
        }
        _ => Ok(auth.new_redirect()),
    }
}
