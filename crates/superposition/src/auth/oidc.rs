use actix_web::{
    cookie::{time::Duration, Cookie, SameSite},
    dev::ServiceRequest,
    get,
    http::header,
    web::{self, Data, Query}, HttpRequest, HttpResponse, Responder,
};

use oidc::{
    core::{CoreClient, CoreProviderMetadata, CoreResponseType, CoreTokenResponse},
    AuthenticationFlow, AuthorizationCode, ClientId, ClientSecret, CsrfToken, IssuerUrl,
    Nonce, RedirectUrl, TokenResponse,
};
use openidconnect as oidc;
use serde::{Deserialize, Serialize};

use super::authenticator::Authenticator;

#[derive(Clone)]
pub struct OIDCAuthenticator(CoreClient);

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

impl OIDCAuthenticator {
    pub fn new(
        idp_url: url::Url,
        base_url: String,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let issuer_url = IssuerUrl::from_url(idp_url);

        // Discover OpenID Provider metadata
        let provider_metadata =
            CoreProviderMetadata::discover(&issuer_url, oidc::reqwest::http_client)?;

        // Create client
        let client = CoreClient::from_provider_metadata(
            provider_metadata,
            ClientId::new("client_id".to_string()),
            Some(ClientSecret::new("client_secret".to_string())),
        )
        .set_redirect_uri(RedirectUrl::new(format!("{}/oidc/login", base_url))?);

        Ok(Self(client))
    }

    fn new_redirect(&self) -> HttpResponse {
        let (auth_url, csrf_token, nonce) = self
            .0
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
                .http_only(true)
                .same_site(SameSite::Strict)
                .finish();
        HttpResponse::Found()
            .insert_header((header::LOCATION, auth_url.to_string()))
            .cookie(pcookie)
            .finish()
    }

    fn decode_token(&self, cookie: &str) -> Option<CoreTokenResponse> {
        let ctr = serde_json::from_str::<CoreTokenResponse>(cookie)
            .map_err(|e| eprintln!("error while decoding token: {}", e))
            .ok()?;
        ctr.id_token()?
            .claims(&self.0.id_token_verifier(), verify_presence)
            .map_err(|e| eprintln!("error while getting claims: {}", e))
            .ok()?;
        Some(ctr)
    }
}

impl Authenticator for OIDCAuthenticator {
    fn authenticate(&self, request: &ServiceRequest) -> Result<(), HttpResponse> {
        let token: Option<CoreTokenResponse> = request
            .cookie("user")
            .and_then(|c| self.decode_token(c.value()));
        let p = &request.path();
        let excep = p.matches("login").count() > 0
        // Implies it's a local/un-forwarded request.
        || !request.headers().contains_key(header::X_FORWARDED_HOST)
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
            eprintln!("OIDC: Missing/Bad protection-cookie, redirecting...");
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
        .0
        .exchange_code(params.code.clone())
        .request_async(oidc::reqwest::async_http_client)
        .await
        .map_err(|e| eprintln!("Failed to exchange auth-code for token: {}", e))
        .and_then(|tr| {
            tr.id_token()
                .ok_or_else(|| eprintln!("No identity-token!"))
                .and_then(|t| {
                    t.claims(&auth.0.id_token_verifier(), &pc.nonce)
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
                .insert_header(("Location", "/"))
                .finish()
        }
        _ => auth.new_redirect(),
    }
}
