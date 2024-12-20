use actix_web::HttpRequest;
use openidconnect::{
    core::{
        CoreGenderClaim, CoreJsonWebKeyType, CoreJweContentEncryptionAlgorithm,
        CoreJwsSigningAlgorithm, CoreTokenType,
    },
    AdditionalClaims, AuthorizationCode, CsrfToken, EmptyExtraTokenFields, IdTokenClaims,
    IdTokenFields, Nonce, StandardTokenResponse,
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Debug, Deserialize, Clone)]
pub(super) struct ExtraClaims {
    pub(super) organisations: Vec<String>,
    pub(super) switch_pass: String,
}

impl AdditionalClaims for ExtraClaims {}

pub(super) type CoreIdTokenFields = IdTokenFields<
    ExtraClaims,
    EmptyExtraTokenFields,
    CoreGenderClaim,
    CoreJweContentEncryptionAlgorithm,
    CoreJwsSigningAlgorithm,
    CoreJsonWebKeyType,
>;

pub(super) type UserTokenResponse =
    StandardTokenResponse<CoreIdTokenFields, CoreTokenType>;

pub(super) type UserClaims = IdTokenClaims<ExtraClaims, CoreGenderClaim>;

#[derive(Deserialize, Serialize)]
pub(super) struct ProtectionCookie {
    pub(super) csrf: CsrfToken,
    pub(super) nonce: Nonce,
}

impl ProtectionCookie {
    pub(super) fn from_req(req: &HttpRequest) -> Option<Self> {
        req.cookie("protection")
            .and_then(|c| serde_json::from_str(c.value()).ok())
    }
}

#[derive(Deserialize)]
pub(super) struct LoginParams {
    pub(super) code: AuthorizationCode,
    pub(super) state: CsrfToken,
}
