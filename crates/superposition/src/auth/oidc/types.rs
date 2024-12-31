use actix_web::HttpRequest;
use openidconnect::{
    core::{
        CoreGenderClaim, CoreIdTokenClaims, CoreJsonWebKeyType,
        CoreJweContentEncryptionAlgorithm, CoreJwsSigningAlgorithm, CoreTokenResponse,
        CoreTokenType,
    },
    AdditionalClaims, AuthorizationCode, CsrfToken, EmptyExtraTokenFields, IdTokenClaims,
    IdTokenFields, Nonce, StandardTokenResponse,
};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Debug, Deserialize, Clone)]
pub(super) struct GlobalUserExtraClaims {
    pub(super) organisations: Vec<String>,
    pub(super) switch_pass: String,
}

impl AdditionalClaims for GlobalUserExtraClaims {}

pub(super) type GlobalUserCoreIdTokenFields = IdTokenFields<
    GlobalUserExtraClaims,
    EmptyExtraTokenFields,
    CoreGenderClaim,
    CoreJweContentEncryptionAlgorithm,
    CoreJwsSigningAlgorithm,
    CoreJsonWebKeyType,
>;

pub(super) type GlobalUserTokenResponse =
    StandardTokenResponse<GlobalUserCoreIdTokenFields, CoreTokenType>;
pub(super) type GlobalUserClaims = IdTokenClaims<GlobalUserExtraClaims, CoreGenderClaim>;

pub(super) type OrgUserTokenResponse = CoreTokenResponse;
pub(super) type OrgUserClaims = CoreIdTokenClaims;

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
