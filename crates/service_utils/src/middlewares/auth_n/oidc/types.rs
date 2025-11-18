use actix_web::HttpRequest;
use base64::{engine::general_purpose, Engine};
use openidconnect::{
    core::{
        CoreGenderClaim, CoreIdTokenClaims, CoreJsonWebKeyType,
        CoreJweContentEncryptionAlgorithm, CoreJwsSigningAlgorithm, CoreTokenResponse,
        CoreTokenType,
    },
    AdditionalClaims, AuthorizationCode, CsrfToken, EmptyExtraTokenFields, IdTokenClaims,
    IdTokenFields, Nonce, StandardTokenResponse,
};
use serde::{Deserialize, Deserializer, Serialize};

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

#[derive(Serialize)]
pub(super) struct ProtectionCookie {
    pub(super) csrf: CsrfToken,
    pub(super) nonce: Nonce,
}

impl<'de> Deserialize<'de> for ProtectionCookie {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper {
            csrf: String,
            nonce: Nonce,
        }
        let helper = Helper::deserialize(deserializer)?;

        let base64_decoded = general_purpose::STANDARD
            .decode(&helper.csrf)
            .map_err(serde::de::Error::custom)?;
        let state: RedirectionState =
            serde_json::from_slice(&base64_decoded).map_err(serde::de::Error::custom)?;

        Ok(Self {
            nonce: helper.nonce,
            csrf: state.csrf,
        })
    }
}

impl ProtectionCookie {
    pub(super) fn from_req(req: &HttpRequest) -> Option<Self> {
        req.cookie("protection")
            .and_then(|c| serde_json::from_str(c.value()).ok())
    }
}

#[derive(Serialize, Deserialize)]
pub(super) struct RedirectionState {
    pub(super) csrf: CsrfToken,
    pub(super) redirect_uri: String,
}

pub(super) struct LoginParams {
    pub(super) code: AuthorizationCode,
    pub(super) state: RedirectionState,
}

impl<'de> Deserialize<'de> for LoginParams {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        #[derive(Deserialize)]
        struct Helper {
            code: AuthorizationCode,
            state: String,
        }
        let helper = Helper::deserialize(deserializer)?;

        let base64_decoded = general_purpose::STANDARD
            .decode(helper.state)
            .map_err(serde::de::Error::custom)?;
        let state: RedirectionState =
            serde_json::from_slice(&base64_decoded).map_err(serde::de::Error::custom)?;

        Ok(Self {
            code: helper.code,
            state,
        })
    }
}
