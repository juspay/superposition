use openidconnect::{AdditionalClaims, GenderClaim, IdTokenClaims, Nonce};
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
    let user = User {
        email: claims
            .email()
            .ok_or(String::from("Username not found"))?
            .to_string(),
        username: claims
            .preferred_username()
            .ok_or(String::from("Username not found"))?
            .to_string(),
        auth_token: "token".into(),
        auth_type: "type".into(),
    };
    Ok(user)
}
