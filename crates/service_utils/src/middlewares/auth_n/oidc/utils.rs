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
    let email = claims
        .email()
        .ok_or(String::from("Email not found"))?
        .to_string();
    let username = claims
        .preferred_username()
        .ok_or(String::from("Username not found"))?
        .to_string();

    Ok(User::new(email, username))
}
