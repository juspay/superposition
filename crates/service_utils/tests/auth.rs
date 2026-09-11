//! Superposition's `authn_kit` integration: principal mapping, scope
//! resolution, and the two application-specific authenticators.
//!
//! These are parity tests. Each asserts that the new stack reproduces
//! `middlewares::auth_n` exactly, because anything that differs silently is a
//! production incident rather than a test failure.

use std::collections::HashSet;

use authn_kit::{
    AuthContext, AuthRequest, AuthScope, Authenticator, ClaimSource, Credential,
    IdentityClaims, ScopeResolver, Verdict,
};
use secrecy::SecretString;
use service_utils::auth::{
    DispatcherAuthenticator, InternalAuthenticator, PrincipalKind,
    SuperpositionPrincipal, SuperpositionProfile, SuperpositionScope,
    SuperpositionScopes,
};
use superposition_types::User;

/// `User`'s fields are private and only `get_email` is exposed, so identity is
/// compared through its serialisation.
fn fields(user: &User) -> (String, String) {
    let value = serde_json::to_value(user).unwrap();
    (
        value["email"].as_str().unwrap().to_string(),
        value["username"].as_str().unwrap().to_string(),
    )
}

fn principal(claims: IdentityClaims) -> Result<SuperpositionPrincipal, String> {
    SuperpositionPrincipal::try_from(claims)
}

// ============================================================ cookie names
//
// Blocking migration item: `Display` on the scope is the session cookie name.
// If these drift from the original's `Login::to_string()`, every browser
// presents a cookie nothing reads and every logged-in user is silently signed
// out on deploy.

#[test]
fn scope_display_matches_the_original_cookie_names() {
    assert_eq!(SuperpositionScope::None.to_string(), "none");
    assert_eq!(SuperpositionScope::Global.to_string(), "user");
    assert_eq!(
        SuperpositionScope::Org("acme".into()).to_string(),
        "org_acme"
    );

    // And the cookie name is taken from `Display`.
    assert_eq!(SuperpositionScope::Global.session_cookie_name(), "user");
    assert_eq!(
        SuperpositionScope::Org("acme".into()).session_cookie_name(),
        "org_acme"
    );
}

/// No scope is `is_public` in authn_kit's sense, because that would yield no
/// principal at all. The original gave excluded routes a fixed `User::default()`,
/// so `PublicScopeAuthenticator` supplies one instead.
#[test]
fn no_scope_is_anonymous_because_excluded_routes_get_a_default_identity() {
    assert!(!SuperpositionScope::None.is_public());
    assert!(!SuperpositionScope::Global.is_public());
    assert!(!SuperpositionScope::Org("acme".into()).is_public());
}

/// The org is part of every cache key, so a credential validated for one
/// organisation can never be served from cache for another.
#[test]
fn only_org_scopes_carry_a_cache_discriminator() {
    assert_eq!(
        SuperpositionScope::Org("acme".into()).cache_discriminator(),
        Some("acme")
    );
    assert_eq!(SuperpositionScope::Global.cache_discriminator(), None);
    assert_eq!(SuperpositionScope::None.cache_discriminator(), None);
}

// ========================================================= scope resolution

fn scopes() -> SuperpositionScopes {
    let exclusions: HashSet<String> = [
        "/health",
        "/assets",
        "/pkg",
        "/admin",
        "/oidc/login",
        "/organisations",
        "/",
    ]
    .iter()
    .map(|s| s.to_string())
    .collect();
    SuperpositionScopes::new(exclusions, String::new())
}

fn request(path: &str) -> AuthRequest {
    AuthRequest::builder().method("GET").path(path).build()
}

#[test]
fn excluded_routes_are_public() {
    for path in ["/health", "/assets", "/pkg", "/admin", "/oidc/login", "/"] {
        assert_eq!(
            scopes().resolve(&request(path)),
            SuperpositionScope::None,
            "for {path}"
        );
    }
}

/// The original matched `(_, true)` before `(true, false)`, so a path in one of
/// the global families is `Global` **even when it also appears in the exclusion
/// list**. `/organisations` is in the default list and is nonetheless
/// authenticated; getting this wrong would leave the org endpoints open.
#[test]
fn global_families_win_over_the_exclusion_list() {
    assert_eq!(
        scopes().resolve(&request("/organisations")),
        SuperpositionScope::Global
    );
}

#[test]
fn global_families_are_recognised() {
    for path in [
        "/organisations",
        "/admin/organisations",
        "/authz/admin/policies",
        "/admin/settings",
        "/api/admin/settings/x",
    ] {
        assert_eq!(
            scopes().resolve(&request(path)),
            SuperpositionScope::Global,
            "for {path}"
        );
    }
}

#[test]
fn the_organisation_id_is_read_from_the_same_three_places() {
    let from_header = AuthRequest::builder()
        .path("/config")
        .header("x-org-id", "from-header")
        .build();
    assert_eq!(
        scopes().resolve(&from_header),
        SuperpositionScope::Org("from-header".into())
    );

    let from_path = AuthRequest::builder()
        .path("/admin/from-path/workspaces")
        .route_pattern(Some("/admin/{org_id}/workspaces".to_string()))
        .build();
    assert_eq!(
        scopes().resolve(&from_path),
        SuperpositionScope::Org("from-path".into())
    );

    let from_query = AuthRequest::builder()
        .path("/config")
        .query("org=from-query")
        .build();
    assert_eq!(
        scopes().resolve(&from_query),
        SuperpositionScope::Org("from-query".into())
    );
}

/// The original used `unwrap_or_default()`, so an unidentified org became the
/// empty string rather than an error.
#[test]
fn an_absent_organisation_id_is_the_empty_string() {
    assert_eq!(
        scopes().resolve(&request("/config")),
        SuperpositionScope::Org(String::new())
    );
}

#[test]
fn the_path_prefix_is_stripped_before_matching_exclusions() {
    let scopes = SuperpositionScopes::new(
        ["/health".to_string()].into_iter().collect(),
        "/superposition".to_string(),
    );

    assert_eq!(
        scopes.resolve(&request("/superposition/health")),
        SuperpositionScope::None
    );
}

// ========================================================= principal mapping

#[test]
fn id_token_claims_map_like_try_user_from() {
    let claims = IdentityClaims::new(ClaimSource::IdToken)
        .with_email("alice@example.com")
        .with_preferred_username("alice");

    let resolved = principal(claims).unwrap();
    assert_eq!(resolved.kind, PrincipalKind::Standard);
    assert_eq!(
        fields(&resolved.user),
        ("alice@example.com".into(), "alice".into())
    );
}

#[test]
fn an_id_token_username_falls_back_to_the_email() {
    let claims =
        IdentityClaims::new(ClaimSource::IdToken).with_email("alice@example.com");

    assert_eq!(
        fields(&principal(claims).unwrap().user),
        ("alice@example.com".into(), "alice@example.com".into())
    );
}

#[test]
fn an_id_token_without_an_email_is_rejected() {
    let claims = IdentityClaims::new(ClaimSource::IdToken).with_subject("sub-1");
    assert_eq!(principal(claims).unwrap_err(), "Email not found");
}

/// Blocking migration item: the principal string is the Casbin subject, so the
/// `service-account-` prefix must survive.
#[test]
fn client_credentials_reproduce_the_service_account_naming() {
    let claims = IdentityClaims::new(ClaimSource::ClientCredentials)
        .with_client_id("reporting")
        .with_subject("reporting");

    let resolved = principal(claims).unwrap();
    assert_eq!(
        fields(&resolved.user),
        (
            "service-account-reporting".into(),
            "service-account-reporting".into()
        )
    );
}

/// Introspection was permissive in the original: `username` -> `sub` -> `email`,
/// with the email defaulting to whichever was found.
#[test]
fn introspection_identity_falls_back_across_claims() {
    let by_username = IdentityClaims::new(ClaimSource::Introspection)
        .with_preferred_username("svc")
        .with_subject("sub-1")
        .with_email("svc@example.com");
    assert_eq!(
        fields(&principal(by_username).unwrap().user),
        ("svc@example.com".into(), "svc".into())
    );

    let by_subject =
        IdentityClaims::new(ClaimSource::Introspection).with_subject("sub-1");
    assert_eq!(
        fields(&principal(by_subject).unwrap().user),
        ("sub-1".into(), "sub-1".into())
    );

    let nothing = IdentityClaims::new(ClaimSource::Introspection);
    assert!(principal(nothing).is_err());
}

#[test]
fn a_static_token_email_defaults_to_its_principal() {
    let without_email = IdentityClaims::new(ClaimSource::StaticToken)
        .with_preferred_username("svc-local")
        .with_subject("svc-local");
    assert_eq!(
        fields(&principal(without_email).unwrap().user),
        ("svc-local".into(), "svc-local".into())
    );

    let with_email = IdentityClaims::new(ClaimSource::StaticToken)
        .with_preferred_username("svc-local")
        .with_email("ops@example.com");
    assert_eq!(
        fields(&principal(with_email).unwrap().user),
        ("ops@example.com".into(), "svc-local".into())
    );
}

// ====================================================== internal scheme

fn internal_credential(token: &str) -> Credential {
    Credential::Other {
        scheme: "internal".into(),
        value: SecretString::from(token.to_string()),
    }
}

fn declined(verdict: &Verdict<SuperpositionPrincipal>) -> bool {
    matches!(verdict, Verdict::NotApplicable)
}

async fn run<A: Authenticator<SuperpositionProfile>>(
    authenticator: &A,
    request: &AuthRequest,
    credential: &Credential,
) -> Verdict<SuperpositionPrincipal> {
    authenticator
        .authenticate(&AuthContext::new(
            request,
            credential,
            &SuperpositionScope::Global,
        ))
        .await
        .unwrap()
}

#[tokio::test]
async fn the_internal_scheme_trusts_the_asserted_user() {
    let auth = InternalAuthenticator::new("service-token");
    let request = AuthRequest::builder()
        .path("/config")
        .header("x-user", r#"{"email":"ops@example.com","username":"ops"}"#)
        .build();

    let Verdict::Authenticated(resolved) =
        run(&auth, &request, &internal_credential("service-token")).await
    else {
        panic!("expected Authenticated");
    };

    assert_eq!(resolved.kind, PrincipalKind::Internal);
    assert_eq!(
        fields(&resolved.user),
        ("ops@example.com".into(), "ops".into())
    );
}

#[tokio::test]
async fn a_wrong_internal_token_declines() {
    let auth = InternalAuthenticator::new("service-token");
    let request = AuthRequest::builder()
        .header("x-user", r#"{"email":"ops@example.com","username":"ops"}"#)
        .build();

    assert!(declined(
        &run(&auth, &request, &internal_credential("wrong")).await
    ));
}

/// Parity: the original returned `None` here, so the request stayed eligible for
/// cookie authentication rather than failing outright.
#[tokio::test]
async fn a_valid_internal_token_with_no_usable_user_header_declines() {
    let auth = InternalAuthenticator::new("service-token");

    for request in [
        AuthRequest::builder().path("/config").build(),
        AuthRequest::builder().header("x-user", "not json").build(),
        AuthRequest::builder()
            .header("x-user", r#"{"email":"only"}"#)
            .build(),
    ] {
        assert!(declined(
            &run(&auth, &request, &internal_credential("service-token")).await
        ));
    }
}

#[tokio::test]
async fn the_internal_scheme_ignores_other_credentials() {
    let auth = InternalAuthenticator::new("service-token");
    let request = AuthRequest::default();

    for credential in [
        Credential::None,
        Credential::Bearer(SecretString::from("t".to_string())),
    ] {
        assert!(declined(&run(&auth, &request, &credential).await));
    }
}

// ==================================================== kronos dispatcher

fn basic(id: &str, secret: &str) -> Credential {
    Credential::Basic {
        id: id.to_string(),
        secret: SecretString::from(secret.to_string()),
    }
}

#[tokio::test]
async fn the_dispatcher_credential_authenticates_on_the_webhook_path() {
    let auth = DispatcherAuthenticator::new("dispatch-token");
    let request = AuthRequest::builder()
        .path("/superposition/dispatch/webhook")
        .build();

    let Verdict::Authenticated(resolved) = run(
        &auth,
        &request,
        &basic("kronos-dispatcher", "dispatch-token"),
    )
    .await
    else {
        panic!("expected Authenticated");
    };

    assert_eq!(resolved.kind, PrincipalKind::Dispatch);
    assert_eq!(
        fields(&resolved.user),
        (
            "kronos-dispatcher@superposition.io".into(),
            "kronos-dispatcher".into()
        )
    );
}

/// The credential is confined to the webhook route, so it cannot be replayed
/// elsewhere — which matters because `auth_z` skips authorization for it.
#[tokio::test]
async fn the_dispatcher_credential_is_confined_to_the_webhook_path() {
    let auth = DispatcherAuthenticator::new("dispatch-token");

    for path in ["/config", "/dispatch", "/dispatch/webhook/other"] {
        let request = AuthRequest::builder().path(path).build();
        assert!(
            declined(
                &run(
                    &auth,
                    &request,
                    &basic("kronos-dispatcher", "dispatch-token")
                )
                .await
            ),
            "for {path}"
        );
    }
}

#[tokio::test]
async fn a_non_dispatcher_basic_credential_falls_through() {
    let auth = DispatcherAuthenticator::new("dispatch-token");
    let request = AuthRequest::builder().path("/dispatch/webhook").build();

    for credential in [
        basic("someone-else", "dispatch-token"),
        basic("kronos-dispatcher", "wrong-token"),
    ] {
        assert!(declined(&run(&auth, &request, &credential).await));
    }
}
