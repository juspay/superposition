//! Superposition's `authn_kit` integration.
//!
//! Four things the crate deliberately knows nothing about, supplied here:
//!
//! * [`principal`] — what a principal is, and how claims become one
//! * [`scope`] — what a request scope is, and how a request maps to one
//! * [`internal`] — the trusted `Internal` service-to-service scheme
//! * [`dispatcher`] — the Kronos webhook callback credential
//!
//! Together these replace `middlewares::auth_n`, whose equivalents were welded
//! into the authenticator implementations themselves.

pub mod dispatcher;
pub mod internal;
pub mod principal;
pub mod public;
pub mod routes;
pub mod scope;
pub mod setup;

pub use dispatcher::DispatcherAuthenticator;
pub use internal::InternalAuthenticator;
pub use principal::{PrincipalKind, SuperpositionPrincipal, SuperpositionProfile};
pub use public::PublicScopeAuthenticator;
pub use routes::{AuthRoutesState, oidc_routes, org_routes};
pub use scope::{SuperpositionScope, SuperpositionScopes};
pub use setup::{AuthProvider, AuthnSettings, SuperpositionAuthn};
