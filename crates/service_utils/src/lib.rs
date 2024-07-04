pub mod aws;
pub mod db;
pub mod helpers;
pub mod middlewares;
pub mod service;

/// General purpose base64 engine
pub(crate) const BASE64_ENGINE: base64::engine::GeneralPurpose =
    base64::engine::general_purpose::STANDARD;
