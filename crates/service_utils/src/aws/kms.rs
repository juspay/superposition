use crate::helpers::get_from_env_unsafe;
use aws_sdk_kms::{primitives::Blob, Client};
use base64::{engine::general_purpose, Engine};

pub async fn decrypt(aws_kms_cli: Client, key: &str) -> String {
    let key_value_env: String =
        get_from_env_unsafe(key).unwrap_or_else(|_| panic!("{key} not present in env"));
    let key_value_enc = general_purpose::STANDARD
        .decode(key_value_env)
        .expect("Input string does not contain valid base 64 characters.");

    let key_value_bytes_result = aws_kms_cli
        .decrypt()
        .ciphertext_blob(Blob::new(key_value_enc))
        .send()
        .await;
    let key_value: String = String::from_utf8(
        key_value_bytes_result
            .unwrap_or_else(|_| panic!("Failed to decrypt {key}"))
            .plaintext()
            .unwrap_or_else(|| panic!("Failed to get plaintext value for {key}"))
            .as_ref()
            .to_vec(),
    )
    .expect("Could not convert to UTF-8");
    key_value
}

pub async fn new_client() -> Client {
    let config = aws_config::load_from_env().await;

    aws_sdk_kms::Client::new(&config)
}
