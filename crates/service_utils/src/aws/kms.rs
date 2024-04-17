use crate::helpers::get_from_env_unsafe;
use bytes::Bytes;
use rusoto_kms::{DecryptRequest, DecryptResponse, Kms, KmsClient};
use rusoto_signature::region::Region;

//TODO refactor below code
#[allow(deprecated)]
pub async fn decrypt(client: KmsClient, secret_name: &str) -> String {
    let cypher = get_from_env_unsafe(secret_name)
        .map(|x: String| base64::decode(x).unwrap())
        .expect(format!("{secret_name} not found in env").as_str());
    let req = DecryptRequest {
        ciphertext_blob: Bytes::from(cypher),
        encryption_algorithm: None,
        encryption_context: None,
        grant_tokens: None,
        //NOTE we use symmetric key encryption therefore key_id is optional
        key_id: None,
    };
    let decrypt_resp = Kms::decrypt(&client, req).await;
    match decrypt_resp {
        Ok(DecryptResponse {
            plaintext: Some(data),
            ..
        }) => String::from_utf8(data.to_vec()).expect(
            format!("Could not convert kms val for {secret_name} to utf8").as_str(),
        ),
        e => panic!("KMS decryption failed for {secret_name} with error {e:?}"),
    }
}

pub fn new_client() -> KmsClient {
    //TODO make this an enum and add to appstate
    let app_env: String = get_from_env_unsafe("APP_ENV").unwrap_or(String::from("PROD"));

    let kms_region = match app_env.as_str() {
        "DEV" => Region::Custom {
            name: get_from_env_unsafe("AWS_REGION").unwrap_or(String::from("ap-south-1")),
            endpoint: get_from_env_unsafe("AWS_REGION_ENDPOINT")
                .unwrap_or(String::from("http://localhost:4566")),
        },
        _ => get_from_env_unsafe("AWS_REGION").unwrap_or(Region::ApSouth1),
    };

    KmsClient::new(kms_region)
}
