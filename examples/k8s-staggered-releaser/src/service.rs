use crate::utils::{K8S_API_SERVER, TOKEN};
use reqwest::Client;
use serde_json::json;

pub async fn delete_service(service_name: String, namespace: String, client: &reqwest::Client) {
    let url = format!(
        "{}/api/v1/namespaces/{namespace}/services/{service_name}",
        K8S_API_SERVER
    );
    let response = client
        .delete(url)
        .bearer_auth(TOKEN)
        .send()
        .await
        .expect("error deleting service");
    if response.status().is_success() {
        println!("service deleted successfully!");
    }
    println!(
        "{}",
        response.json::<serde_json::Value>().await.expect("sas")
    );
}
pub async fn update_service(service_name: String, namespace: String, dep_name: String) {
    let service_val = json!({
        "apiVersion": "v1",
        "kind": "Service",
        "metadata": {
          "name": service_name.clone(),
          "labels": {
            "app": service_name.clone()
          }
        },
        "spec": {
          "ports": [
            {
              "port": 80,
              "targetPort": 80,
              "protocol": "TCP",
              "name": "http"
            }
          ],
          "selector": {
            "app": dep_name.clone()
          }
        }
    });

    let client = Client::builder()
        .danger_accept_invalid_certs(true) // ⚠️ Disables TLS verification
        .build()
        .expect("reqwest client fails");
    // let ingress_name: String =  serde_json::from_value(variant["id"].clone()).expect("Wq");
    // Kubernetes API URL
    let url = format!(
        "{}/api/v1/namespaces/{namespace}/services/{service_name}",
        K8S_API_SERVER
    );

    // Send PUT request
    let response = client
        .put(&url)
        .bearer_auth(TOKEN)
        .json(&service_val)
        .send()
        .await
        .expect("error");
    if response.status().is_success() {
        println!("service update successfully!");
    }
    println!(
        "{}",
        response.json::<serde_json::Value>().await.expect("sas")
    );
}

pub async fn create_service(service_name: String, deployment_name: String, namespace: String) {
    let service_val = json!({
        "apiVersion": "v1",
        "kind": "Service",
        "metadata": {
          "name": service_name.clone(),
          "labels": {
            "app": service_name.clone()
          }
        },
        "spec": {
          "ports": [
            {
              "port": 80,
              "targetPort": 80,
              "protocol": "TCP",
              "name": "http"
            }
          ],
          "selector": {
            "app": deployment_name.clone()
          }
        }
    });

    let client = Client::builder()
        .danger_accept_invalid_certs(true) // ⚠️ Disables TLS verification
        .build()
        .expect("reqwest client fails");
    // let ingress_name: String =  serde_json::from_value(variant["id"].clone()).expect("Wq");
    // Kubernetes API URL
    let url = format!("{}/api/v1/namespaces/{namespace}/services", K8S_API_SERVER);

    // Send PUT request
    let response = client
        .post(&url)
        .bearer_auth(TOKEN)
        .json(&service_val)
        .send()
        .await
        .expect("error");
    if response.status().is_success() {
        println!("service created successfully!");
    }
    println!(
        "{}",
        response.json::<serde_json::Value>().await.expect("sas")
    );
}
