use serde_json::{json, Value};

use crate::utils::{K8S_API_SERVER, TOKEN};

pub async fn create_ingress(
    client: &reqwest::Client,
    _variant: Value,
    traffic_percentage: String,
    service: String,
    service_name: String,
    ingress_name: String,
    namespace: String,
    _app_name: String,
) {
 

    let val = json!({
        "apiVersion": "networking.k8s.io/v1",
        "kind": "Ingress",
        "metadata": {
        "name": ingress_name.clone(),
        "annotations": {"nginx.ingress.kubernetes.io/canary": "true",
        "nginx.ingress.kubernetes.io/canary-weight": traffic_percentage}
        },
        "spec": {
        "ingressClassName": "nginx",
        "rules": [
            {
            "host": format!("{service}.{namespace}"),
            "http": {
                "paths": [
                {
                    "pathType": "Prefix",
                    "path": "/",
                    "backend": {
                    "service": {
                        "name": service_name.clone(),
                        "port": {
                        "number": 80
                        }
                    }
                    }
                }
                ]
            }
            }
        ]
        }
    });

    // Kubernetes API URL
    let url = format!(
        "{}/apis/networking.k8s.io/v1/namespaces/{namespace}/ingresses",
        K8S_API_SERVER
    );

    // Send PUT request
    let response = client
        .post(&url)
        .bearer_auth(TOKEN)
        .json(&val)
        .send()
        .await
        .expect("error");
    if response.status().is_success() {
        println!("Ingress created successfully!");
    }
    println!(
        "{}",
        response.json::<serde_json::Value>().await.expect("sas")
    );
}

pub async fn update_ingress(
    client: &reqwest::Client,
    _variant: Value,
    traffic_percentage: String,
    service: String,
    service_name: String,
    ingress_name: String,
    namespace: String,
    _app_name: String,
) {

    let val = json!({
        "apiVersion": "networking.k8s.io/v1",
        "kind": "Ingress",
        "metadata": {
        "name": ingress_name.clone(),
        "annotations": {"nginx.ingress.kubernetes.io/canary": "true",
        "nginx.ingress.kubernetes.io/canary-weight": traffic_percentage }
        },
        "spec": {
        "ingressClassName": "nginx",
        "rules": [
            {
            "host": format!("{service}.{namespace}"),
            "http": {
                "paths": [
                {
                    "pathType": "Prefix",
                    "path": "/",
                    "backend": {
                    "service": {
                        "name": service_name.clone(),
                        "port": {
                        "number": 80
                        }
                    }
                    }
                }
                ]
            }
            }
        ]
        }
    });

    // Kubernetes API URL
    let url = format!(
        "{}/apis/networking.k8s.io/v1/namespaces/{namespace}/ingresses/{ingress_name}",
        K8S_API_SERVER
    );

    // Send PUT request
    let response = client
        .put(&url)
        .bearer_auth(TOKEN)
        .json(&val)
        .send()
        .await
        .expect("error");
    if response.status().is_success() {
        println!("Ingress updated successfully!");
    }
    println!(
        "{}",
        response.json::<serde_json::Value>().await.expect("sas")
    );
}

pub async fn delete_ingress(ingress_name: String, namespace: String, client: &reqwest::Client) {
    let url = format!(
        "{}/apis/networking.k8s.io/v1/namespaces/{namespace}/ingresses/{ingress_name}",
        K8S_API_SERVER
    );
    let response = client
        .delete(url)
        .bearer_auth(TOKEN)
        .send()
        .await
        .expect("error ingress delete");

    if response.status().is_success() {
        println!("Ingress deleted successfully!");
    }
    println!(
        "{}",
        response.json::<serde_json::Value>().await.expect("sas")
    );
}
