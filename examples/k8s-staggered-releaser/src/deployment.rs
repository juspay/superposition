use serde_json::{json, Value};

use crate::utils::{K8S_API_SERVER, TOKEN};



pub fn generate_deployment_config(config: Value, dep_name: String) -> Value {
    let mut env_object: Vec<Value> = Vec::new();

    if let Value::Object(map) = config.clone() {
        for (key, value) in &map {
            if key.contains("env.") {
                env_object.push(json!({"name": key.replace("env.", ""), "value": value.clone()}));
            }
        }
    }

    let dep_val = json!({
        "apiVersion": "apps/v1",
        "kind": "Deployment",
        "metadata": {
          "name": dep_name.clone(),
          "labels": {
            "app": dep_name.clone()
          }
        },
        "spec": {
          "replicas": config["replicas"],
          "selector": {
            "matchLabels": {
              "app":dep_name.clone()
            }
          },
          "template": {
            "metadata": {
              "labels": {
                "app": dep_name.clone()
              }
            },
            "spec": {
              "containers": [
                {
                  "name": dep_name.clone(),
                  "image": config["container.image"],
                  "ports": [
                    {
                      "containerPort": 80,
                    }
                  ],
                  "env": env_object
                }
              ]
            }
          }
        }
    });
    return dep_val;
}

pub async fn create_deployment(
    client: &reqwest::Client,
    service: String,
    dep_name: String,
    variant: Value,
    namespace: String,
) -> () {
    let variant_id: String = serde_json::from_value(variant["id"].clone()).expect("Wq");
    let variant_use = variant_id.clone();

    let resp = client
        .get(format!(
            "http://localhost:8080/config/resolve?namespace={namespace}&variantIds={variant_use}"
        ))
        .header("x-org-id", "localorg")
        .header("x-tenant", service.clone())
        .send()
        .await
        .expect("ew");
    let config_resp = resp.json::<serde_json::Value>().await.expect("sas");
    println!("{}", config_resp);

    let dep_config = generate_deployment_config(config_resp.clone(), dep_name.clone());

    let url = format!(
        "{}/apis/apps/v1/namespaces/{namespace}/deployments",
        K8S_API_SERVER
    );

    // Send PUT request
    let response = client
        .post(&url)
        .bearer_auth(TOKEN)
        .json(&dep_config)
        .send()
        .await
        .expect("error");
    if response.status().is_success() {
        println!("deployment created successfully!");
    }
    println!(
        "{}",
        response.json::<serde_json::Value>().await.expect("sas")
    );
}
