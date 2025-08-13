use serde_json::Value;

use crate::deployment::create_deployment;
use crate::ingress::{create_ingress, delete_ingress, update_ingress};
use crate::service::{create_service, delete_service, update_service};
use crate::utils::get_namespace;

pub async fn experiment_inprogess(req: Value, client: &reqwest::Client) {
    let traffic_percentage: i32 =
        serde_json::from_value(req["payload"]["traffic_percentage"].clone())
            .expect("invalid traffic value");

    let context = req["payload"]["context"].clone();

    let namespace = get_namespace(context);
    let service: String = serde_json::from_value(req["event_info"]["workspace_id"].clone())
        .expect("service deocee error");
    let variants: Vec<Value> =
        serde_json::from_value(req["payload"]["variants"].clone()).expect("expected aaray");

    for variant in variants {
        if variant["variant_type"].clone() != "CONTROL" {
            let variant_id: String = serde_json::from_value(variant["id"].clone()).expect("Wq");
            let service_name = service.clone() + "-service-" + &variant_id;
            let ingress_name = service.clone() + "-ingress-" + &variant_id;

            update_ingress(
                client,
                variant,
                traffic_percentage.to_string(),
                service.clone(),
                service_name,
                ingress_name,
                namespace.clone(),
                service.clone(),
            )
            .await;
        }
    }
}
pub async fn experiment_started(req: Value, client: &reqwest::Client) {
    let variants: Vec<Value> =
        serde_json::from_value(req["payload"]["variants"].clone()).expect("expected aaray");
    let traffic_percentage: i32 =
        serde_json::from_value(req["payload"]["traffic_percentage"].clone())
            .expect("invalid traffic value");
    let service: String = serde_json::from_value(req["event_info"]["workspace_id"].clone())
        .expect("service deocee error");
    let context = req["payload"]["context"].clone();

    let namespace = get_namespace(context);
    for variant in variants {
        if variant["variant_type"].clone() != "CONTROL" {
            let variant_id: String = serde_json::from_value(variant["id"].clone()).expect("Wq");
            let service_name = service.clone() + "-service-" + &variant_id;
            let deployment_name = service.clone() + "-dep-" + &variant_id;
            let ingress_name = service.clone() + "-ingress-" + &variant_id;

            create_deployment(client, service.clone(), deployment_name.clone(), variant.clone(), namespace.clone()).await;
            create_service( service_name.clone(), deployment_name.clone(), namespace.clone()).await;

            create_ingress(
                client,
                variant,
                traffic_percentage.to_string(),
                service.clone(),
                service_name.clone(),
                ingress_name,
                namespace.clone(),
                service.clone(),
            )
            .await;
        }
    }
}

pub async fn experiment_concluded(req: Value, client: &reqwest::Client, namespace: String) {
    let variants: Vec<Value> =
        serde_json::from_value(req["payload"]["variants"].clone()).expect("expected aaray");
    let service: String = serde_json::from_value(req["event_info"]["workspace_id"].clone())
        .expect("service deocee error");

    let chosen_variant: String = serde_json::from_value(req["payload"]["chosen_variant"].clone())
        .expect("error chosen variant");

    for variant in variants.clone() {
        if variant["variant_type"].clone() != "CONTROL" {
            let variant_id: String = serde_json::from_value(variant["id"].clone()).expect("Wq");
            if (variant["variant_type"].clone() != "CONTROL") && (variant_id == chosen_variant) {
                let deployment_name = service.clone() + "-dep-" + &variant_id;
                update_service(service.clone(), namespace.clone(), deployment_name.clone()).await;
            }
        }
    }

    for variant in variants {
        if variant["variant_type"].clone() != "CONTROL" {
            let variant_id: String = serde_json::from_value(variant["id"].clone()).expect("Wq");
            let service_name = service.clone() + "-service-" + &variant_id;
            let ingress_name = service.clone() + "-ingress-" + &variant_id;
            delete_ingress(ingress_name.clone(), namespace.clone(), client).await;

            delete_service(service_name.clone(), namespace.clone(), client).await;
        }
    }
}
