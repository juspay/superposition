use actix_web::{
    get,
    web::{scope, Data, Json},
    App, HttpResponse, HttpServer,
};
use reqwest::Client;
use serde_json::json;

pub mod deployment;
pub mod experiment;
pub mod ingress;
pub mod service;
pub mod utils;

use crate::utils::{get_namespace, AppState};

use experiment::{experiment_concluded, experiment_inprogess, experiment_started};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    let app_state = Data::new(AppState {
        namespaces: ["mumbai".to_string(), "hyderabad".to_string()].to_vec(),
        tenants: ["nginxservice".to_string()].to_vec(),
    });

    HttpServer::new(move || {
        App::new()
            .app_data(app_state.clone())
            .service(scope("/hi").service(webhook_receiver))
    })
    .bind(("127.0.0.1", 8090))?
    .run()
    .await
}

#[get("")]
async fn webhook_receiver(req: Json<serde_json::Value>) -> HttpResponse {
    let client = Client::builder()
        .danger_accept_invalid_certs(true) // ⚠️ Disables TLS verification
        .build()
        .expect("reqwest client fails");
    let context = req["payload"]["context"].clone();

    let namespace = get_namespace(context);

    let deployment_type = req["event_info"]["webhook_event"].clone();

    if deployment_type == "ExperimentInprogress" {
        experiment_inprogess(req.clone(), &client).await;
    } else if deployment_type == "ExperimentStarted" {
        experiment_started(req.clone(), &client).await;
    } else if deployment_type == "ExperimentConcluded" {
        experiment_concluded(req.clone(), &client, namespace).await;
    }

    return HttpResponse::Ok().json(json!({"message": "deployment done!"}));
}
