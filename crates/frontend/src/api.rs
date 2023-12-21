use crate::pages::ExperimentList::types::{DefaultConfig, Dimension};

// #[derive(Debug, Serialize, Deserialize, Clone)]
// pub struct Dimension {
//     pub dimension: String,
//     pub priority: i32,
//     pub created_at: DateTime<Utc>,
//     pub created_by: String,
//     pub schema: Value,
// }

// #[derive(Serialize, Deserialize, Clone, Debug)]
// pub struct DefaultConfig {
//     pub key: String,
//     pub value: Value,
//     pub created_at: DateTime<Utc>,
//     pub created_by: String,
//     pub schema: Value,
// }

pub async fn fetch_dimensions(tenant: String) -> Result<Vec<Dimension>, String> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/dimension");
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let dimensions = response.json().await.map_err(|e| e.to_string())?;
            Ok(dimensions)
        }
        Err(e) => Err(e.to_string()),
    }
}

pub async fn fetch_default_config(tenant: String) -> Result<Vec<DefaultConfig>, String> {
    let client = reqwest::Client::new();
    let host = "http://localhost:8080";
    let url = format!("{host}/default-config");
    match client.get(url).header("x-tenant", tenant).send().await {
        Ok(response) => {
            let default_config = response.json().await.map_err(|e| e.to_string())?;
            Ok(default_config)
        }
        Err(e) => Err(e.to_string()),
    }
}

//pub fn dimension_resource(
//  tenant: ReadSignal<String>,
//) -> Resource<String, Vec<Dimension>> {
//  create_blocking_resource(
//    move || tenant.get(),
//  |tenant| async {
//    match fetch_dimensions(tenant).await {
//      Ok(data) => data,
//    Err(_) => vec![],
// }
// },
// )
//}

//pub fn default_config_resource(
//  tenant: ReadSignal<String>,
//) -> Resource<String, Vec<DefaultConfig>> {
//  create_blocking_resource(
//    move || tenant.get(),
//  |tenant| async {
//    match fetch_default_config(tenant).await {
//      Ok(data) => data,
//    Err(_) => vec![],
// }
// },
//)
//}
