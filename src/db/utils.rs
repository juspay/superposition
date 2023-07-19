use actix::{Actor, Addr, SyncContext};
use diesel::{
    r2d2::{ConnectionManager, Pool},
    PgConnection,
};
use jsonschema::JSONSchema;
use urlencoding::encode;

use crate::v1::{aws::kms, helpers::get_from_env_unsafe};

//TODOP separate out AppState from DB
pub struct AppState {
    pub db: Addr<DbActor>,
    pub db_pool: Pool<ConnectionManager<PgConnection>>,
    pub default_config_validation_schema: JSONSchema,
    pub admin_token: String,
}

pub struct DbActor(pub Pool<ConnectionManager<PgConnection>>);

impl Actor for DbActor {
    type Context = SyncContext<Self>;
}

pub async fn get_pool() -> Pool<ConnectionManager<PgConnection>> {
    let db_url = get_database_url().await;
    let manager: ConnectionManager<PgConnection> =
        ConnectionManager::<PgConnection>::new(db_url);
    Pool::builder()
        .build(manager)
        .expect("Error building a connection pool")
}

async fn get_database_url() -> String {
    let db_user: String = get_from_env_unsafe("DB_USER").unwrap();
    let kms_client = kms::new_client();
    let db_password_raw = kms::decrypt(kms_client, "DB_PASSWORD").await;
    let db_password = encode(db_password_raw.as_str()).to_string();
    let db_host: String = get_from_env_unsafe("DB_HOST").unwrap();
    let db_name: String = get_from_env_unsafe("DB_NAME").unwrap();
    let options_search_path = String::from("?options=--search_path%3d");
    let db_schema = match (
        get_from_env_unsafe::<String>("DB_SCHEMA"),
        get_from_env_unsafe::<String>("APP_ENV"),
    ) {
        (Ok(schema_name), _) => format!("{options_search_path}{schema_name}"),
        (Err(_), Ok(env)) if env.as_str() == "PROD" => {
            format!("{options_search_path}cac_v1")
        }
        _ => String::from(""),
    };
    format!("postgres://{db_user}:{db_password}@{db_host}/{db_name}{db_schema}")
}
