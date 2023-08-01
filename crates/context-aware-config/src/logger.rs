use env_logger::{Builder, Env};
use serde::{ser::SerializeStruct, Serialize, Serializer};
use serde_json::{json, Value};
use std::io::Write;

//TODO make Log type more strict and have custom implementation of
//serialize trait
pub struct Log {
    value: Value,
    timestamp: String,
    level: String,
}

//NOTE we can't use '-' in a struct identifier name therefore writing custom serializer
impl Serialize for Log {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let no_of_fields = 4;
        let mut log = serializer.serialize_struct("Log", no_of_fields)?;
        log.serialize_field("value", &self.value)?;
        log.serialize_field("service", "context-aware-config")?;
        log.serialize_field("timestamp", &self.timestamp)?;
        log.serialize_field("level", &self.level)?;
        log.end()
    }
}

pub fn init_logger() {
    let env = Env::default();
    Builder::from_env(env)
        .format(move |buf, record| {
            let log = Log {
                timestamp: buf.timestamp_millis().to_string(),
                value: json!(record.args()),
                level: record.level().to_string(),
            };

            writeln!(buf, "{}", serde_json::json!(log))
        })
        .target(env_logger::Target::Stdout)
        .init();
}
