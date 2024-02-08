# Context Aware Configuration Client Integration

This provides SDK to interact with ```context-aware-config```.We support cac_client for
 1. Rust
 2. Haskell


## Rust

### Implementation
Below is the rust implementation to instantiate CAC client .

```rust
use cac_client as cc;

let tenants: Vec<String> = ["abc", "wer"];
//You can create a clientFactory
for tenant in tenants {
    cc::CLIENT_FACTORY
        .create_client(
            tenant.to_string(),
            update_cac_periodically,//flag for if you want to update cac config periodically
            polling_interval,//polling interval in secs, default is 60
            cac_hostname.to_string(),//cac server host
        )
        .await
        .expect(format!("{}: Failed to acquire cac_client", tenant).as_str());
};
//You can extract an individual tenant's client from clientFactory
let tenant = "abc".to_owned();
let cac_client = cc::CLIENT_FACTORY.get_client(tenant.clone()).map_err(|e| {
        log::error!("{}: {}", tenant.clone(), e);
        ErrorType::IgnoreError(format!("{}: Failed to get cac client", tenant))
    })?;


```

### Methods Overview

1. ```pub async fn start_polling_update(self)``` -> This updates client's config at given interval.
1. ```pub fn get_config(&self) -> Result<Config, String> {}``` -> You can fetch the client's config.
2. ```pub fn get_last_modified<E>(&'static self) -> Result<DateTime<Utc>, String> {}``` -> This provides when the config last modified.
3. ```pub fn eval(&self, query_data: Map<String, Value>,) -> Result<Map<String, Value>, String> {} ``` 
-> This helps in evaluating the config based on the given context.


## Haskell


