# Experimentation Client Integration

This provides SDK to interact with ```experimentation-platform```.We support superposition_client for
 1. Rust
 2. Haskell


## Rust

### Implementation
Below is the rust implementation to instantiate Experimentation client .

```rust
use superpostion_client as sp;

let tenants: Vec<String> = ["abc", "wer"];
//You can create a clientFactory
for tenant in tenants {
        rt::spawn(
            sp::CLIENT_FACTORY
                .create_client(tenant.to_string(),
                                poll_frequency,//How frequently you want to update config in secs
                                hostname.to_string()// superposition hostname
                            )
                .await
                .expect(format!("{}: Failed to acquire superposition_client", tenant).as_str())
                .clone()
                .run_polling_updates(),
        );
};
//You can extract an individual tenant's client from clientFactory
let tenant = "abc".to_owned();
let sp_client = sp::CLIENT_FACTORY
        .get_client(tenant.clone())
        .await
        .map_err(|e| {
            log::error!("{}: {}", tenant, e);
            ErrorType::IgnoreError(format!("{}: Failed to get superposition_client", tenant))
        })?;


```

### Methods Overview

1. ```pub async fn run_polling_updates(self: Arc<Self>) {}``` -> You can set the interval for updating config in the client.
2. ```pub async fn get_applicable_variant(&self, context: &Value, toss: i8) -> Vec<String> {}``` -> Provides variantIds for which the given context holds true.
3. ```pub async fn get_satisfied_experiments(&self, context: &Value) -> Experiments {}``` -> Lists all the experiments for the given context.
4. ``` pub async fn get_running_experiments(&self) -> Experiments {}``` -> This lists only the inprogress experiments.


## Haskell