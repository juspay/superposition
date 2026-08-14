---
sidebar_position: 2
title: Experimentation Client Integration
---

# Experimentation Client Integration

This provides SDK to interact with ```experimentation-platform```

  - [Rust](#rust)
    - [Client Factory Methods Reference](#client-factory-methods-reference)
      - [Create Client](#create-client)
        - [Function definition](#function-definition)
        - [Params](#params)
      - [Get Client](#get-client)
        - [Function definition](#function-definition-1)
        - [Params](#params-1)
      - [Example Implementation](#example-implementation)
    - [Experiment Client Methods Reference](#experiment-client-methods-reference)
      - [Run polling for updates from Superposition Service](#run-polling-for-updates-from-superposition-service)
        - [Function definition](#function-definition-2)
      - [Get an applicable variant](#get-an-applicable-variant)
        - [Function Definition](#function-definition-3)
        - [Params](#params-2)
      - [Get satisfied experiments](#get-satisfied-experiments)
        - [Function Definition](#function-definition-4)
        - [Params](#params-3)
      - [Get all running experiments](#get-all-running-experiments)
        - [Function Definition](#function-definition-5)
  - [Haskell](#haskell)
    - [Experiment Client Methods Reference](#experiment-client-methods-reference-1)
      - [Create Client](#create-client-1)
        - [Function definition](#function-definition-6)
        - [Params](#params-4)
      - [Get Client](#get-client-1)
        - [Function definition](#function-definition-7)
        - [Params](#params-5)
      - [Run polling for updates from Superposition Service](#run-polling-for-updates-from-superposition-service-1)
        - [Function definition](#function-definition-8)
      - [Get an applicable variant](#get-an-applicable-variant-1)
        - [Function Definition](#function-definition-9)
        - [Params](#params-6)
      - [Get satisfied experiments](#get-satisfied-experiments-1)
        - [Function Definition](#function-definition-10)
        - [Params](#params-7)
      - [Get all running experiments](#get-all-running-experiments-1)
        - [Function Definition](#function-definition-11)
      - [Sample Integration](#sample-integration)


## Rust

The rust client have a client factory that helps you work with multiple clients connected to different tenants

### Client Factory Methods Reference

#### Create Client

Create a client in the factory. You can chose to use the result to check for errors faced by the Client Factory while creating your client, it is not mandatory to consume the `Ok` value.

##### Function definition
```
pub async fn create_client(
        &self,
        tenant: String,
        poll_frequency: u64,
        hostname: String,
    ) -> Result<Arc<Client>, String>
```
##### Params
| Param              | type     | description                                                                                                          | Example value                     |
| ------------------ | -------- | -------------------------------------------------------------------------------------------------------------------- | --------------------------------- |
| `tenant`           | String   | specifies the tenant whose experiments will be loaded into the client from `hostname`                                | mjos                              |
| `poll_frequency`   | u64      | specifies the time experimentation client waits, in seconds, before checking with the server for updates             | 5                                 |
| `hostname`         | String   | The URL of the superposition server                                                                                  | https://superposition.example.com |

#### Get Client

Get a client 

##### Function definition
```
pub async fn get_client(
        tenant: String
    ) -> Result<Arc<Client>, String>
```
##### Params
| Param    | type   | description                                      | Example value |
| -------- | ------ | ------------------------------------------------ | ------------- |
| `tenant` | String | specifies the tenant used during `create_client` | mjos          |

#### Example Implementation

Below is the rust implementation to instantiate Experimentation client .

```rust
use experimentation_client as exp;

let tenants = ["dev", "test"];
for tenant in tenants {
    let client = exp::CLIENT_FACTORY
        .create_client(
            tenant.to_string(),
            10,
            hostname.to_string(),
        )
        .await
        .expect(format!("{}: Failed to acquire experimentation_client", tenant).as_str());
    tokio::spawn(client.clone().run_polling_updates());
};

let tenant = "dev".to_owned();
let sp_client = exp::CLIENT_FACTORY
        .get_client(tenant.clone())
        .await
        .map_err(|e| {
            log::error!("{}: {}", tenant, e);
            ErrorType::IgnoreError(format!("{}: Failed to get experimentation_client", tenant))
        })?;


```

### Experiment Client Methods Reference

#### Run polling for updates from Superposition Service

the Experimentation client polls for updates from the superposition service and loads any changes done on the server. This means that experiments changed in superposition are reflected on the client within the configured `poll_frequency`. `run_polling_updates()` should be run in a separate thread, as it does not terminate.

##### Function definition

 ```
 pub async fn run_polling_updates()
 ``` 

#### Get an applicable variant

When experiments are running, you can get different variants of the experiment based on an identifier. The client hashes the identifier with each experiment group id to select a bucket, and returns the matching variant IDs. You can then include these in your CAC client request.

The dimensions map is used to evaluate local cohorts before matching experiment contexts. The optional prefix filters config keys on satisfied experiments before variant selection.

##### Function Definition
```
pub async fn get_applicable_variant(
        &self,
        dimensions_info: &HashMap<String, DimensionInfo>,
        context: &Map<String, Value>,
        identifier: &str,
        prefix: Option<Vec<String>>,
    ) -> Result<Vec<String>, String>
```
##### Params

| Param             | type                              | description                                         | Example value                             |
| ---------         | -----                             | --------------------------------------------------- | ----------------------------------------- |
| `dimensions_info` | `HashMap<String, DimensionInfo>`  | Dimension metadata used for local cohort evaluation | `HashMap::new()`                          |
| `context`         | `Map<String, Value>`              | The context under which you want to resolve variants | `{"os": "android", "merchant": "juspay"}` |
| `identifier`      | `&str`                            | Stable identifier used for bucketing                | `"user-123"`                              |
| `prefix`          | `Option<Vec<String>>`             | Optional config-key prefix filter                   | `Some(vec!["payment".into()])`            |

#### Get satisfied experiments

Rather than just getting the variant ID, you can get the whole experiment(s) that are satisfying your context - rather than just the final result.

##### Function Definition
```
pub async fn get_satisfied_experiments(
        &self,
        context: &Map<String, Value>,
        prefix: Option<Vec<String>>,
    ) -> Result<Experiments, String>
```
##### Params

| Param     | type                  | description                                         | Example value                             |
| --------- | -----                 | --------------------------------------------------- | ----------------------------------------- |
| `context` | `Map<String, Value>`  | The context under which you want to resolve experiments | `{"os": "android", "merchant": "juspay"}` |
| `prefix`  | `Option<Vec<String>>` | Optional config-key prefix filter                   | `None`                                    |

#### Get filtered satisfied experiments

Evaluate experiments with unresolved local cohorts skipped, then return experiments that satisfy the context.

##### Function Definition
```
pub async fn get_filtered_satisfied_experiments(
        &self,
        context: &Map<String, Value>,
        prefix: Option<Vec<String>>,
    ) -> Result<Experiments, String>
```

#### Get all running experiments

Get all running experiments, why would you want to do this? We don't know. But you can.

##### Function Definition
```
pub async fn get_running_experiments(&self) -> Result<Experiments, String>
```

## Haskell

### Experiment Client Methods Reference

#### Create Client

Create a client in the factory. You can chose to use the result to check for errors faced by the Client Factory while creating your client.

##### Function definition
```
createExpClient:: Tenant -> Integer -> String -> IO (Either Error ())
```
##### Params
| Param              | type     | description                                                                                                          | Example value                     |
| ------------------ | -------- | -------------------------------------------------------------------------------------------------------------------- | --------------------------------- |
| `Tenant`           | String   | specifies the tenant whose experiments will be loaded into the client from `hostname`                                | mjos                              |
| `Interval` | Integer | specifies the time experimentation client waits, in seconds, before checking with the server for updates             | 5                                 |
| `Hostname`         | String   | The URL of the superposition server                                                                                  | https://superposition.example.com |

#### Get Client

Get a client 

##### Function definition
```
getExpClient :: Tenant -> IO (Either Error (ForeignPtr ExpClient))
```
##### Params
| Param    | type   | description                                      | Example value |
| -------- | ------ | ------------------------------------------------ | ------------- |
| `tenant` | String | specifies the tenant used during `create_client` | mjos          |

#### Run polling for updates from Superposition Service

the Experimentation client polls for updates from the superposition service and loads any changes done on the server. This means that experiments changed in superposition are reflected on the client in the duration of `Interval`. `expStartPolling` should be run in a separate thread, as it does not terminate.

##### Function definition

 ```
 expStartPolling :: Tenant -> IO ()
 ``` 

#### Get an applicable variant

When experiments are running, you can get different variants of the experiment based on a stable identifier. The dimensions JSON is used for local cohort evaluation, the context JSON is matched against experiments, and the optional prefix filters config keys.

The function returns a JSON string containing the matching variant IDs.

##### Function Definition
```
getApplicableVariants :: ForeignPtr ExpClient -> String -> String -> String -> Maybe String -> IO (Either Error String)
```
##### Params

| Param        | type           | description                                         | Example value                             |
| ---------    | -----          | --------------------------------------------------- | ----------------------------------------- |
| `dimensions` | String         | JSON object containing dimension metadata           | `{}`                                      |
| `context`    | String         | JSON object containing request context              | `{"os": "android", "merchant": "juspay"}` |
| `identifier` | String         | Stable identifier used for bucketing                | `"user-123"`                              |
| `prefix`     | Maybe String   | Optional comma-separated config-key prefix filter   | `Just "payment,network"`                  |

#### Get satisfied experiments

Rather than just getting the variant ID, you can get the whole experiment(s) that are satisfying your context - rather than just the final result.

##### Function Definition
```
getSatisfiedExperiments :: ForeignPtr ExpClient -> String -> String -> Maybe String -> IO (Either Error Value)
```
##### Params

| Param        | type         | description                                         | Example value                             |
| ---------    | -----        | --------------------------------------------------- | ----------------------------------------- |
| `dimensions` | String       | JSON object containing dimension metadata           | `{}`                                      |
| `context`    | String       | JSON object containing request context              | `{"os": "android", "merchant": "juspay"}` |
| `prefix`     | Maybe String | Optional comma-separated config-key prefix filter   | `Nothing`                                 |

#### Get filtered satisfied experiments

##### Function Definition
```
getFilteredSatisfiedExperiments :: ForeignPtr ExpClient -> String -> Maybe String -> Maybe String -> IO (Either Error Value)
```

#### Get all running experiments

Get all running experiments, why would you want to do this? We don't know. But you can.

##### Function Definition
```
getRunningExperiments :: ForeignPtr ExpClient -> IO (Either Error Value)
```

#### Sample Integration

```
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Client             (createExpClient, expStartPolling,
                                     getApplicableVariants, getExpClient,
                                     getRunningExperiments,
                                     getSatisfiedExperiments)
import           Control.Concurrent
import           Prelude

main :: IO ()
main = do
    createExpClient "dev" 10 "http://localhost:8080" >>= \case
        Left err -> putStrLn err
        Right _  -> pure ()
    threadId <- forkIO (expStartPolling "dev")
    print threadId
    getExpClient "dev" >>= \case
        Left err     -> putStrLn err
        Right client -> loop client
    pure ()
    where
        loop client = do
            runningExperiments   <- getRunningExperiments client
            satisfiedExperiments <- getSatisfiedExperiments client "{}" "{\"os\": \"android\", \"client\": \"1mg\"}" Nothing
            variants             <- getApplicableVariants client "{}" "{\"os\": \"android\", \"client\": \"1mg\"}" "1mg-android" Nothing
            print "Running experiments"
            print runningExperiments
            print "experiments that satisfy context"
            print satisfiedExperiments
            print "variant ID applied"
            print variants
            -- threadDelay 10000000
            loop client

```
