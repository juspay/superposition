# Context Aware Config Client Integration
----

This provides SDK to interact with ```context-aware-config```. 

- [Context Aware Config Client Integration](#context-aware-config-client-integration)
  - [Rust](#rust)
    - [Client Factory Methods Reference](#client-factory-methods-reference)
      - [Create Client](#create-client)
        - [Function definition](#function-definition)
        - [Params](#params)
      - [Get Client](#get-client)
        - [Function definition](#function-definition-1)
        - [Params](#params-1)
      - [Example Implementation](#example-implementation)
    - [CAC Client Methods Reference](#cac-client-methods-reference)
      - [Run polling for updates from Superposition Service](#run-polling-for-updates-from-superposition-service)
        - [Function definition](#function-definition-2)
      - [Get Config](#get-config)
        - [Funtion Definition](#funtion-definition)
      - [Get the last modified Time](#get-the-last-modified-time)
        - [Function Definition](#function-definition-3)
      - [Evaluate Context to derive configs](#evaluate-context-to-derive-configs)
        - [Function Definition](#function-definition-4)
        - [Params](#params-2)
      - [Get Default Config](#get-default-config)
        - [Function Definition](#function-definition-5)
        - [Param](#param)
  - [Haskell](#haskell)
    - [Adding the clients to your project](#adding-the-clients-to-your-project)
      - [Nix](#nix)
    - [Haskell CAC client functions reference](#haskell-cac-client-functions-reference)
      - [Create a client](#create-a-client)
        - [Function Definition](#function-definition-6)
        - [Param](#param-1)
      - [Get a client](#get-a-client)
        - [Function Definition](#function-definition-7)
        - [Param](#param-2)
      - [Run polling for updates from Superposition Service](#run-polling-for-updates-from-superposition-service-1)
        - [Function definition](#function-definition-8)
        - [Param](#param-3)
      - [Get Config](#get-config-1)
        - [Funtion Definition](#funtion-definition-1)
      - [Get the last modified Time](#get-the-last-modified-time-1)
        - [Function Definition](#function-definition-9)
      - [Evaluate Context to derive configs](#evaluate-context-to-derive-configs-1)
        - [Function Definition](#function-definition-10)
        - [Params](#params-3)
      - [Get Default Config](#get-default-config-1)
        - [Function Definition](#function-definition-11)
      - [Sample Integration](#sample-integration)

---
## Rust

The rust client have a client factory that helps you work with multiple clients connected to different tenants

### Client Factory Methods Reference

#### Create Client

Create a client in the factory. You can chose to use the result to check for errors faced by the Client Factory while creating your client, it is not mandatory to consume the `Ok` value.

##### Function definition
```
pub async fn create_client(
        tenant: String,
        polling_interval: Duration,
        hostname: String,
    ) -> Result<Arc<Client>, String>
```
##### Params
| Param              | type     | description                                                                                                          | Example value                     |
| ------------------ | -------- | -------------------------------------------------------------------------------------------------------------------- | --------------------------------- |
| `tenant`           | String   | specifies the tenants configs and contexts that will be loaded into the client at `polling_interval` from `hostname` | mjos                              |
| `polling_interval` | Duration | specifies the time cac client waits before checking with the server for updates                                      | Duration::from_secs(5)            |
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

Below is the rust implementation to instantiate CAC client using the client factory.

```rust
use cac_client as cc;

let tenants: Vec<String> = ["dev", "test"];
//You can create a clientFactory
for tenant in tenants {
    cc::CLIENT_FACTORY
        .create_client(
            tenant.to_string(),
            update_cac_periodically,//flag for if you want to update cac config periodically
            polling_interval,//polling interval in secs, default is 60
            cac_hostname.to_string(),// superposition service host
        )
        .await
        .expect(format!("{}: Failed to acquire cac_client", tenant).as_str());
};
//You can extract an individual tenant's client from clientFactory
let tenant = "dev".to_owned();
let cac_client = cc::CLIENT_FACTORY.get_client(tenant.clone()).map_err(|e| {
        log::error!("{}: {}", tenant.clone(), e);
        ErrorType::IgnoreError(format!("{}: Failed to get cac client", tenant))
    })?;
```

### CAC Client Methods Reference

After calling `get_client` method of Client Factory, you can do the following with the `Client` returned.

#### Run polling for updates from Superposition Service

the CAC client polls for updates from the superposition service and loads any changes done on the server. This means that configs changed in superposition are reflected on the client in the duration of `polling_interval`. `run_polling_updates()` should be run in a separate thread, as it does not terminate.

##### Function definition

 ```
 pub async fn run_polling_updates()
 ``` 

#### Get Config

Get the full config definition of your tenants configuration from superposition. `Config` has the following information:

```
pub struct Config {
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    default_configs: Map<String, Value>,
}
```

##### Funtion Definition

```
pub fn get_full_config_state_with_filter(query_data: Option<Map<String, Value>>) -> Result<Config, String>
``` 

#### Get the last modified Time

CAC client lets you get the last modified time of your configs, in case you want to log it, etc.

##### Function Definition

```
pub fn get_last_modified() -> Result<DateTime<Utc>, String>
``` 

#### Evaluate Context to derive configs

Given a context, get overrides for a specific set of keys, if provided. If None is provided for `filter_keys`, all configs are returned.

##### Function Definition

```
pub fn get_resolved_config(context: Map<String, Value>, filter_keys: Option<Vec<String>>) -> Result<Map<String, Value>, String>
``` 
##### Params

| Param         | type                | description                                                                           | Example value                             |
| ---------     | ------------------  | ------------------------------------------------------------------------------------- | ----------------------------------------- |
| `context`     | Map<String, Value>  | The context under which you want to resolve configs                                   | `{"os": "android", "merchant": "juspay"}` |
| `filter_keys` | Option<Vec<String>> | The keys for which you want the values. If empty, all configuration keys are returned | `Some([payment, network, color])`         |

#### Get Default Config

The default config for a specific set of keys, if provided. If None is provided for `filter_keys`, all configs are returned.

##### Function Definition

```
pub fn get_default_config(filter_keys: Option<Vec<String>>) -> Result<Map<String, Value>, String>
```
##### Param
| Param         | type                | description                                                                           | Example value                     |
| ------        | -----------         | ------------------------------------------------------------------------------------- | ---------------------------       |
| `filter_keys` | Option<Vec<String>> | The keys for which you want the values. If None, all configuration keys are returned | `Some([payment, network, color])` |

---

## Haskell

### Adding the clients to your project

#### Nix

Add the following to your inputs

```
crane.url = "github:ipetkov/crane/54b63c8eae4c50172cb50b612946ff1d2bc1c75c";
crane.inputs.nixpkgs.follows = "common/nixpkgs";
superposition = {
    url = "github:juspay/superposition";
    inputs.nixpkgs.follows = "common/nixpkgs";
    inputs.crane.follows = "crane";
};
```

then, add the following to your imports section in outputs:

```
imports = [
    ......
    inputs.superposition.haskellFlakeProjectModules.output
]
```

then add the libraries to your project.cabal file:

```
extra-libraries:
    cac_client
    experimentation_client
```
### Haskell CAC client functions reference

#### Create a client

Create a new client in the Client Factory

##### Function Definition

```
createCacClient:: Tenant -> Interval -> Hostname -> IO (Either Error ())
```

##### Param

| Param      | type     | description                                                                                                  | Example value                     |
| ---------- | -------- | ------------------------------------------------------------------------------------------------------------ | --------------------------------- |
| `Tenant`   | String   | specifies the tenants configs and contexts that will be loaded into the client at `Interval` from `Hostname` | mjos                              |
| `Interval` | Duration | specifies the time cac client waits before checking with the server for updates, in seconds                  | 10                                |
| `Hostname` | String   | The URL of the superposition server                                                                          | https://superposition.example.com |

#### Get a client

Create a new client in the Client Factory

##### Function Definition

```
getCacClient :: Tenant -> IO (Either Error (ForeignPtr CacClient))
```

##### Param

| Param    | type   | description                                                                                                  | Example value |
| -------- | ------ | ------------------------------------------------------------------------------------------------------------ | ------------- |
| `Tenant` | String | specifies the tenants configs and contexts that will be loaded into the client at `Interval` from `Hostname` | mjos          |


#### Run polling for updates from Superposition Service

the CAC client polls for updates from the superposition service and loads any changes done on the server. This means that configs changed in superposition are reflected on the client in the duration of `Interval`. `cacStartPolling` should be run in a separate thread, as it does not terminate.

##### Function definition

 ```
cacStartPolling :: Tenant -> IO ()
 ``` 

##### Param

| Param    | type   | description                                                                                                  | Example value |
| -------- | ------ | ------------------------------------------------------------------------------------------------------------ | ------------- |
| `Tenant` | String | specifies the tenants configs and contexts that will be loaded into the client at `Interval` from `Hostname` | mjos          | 

#### Get Config

Get the full config definition of your tenants configuration from superposition. `Config` has the following information:

```
{
    contexts: [Context],
    overrides: Map String Value,
    default_configs: Map String Value,
}
```

##### Funtion Definition

```
getFullConfigStateWithFilter :: ForeignPtr CacClient -> Maybe String -> IO (Either Error Value)
``` 

#### Get the last modified Time

CAC client lets you get the last modified time of your configs, in case you want to log it, etc.

##### Function Definition

```
getCacLastModified :: ForeignPtr CacClient -> IO (Either Error String)
``` 

#### Evaluate Context to derive configs

Given a context, get overrides for a specific set of keys, if provided. If Nothing is provided for `filter_keys`, all configs are returned.

##### Function Definition

```
getResolvedConfig :: ForeignPtr CacClient -> String -> Maybe [String] -> IO (Either Error Value)
``` 
##### Params

| Param             | type               | description                                                                           | Example value                             |
| ---------         | ------------------ | ------------------------------------------------------------------------------------- | ----------------------------------------- |
| `context`         | String             | The context under which you want to resolve configs                                   | `{"os": "android", "merchant": "juspay"}` |
| `filter_keys`     | Maybe([String])    | The keys for which you want the values. If empty, all configuration keys are returned | `Just ([payment, network, color])`        |

#### Get Default Config

The default config for a specific set of keys, if provided. If Nothing is provided for `filter_keys`, all configs are returned.

##### Function Definition

```
getDefaultConfig :: ForeignPtr CacClient -> Maybe [String] -> IO (Either Error Value)
```
| Param         | type              | description                                                                               | Example value                     |
| ------        | -----------       | -------------------------------------------------------------------------------------     | ---------------------------       |
| `filter_keys` | Maybe([String])   | The keys for which you want the values. If Nothing, all configuration keys are returned   | `Just ([payment, network, color])`|

#### Sample Integration

```
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Client             (getResolvedConfig, createCacClient, getCacClient,
                                     getFullConfigStateWithFilter, getCacLastModified, cacStartPolling, getDefaultConfig)
import           Control.Concurrent
import           Prelude

main :: IO ()
main = do
    createCacClient "dev" 10 "http://localhost:8080" >>= \case
        Left err -> putStrLn err
        Right _  -> pure ()
    threadId <- forkOS (cacStartPolling "dev")
    print threadId
    getCacClient "dev" >>= \case
        Left err     -> putStrLn err
        Right client -> do
            config          <- getFullConfigStateWithFilter client Nothing
            lastModified    <- getCacLastModified client
            overrides       <- getResolvedConfig client "{\"country\": \"India\"}" $ Just ["country_image_url", "hyperpay_version"]
            defaults        <- getDefaultConfig client $ Just ["country_image_url", "hyperpay_version"]
            filteredConfig  <- getFullConfigStateWithFilter client $ Just "{\"prefix\": \"hyperpay\"}"
            print config
            print lastModified
            print overrides
            print defaults
            print filteredConfig
            threadDelay 1000000000
    pure ()

```