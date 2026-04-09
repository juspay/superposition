---
sidebar_position: 6
title: Haskell
---

# Haskell — Superposition OpenFeature Provider

The Haskell provider is the native implementation of the Superposition OpenFeature provider. It offers two provider variants:

- **`LocalResolutionProvider`** — Fetches config from a data source (HTTP server or local file), caches it locally, and evaluates flags in-process. Supports polling, on-demand, file-watch, and manual refresh strategies. This is the **recommended provider** for most use cases.
- **`SuperpositionAPIProvider`** — A stateless remote provider that makes an HTTP API call to the Superposition server on every evaluation. No local caching — useful for serverless or low-traffic scenarios.

**Hackage:** [`superposition-open-feature-provider`](https://hackage.haskell.org/package/superposition-open-feature-provider)

## Installation

Add the following to your `.cabal` file:

```cabal
build-depends:
    base,
    superposition-open-feature-provider,
    open-feature,
    text,
    network-uri
```

## Quick Start

This is the most common usage — the provider connects to a Superposition server via HTTP, polls for config updates, and evaluates flags locally.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.OpenFeature (EvaluationContext, OpenFeature)
import Data.OpenFeature.SuperpositionProvider
    ( HttpDataSource(..), LocalResolutionProvider(..)
    , PollingStrategy(..), RefreshStrategy(..), SuperpositionOptions(..)
    )
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    -- 1. Create an HTTP data source pointing to your Superposition server
    let httpSource = HttpDataSource $ SuperpositionOptions
            { endpoint    = "http://localhost:8080"
            , token       = "your_token_here"
            , orgId       = "localorg"
            , workspaceId = "test"
            }

    -- 2. Create the provider with a polling refresh strategy
    let provider = LocalResolutionProvider
            { dataSource       = httpSource
            , fallbackSource   = Nothing  -- no fallback data source
            , refreshStrategy  = Polling $ PollingStrategy
                { interval = 60       -- seconds between polls
                , timeout  = Just 30  -- HTTP request timeout in seconds
                }
            }

    -- 3. Register with OpenFeature and create a client
    api <- openFeatureSingleton
    setProvider api provider
    let client = createClient api

    -- Allow time for the provider to initialize
    threadDelay 2000000

    -- 4. Evaluate feature flags
    let context = defaultEvaluationContext
            & withTargetingKey "user-42"
            & withCustomField "city" "Berlin"

    stringVal <- getStringValue client "currency" (Just context) Nothing
    putStrLn $ "currency = " <> stringVal

    intVal <- getIntValue client "price" (Just context) Nothing
    putStrLn $ "price = " <> show intVal

    boolVal <- getBoolValue client "dark_mode" (Just context) Nothing
    putStrLn $ "dark_mode = " <> show boolVal
```

## Configuration Options

### `SuperpositionOptions`

Connection options shared by `HttpDataSource` and `SuperpositionAPIProvider`:

| Field          | Type     | Required | Description                    |
| -------------- | -------- | -------- | ------------------------------ |
| `endpoint`     | `Text`   | Yes      | Superposition server URL       |
| `token`        | `Text`   | Yes      | Authentication token (bearer)  |
| `orgId`        | `Text`   | Yes      | Organisation ID                |
| `workspaceId`  | `Text`   | Yes      | Workspace ID                   |

```haskell
let options = SuperpositionOptions
        { endpoint    = "http://localhost:8080"
        , token       = "your_token"
        , orgId       = "localorg"
        , workspaceId = "test"
        }
```

### Refresh Strategies

The `RefreshStrategy` type supports four variants:

```haskell
-- Polling — periodically fetches updates from the server
Polling $ PollingStrategy
    { interval = 60       -- seconds between polls (default: 60)
    , timeout  = Just 30  -- HTTP request timeout in seconds (default: 30)
    }

-- On-Demand — fetches on first access, then caches with a TTL
OnDemand $ OnDemandStrategy
    { ttl             = 300        -- cache TTL in seconds (default: 300)
    , useStaleOnError = Just True  -- serve stale data on fetch error (default: Just True)
    , timeout         = Just 30    -- HTTP timeout in seconds (default: Just 30)
    }

-- Watch — uses file-system notifications (for FileDataSource only)
Watch $ WatchStrategy
    { debounceMs = Just 500  -- debounce interval in milliseconds (default: Just 500)
    }

-- Manual — no automatic refresh; user triggers refresh
Manual
```

### `ExperimentationOptions`

| Field              | Type                             | Required | Description                         |
| ------------------ | -------------------------------- | -------- | ----------------------------------- |
| `refreshStrategy`  | `RefreshStrategy`                | Yes      | How experiment data is refreshed    |
| `evaluationCache`  | `Maybe EvaluationCacheOptions`   | No       | Cache for experiment evaluations    |
| `defaultToss`      | `Maybe Int`                      | No       | Default toss value for experiments  |

`ExperimentationOptions` supports a builder pattern:

```haskell
let expOptions = defaultExperimentationOptions
        (Polling $ PollingStrategy { interval = 5, timeout = Just 3 })
        & withEvaluationCache defaultEvaluationCacheOptions
        & withDefaultToss 50
```

### `EvaluationCacheOptions`

| Field  | Type          | Default    | Description                     |
| ------ | ------------- | ---------- | ------------------------------- |
| `ttl`  | `Maybe Int`   | `Just 60`  | Cache time-to-live in seconds   |
| `size` | `Maybe Int`   | `Just 500` | Maximum number of cache entries |

## Provider Variants

### 1. `LocalResolutionProvider` (Recommended)

Fetches config from a pluggable data source (HTTP or file), caches locally, and evaluates flags in-process. Supports all four refresh strategies. Accepts an optional fallback data source.

```haskell
import Data.OpenFeature (EvaluationContext(..))
import Data.OpenFeature.SuperpositionProvider
    ( HttpDataSource(..), LocalResolutionProvider(..)
    , AllFeatureProvider(..), FeatureExperimentMeta(..)
    , PollingStrategy(..), RefreshStrategy(..), SuperpositionOptions(..)
    )

let httpSource = HttpDataSource $ SuperpositionOptions
        { endpoint    = "http://localhost:8080"
        , token       = "token"
        , orgId       = "localorg"
        , workspaceId = "dev"
        }

let provider = LocalResolutionProvider
        { dataSource      = httpSource
        , fallbackSource  = Nothing  -- optional fallback data source
        , refreshStrategy = Polling $ PollingStrategy { interval = 30, timeout = Just 10 }
        }

-- Initialize the provider (fetches initial config)
initProvider provider defaultEvaluationContext

-- Resolve all features
let context = defaultEvaluationContext
        & withTargetingKey "user-1234"
        & withCustomField "dimension" "d2"

allConfig <- resolveAllFeatures provider context
putStrLn $ "All config: " <> show allConfig

-- Get applicable experiment variants
variants <- getApplicableVariants provider context Nothing
putStrLn $ "Variants: " <> show variants

-- Cleanup
closeProvider provider
```

**Key capabilities:**

- **Pluggable data sources** — use `HttpDataSource` for server-backed, `FileDataSource` for local TOML files, or implement the `SuperpositionDataSource` typeclass for custom sources
- **Optional fallback** — provide a secondary data source (e.g. a local file) that is used when the primary source fails
- **Manual refresh** — call `refresh provider` to trigger a config refresh on demand
- **Chainable** — `LocalResolutionProvider` itself implements `SuperpositionDataSource`, so it can be used as a data source for another provider

### 2. `SuperpositionAPIProvider` (Remote / Stateless)

A stateless provider that calls the Superposition server on every evaluation. No local caching — each flag evaluation makes an HTTP request. Best for serverless, low-traffic, or scenarios where you always want the latest config.

```haskell
import Data.OpenFeature (EvaluationContext, OpenFeature)
import Data.OpenFeature.SuperpositionProvider
    ( SuperpositionAPIProvider(..), SuperpositionOptions(..) )

let provider = SuperpositionAPIProvider $ SuperpositionOptions
        { endpoint    = "http://localhost:8080"
        , token       = "token"
        , orgId       = "localorg"
        , workspaceId = "dev"
        }

-- Use with OpenFeature
api <- openFeatureSingleton
setProvider api provider
let client = createClient api

let context = defaultEvaluationContext
        & withCustomField "city" "Berlin"

value <- getStringValue client "currency" (Just context) Nothing
putStrLn $ "currency = " <> value
```

## Local File Resolution (SuperTOML)

Resolve configs from a local `.toml` file without needing a server:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.OpenFeature (EvaluationContext)
import Data.OpenFeature.SuperpositionProvider
    ( FileDataSource(..), LocalResolutionProvider(..)
    , AllFeatureProvider(..)
    , OnDemandStrategy(..), RefreshStrategy(..)
    )

main :: IO ()
main = do
    let fileSource = FileDataSource "config.toml"

    let provider = LocalResolutionProvider
            { dataSource      = fileSource
            , fallbackSource  = Nothing
            , refreshStrategy = OnDemand $ OnDemandStrategy
                { ttl             = 60
                , useStaleOnError = Nothing
                , timeout         = Nothing
                }
            }

    initProvider provider defaultEvaluationContext

    let context = defaultEvaluationContext
            & withCustomField "os" "linux"
            & withCustomField "city" "Boston"

    config <- resolveAllFeatures provider context
    putStrLn $ "Config: " <> show config

    closeProvider provider
```

:::note
The `FileDataSource` supports the `Watch` refresh strategy for automatic reloading on file changes. It does **not** support experimentation.
:::

## Local HTTP with File Fallback

Use the server as the primary data source with a local TOML file as fallback. If the HTTP source fails during initialization, the provider loads config from the fallback file instead — ensuring your application always starts with a valid configuration.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.OpenFeature (EvaluationContext, OpenFeature)
import Data.OpenFeature.SuperpositionProvider
    ( FileDataSource(..), HttpDataSource(..), LocalResolutionProvider(..)
    , PollingStrategy(..), RefreshStrategy(..), SuperpositionOptions(..)
    )
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    -- Primary: HTTP data source (Superposition server)
    let httpSource = HttpDataSource $ SuperpositionOptions
            { endpoint    = "http://localhost:8080"
            , token       = "token"
            , orgId       = "localorg"
            , workspaceId = "dev"
            }

    -- Fallback: local TOML config file
    let fileSource = FileDataSource "config.toml"

    let provider = LocalResolutionProvider
            { dataSource      = httpSource
            , fallbackSource  = Just fileSource  -- used when HTTP source is unavailable
            , refreshStrategy = Polling $ PollingStrategy
                { interval = 10
                , timeout  = Just 10
                }
            }

    -- Register with OpenFeature
    api <- openFeatureSingleton
    setProvider api provider
    let client = createClient api

    threadDelay 2000000

    let context = defaultEvaluationContext
            & withTargetingKey "user-456"
            & withCustomField "os" "linux"
            & withCustomField "city" "Berlin"

    currency <- getStringValue client "currency" (Just context) Nothing
    putStrLn $ "currency = " <> currency

    price <- getIntValue client "price" (Just context) Nothing
    putStrLn $ "price = " <> show price
```

:::tip
The fallback is only consulted during initialization or when the primary source fails. Once the primary source succeeds, the provider uses its data exclusively. Polling continues to try the primary source on each interval.
:::

## Evaluation Context

Pass dimensions and a targeting key for experiment bucketing:

```haskell
import Data.OpenFeature (EvaluationContext)

let context = defaultEvaluationContext
        & withTargetingKey "user-42"
        & withCustomField "city" "Berlin"
        & withCustomField "os" "android"

value <- getStringValue client "currency" (Just context) Nothing
```

## Supported Value Types

| Method            | Return Type    |
| ----------------- | -------------- |
| `getBoolValue`    | `Bool`         |
| `getIntValue`     | `Int`          |
| `getFloatValue`   | `Double`       |
| `getStringValue`  | `Text`         |
| `getStructValue`  | `Value`        |

The `LocalResolutionProvider` and `SuperpositionAPIProvider` also implement the `AllFeatureProvider` typeclass:

```haskell
-- Resolve all features
allConfig <- resolveAllFeatures provider context

-- Resolve with a prefix filter (e.g. only keys starting with "payment.")
filtered <- resolveAllFeaturesWithFilter provider context (Just ["payment."])
```

And `FeatureExperimentMeta` for experiment metadata:

```haskell
-- Get applicable experiment variant IDs
variants <- getApplicableVariants provider context Nothing  -- optional prefix filter
```

## Logging

The provider uses the `monad-logger` framework. Enable with `runStdoutLoggingT`:

```haskell
import Control.Monad.Logger (runStdoutLoggingT, LogLevel(..))

runStdoutLoggingT $ do
    -- provider operations here
```

## Examples

- [`clients/haskell/open-feature-provider`](https://github.com/juspay/superposition/tree/main/clients/haskell/open-feature-provider) — Provider source and usage
