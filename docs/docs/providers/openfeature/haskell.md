---
sidebar_position: 6
title: Haskell
---

# Haskell - Superposition OpenFeature Provider

The Haskell package currently exposes one OpenFeature provider:
`SuperpositionProvider`.

It fetches configuration from the Superposition HTTP API, stores it in a native
provider cache through `superposition-bindings`, and evaluates OpenFeature
values from that local cache.

The Haskell provider supports:

- OpenFeature evaluation for boolean, string, integer, double, and object values
- Full resolved config lookup through `resolveAllConfig`
- Polling and on-demand refresh for configuration
- Optional polling or on-demand refresh for experimentation metadata
- Provider cleanup through `closeSuperpositionProvider`

It does not currently expose the newer data-source provider API used by some
other language providers. In particular, there is no Haskell
`LocalResolutionProvider`, `SuperpositionAPIProvider`, `HttpDataSource`,
`FileDataSource`, fallback data source, file-watch refresh, or manual refresh
API in the current code.

**Package source:** [`clients/haskell/open-feature-provider`](https://github.com/juspay/superposition/tree/main/clients/haskell/open-feature-provider)

## Installation

Add the provider and OpenFeature packages to your `.cabal` file:

```cabal
build-depends:
    base,
    superposition-open-feature-provider,
    open-feature,
    network-uri
```

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (Value (String))
import Data.OpenFeature.EvaluationContext qualified as Context
import Data.OpenFeature.FeatureProvider qualified as OF
import Data.OpenFeature.SuperpositionProvider qualified as SP
import Network.URI qualified as URI

expectRight :: Show e => Either e a -> IO a
expectRight (Right value) = pure value
expectRight (Left err) = fail (show err)

main :: IO ()
main = do
  endpoint <-
    maybe (fail "Invalid Superposition endpoint") pure $
      URI.parseURI "http://localhost:8080"

  provider <-
    SP.newSuperpositionProvider
      SP.defaultProviderOptions
        { SP.orgId = "localorg",
          SP.workspaceId = "dev",
          SP.endpoint = endpoint,
          SP.token = "your_token_here",
          SP.refreshOptions = SP.Poll 60,
          SP.experimentationRefreshOptions = Nothing,
          SP.logLevel = SP.LevelInfo
        }
      >>= expectRight

  let context =
        Context.withCustomField "city" (String "Berlin") $
          Context.withTargetingKey "user-42" Context.defaultContext

  OF.initialize provider context

  resolvedConfig <- SP.resolveAllConfig provider context
  case resolvedConfig of
    Right json -> putStrLn json
    Left err -> fail err

  boolResult <- OF.resolveBooleanValue provider "dark_mode" context
  case boolResult of
    Right details -> print (OF.value details)
    Left err -> print err

  SP.closeSuperpositionProvider provider
```

## Provider Options

Create a provider with `newSuperpositionProvider` and
`SuperpositionProviderOptions`. The easiest path is to start from
`defaultProviderOptions` and override the fields for your workspace.

| Field | Type | Required | Description |
| ----- | ---- | -------- | ----------- |
| `orgId` | `Text` | Yes | Organisation ID |
| `workspaceId` | `Text` | Yes | Workspace ID |
| `endpoint` | `Network.URI.URI` | Yes | Superposition server URL |
| `token` | `Text` | Yes | Authentication bearer token |
| `refreshOptions` | `RefreshOptions` | Yes | Config refresh strategy |
| `experimentationRefreshOptions` | `Maybe RefreshOptions` | No | Enables experiment and experiment-group refresh when set |
| `logLevel` | `LogLevel` | No | Minimum log level emitted by the provider |
| `fallbackConfig` | `()` | No | Placeholder field only; fallback config is not implemented today |

```haskell
let options =
      SP.defaultProviderOptions
        { SP.orgId = "localorg",
          SP.workspaceId = "dev",
          SP.endpoint = endpoint,
          SP.token = "your_token_here",
          SP.refreshOptions = SP.Poll 60,
          SP.logLevel = SP.LevelInfo
        }
```

## Refresh Options

The current Haskell API has two refresh modes:

| Constructor | Description |
| ----------- | ----------- |
| `Poll Int` | Starts a background refresh task. The integer is the polling interval in seconds. |
| `OnDemand Int64` | Fetches lazily and caches the value for the given TTL in seconds. If a refresh fails, the previous cached value is reused when available. |

```haskell
-- Poll every 60 seconds
SP.Poll 60

-- Fetch on demand and cache for 300 seconds
SP.OnDemand 300
```

## Experimentation

Experimentation refresh is enabled by setting `experimentationRefreshOptions`.
When enabled, the provider fetches active experiments and experiment groups from
the server and initializes the native experiment cache.

```haskell
let options =
      SP.defaultProviderOptions
        { SP.orgId = "localorg",
          SP.workspaceId = "dev",
          SP.endpoint = endpoint,
          SP.token = "your_token_here",
          SP.refreshOptions = SP.Poll 60,
          SP.experimentationRefreshOptions = Just (SP.Poll 60)
        }
```

Experimentation uses the OpenFeature targeting key for bucketing. If
experimentation refresh is configured and the evaluation context has no
targeting key, the provider logs a warning and evaluates without experiment
bucketing.

The Haskell provider does not currently expose `ExperimentationOptions`,
evaluation-cache options, default toss configuration, or a
`getApplicableVariants` helper.

## Resolving Values

The provider implements the OpenFeature `FeatureProvider` instance methods:

| Method | Value type |
| ------ | ---------- |
| `resolveBooleanValue` | `Bool` |
| `resolveStringValue` | `Text` |
| `resolveIntegerValue` | `Integer` |
| `resolveDoubleValue` | `Double` |
| `resolveObjectValue` | Object / JSON-like value |

For the full resolved configuration, use `resolveAllConfig`:

```haskell
resolvedConfig <- SP.resolveAllConfig provider context
case resolvedConfig of
  Right json -> putStrLn json
  Left err -> fail err
```

## Logging

Set `logLevel` on `SuperpositionProviderOptions` to control provider logs:

```haskell
let options =
      SP.defaultProviderOptions
        { SP.logLevel = SP.LevelDebug
        }
```

## Current Limitations

The following provider features are not implemented in Haskell yet:

- Local file resolution from SuperTOML or JSON
- Pluggable data sources
- HTTP primary plus local-file fallback
- Stateless remote provider API
- File-watch refresh
- Manual refresh
- Prefix-filtered all-feature resolution
- Direct applicable-variant lookup
