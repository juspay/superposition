---
sidebar_position: 5
title: Java / Kotlin
---

# Java / Kotlin - Superposition OpenFeature Provider

The Java package currently documents one OpenFeature provider:
`SuperpositionOpenFeatureProvider`.

It is configured with `SuperpositionProviderOptions`, fetches data from the
Superposition HTTP API, and integrates with the OpenFeature Java SDK.

The Java provider documentation in this repository supports:

- OpenFeature evaluation for boolean, string, number, and object values
- Full resolved config lookup through `evaluateConfig`
- Applicable experiment variant lookup through `applicableVariants`
- Polling refresh strategies for configuration and experimentation
- Optional experimentation configuration

The current Java package in this checkout does not include the newer
`LocalResolutionProvider`, `SuperpositionAPIProvider`, `HttpDataSource`, or
`FileDataSource` provider API shown by some other language implementations.

## Installation

The current provider README uses the Juspay sandbox Maven repository:

```kotlin
repositories {
    mavenCentral()
    maven(url = "https://sandbox.assets.juspay.in/m2")
}

dependencies {
    implementation("io.juspay.superposition.openfeature:superposition-provider:0.0.1-dev")
    implementation("dev.openfeature:sdk:1.15.1")
}
```

:::note
You need a running Superposition server. See [Quick Start](../../quick_start) for setup instructions.
:::

## Quick Start

```java
import dev.openfeature.sdk.*;
import io.juspay.superposition.openfeature.*;
import io.juspay.superposition.openfeature.options.RefreshStrategy;

import java.util.List;
import java.util.Map;

public class Example {
    public static void main(String[] args) {
        SuperpositionProviderOptions.ExperimentationOptions expOptions =
            SuperpositionProviderOptions.ExperimentationOptions.builder()
                .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000))
                .build();

        SuperpositionProviderOptions options =
            SuperpositionProviderOptions.builder()
                .orgId("your-org-id")
                .workspaceId("your-workspace-id")
                .endpoint("http://localhost:8080")
                .token("your-api-token")
                .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
                .experimentationOptions(expOptions)
                .build();

        SuperpositionOpenFeatureProvider provider =
            new SuperpositionOpenFeatureProvider(options);

        EvaluationContext initCtx =
            new ImmutableContext(Map.of("city", new Value("Berlin")));

        provider.initialize(initCtx);

        OpenFeatureAPI.getInstance().setProvider(provider);
        Client client = OpenFeatureAPI.getInstance().getClient();

        EvaluationContext ctx =
            new ImmutableContext(
                "user-42",
                Map.of(
                    "city", new Value("Berlin"),
                    "os", new Value("android")
                )
            );

        boolean darkMode = client.getBooleanValue("dark_mode", false, ctx);
        System.out.println("dark_mode = " + darkMode);

        String currency = client.getStringValue("currency", "USD", ctx);
        System.out.println("currency = " + currency);

        System.out.println("Full config: " + provider.evaluateConfig(ctx));

        List<String> variants = provider.applicableVariants(ctx);
        System.out.println("Variants: " + variants);
    }
}
```

## Provider Options

Create a provider with `SuperpositionProviderOptions.builder()`.

| Field | Required | Description |
| ----- | -------- | ----------- |
| `orgId` | Yes | Organisation ID |
| `workspaceId` | Yes | Workspace ID |
| `endpoint` | Yes | Superposition server URL |
| `token` | Yes | Authentication token |
| `refreshStrategy` | Yes | Configuration refresh strategy |
| `experimentationOptions` | No | Enables experiment refresh and variant resolution |

```java
SuperpositionProviderOptions options =
    SuperpositionProviderOptions.builder()
        .orgId("your-org-id")
        .workspaceId("your-workspace-id")
        .endpoint("http://localhost:8080")
        .token("your-api-token")
        .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
        .build();
```

## Experimentation

Enable experimentation by adding nested experimentation options:

```java
SuperpositionProviderOptions.ExperimentationOptions expOptions =
    SuperpositionProviderOptions.ExperimentationOptions.builder()
        .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000))
        .build();

SuperpositionProviderOptions options =
    SuperpositionProviderOptions.builder()
        .orgId("your-org-id")
        .workspaceId("your-workspace-id")
        .endpoint("http://localhost:8080")
        .token("your-api-token")
        .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
        .experimentationOptions(expOptions)
        .build();
```

Use an OpenFeature targeting key when evaluating experiment-backed config:

```java
EvaluationContext experimentCtx =
    new ImmutableContext(
        "user-42",
        Map.of("city", new Value("Berlin"))
    );

List<String> variants = provider.applicableVariants(experimentCtx);
```

## Evaluation Context

Custom fields on the OpenFeature evaluation context map to Superposition
dimensions.

```java
EvaluationContext ctx =
    new ImmutableContext(
        "user-42",
        Map.of(
            "city", new Value("Berlin"),
            "os", new Value("android")
        )
    );

String currency = client.getStringValue("currency", "USD", ctx);
```

## Provider-Specific Methods

The Java provider README documents these provider-specific methods:

| Method | Description |
| ------ | ----------- |
| `evaluateConfig(ctx)` | Resolve and return the full configuration for a context |
| `applicableVariants(ctx)` | Return applicable experiment variant IDs for a context |

## Current Limitations

The following provider features are not present in the checked-in Java provider
documentation or source layout today:

- `LocalResolutionProvider`
- `SuperpositionAPIProvider`
- `HttpDataSource` and `FileDataSource`
- Local SuperTOML or JSON file resolution
- Pluggable data sources
- Fallback data sources
- Watch refresh
- Manual refresh
