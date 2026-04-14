---
sidebar_position: 5
title: Java / Kotlin
---

# Java / Kotlin — Superposition OpenFeature Provider

The Java provider is an OpenFeature-compatible provider for Superposition that works with both Java and Kotlin. It offers two provider variants:

- **`LocalResolutionProvider`** — Fetches config from a data source (HTTP server), caches it locally, and evaluates flags in-process. Supports polling and on-demand refresh strategies. This is the **recommended provider** for most use cases.
- **`SuperpositionAPIProvider`** — A stateless remote provider that makes an HTTP API call to the Superposition server on every evaluation. No local caching — useful for serverless or low-traffic scenarios.

The provider includes an Android-compatible HTTP transport.

**Maven Central:** [`io.juspay.superposition:openfeature-provider`](https://central.sonatype.com/artifact/io.juspay.superposition/openfeature-provider)

## Installation

### Gradle (Kotlin DSL)

```kotlin
implementation("dev.openfeature:sdk:<openfeature-version>")
implementation("io.juspay.superposition:openfeature-provider:<superposition-version>")
```

### Gradle (Groovy)

```groovy
implementation "dev.openfeature:sdk:<openfeature-version>"
implementation "io.juspay.superposition:openfeature-provider:<superposition-version>"
```

### Maven

```xml
<dependency>
    <groupId>dev.openfeature</groupId>
    <artifactId>sdk</artifactId>
    <version>${openfeature.version}</version>
</dependency>
<dependency>
    <groupId>io.juspay.superposition</groupId>
    <artifactId>openfeature-provider</artifactId>
    <version>${superposition.version}</version>
</dependency>
```

:::note
You need a running Superposition server. See [Quick Start](../../quick_start) for setup instructions.
:::

## Quick Start (Java)

This is the most common usage — the provider connects to a Superposition server via HTTP, polls for config updates, and evaluates flags locally.

```java
import dev.openfeature.sdk.*;
import io.juspay.superposition.openfeature.*;
import io.juspay.superposition.openfeature.options.*;

import java.util.List;
import java.util.Map;

public class Example {
    public static void main(String[] args) {
        // 1. Create an HTTP data source pointing to your Superposition server
        HttpDataSource httpSource = new HttpDataSource(SuperpositionOptions.builder()
            .endpoint("http://localhost:8080")
            .token("your-api-token")
            .orgId("localorg")
            .workspaceId("test")
            .build());

        // 2. Configure experimentation options
        ExperimentationOptions expOptions = ExperimentationOptions.builder()
            .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000))
            .build();

        // 3. Create the provider with a polling refresh strategy
        LocalResolutionProvider provider = LocalResolutionProvider.builder()
            .dataSource(httpSource)
            .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
            .experimentationOptions(expOptions)
            .build();

        // 4. Initialize and register with OpenFeature
        EvaluationContext initCtx =
            new ImmutableContext(Map.of("city", new Value("Berlin")));
        provider.initialize(initCtx);

        OpenFeatureAPI api = OpenFeatureAPI.getInstance();
        api.setProvider(provider);
        Client client = api.getClient();

        // 5. Define evaluation context
        EvaluationContext ctx = new ImmutableContext(
            "user-42",
            Map.of(
                "city", new Value("Berlin"),
                "os", new Value("android")
            )
        );

        // 6. Evaluate feature flags
        String currency = client.getStringValue("currency", "USD", ctx);
        System.out.println("currency = " + currency);

        Integer price = client.getIntegerValue("price", 0, ctx);
        System.out.println("price = " + price);

        boolean darkMode = client.getBooleanValue("dark_mode", false, ctx);
        System.out.println("dark_mode = " + darkMode);

        // 7. Get full resolved config (provider-specific)
        Map<String, Object> fullConfig = provider.evaluateConfig(ctx);
        System.out.println("Full config: " + fullConfig);

        // 8. Get applicable experiment variants (provider-specific)
        List<String> variants = provider.getApplicableVariants(ctx);
        System.out.println("Applicable variants: " + variants);
    }
}
```

## Quick Start (Kotlin)

```kotlin
import dev.openfeature.sdk.*
import io.juspay.superposition.openfeature.*
import io.juspay.superposition.openfeature.options.*

fun main() {
    // 1. Create an HTTP data source
    val httpSource = HttpDataSource(SuperpositionOptions.builder()
        .endpoint("http://localhost:8080")
        .token("your-api-token")
        .orgId("localorg")
        .workspaceId("test")
        .build())

    // 2. Configure experimentation
    val expOptions = ExperimentationOptions.builder()
        .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000))
        .build()

    // 3. Create the provider
    val provider = LocalResolutionProvider.builder()
        .dataSource(httpSource)
        .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
        .experimentationOptions(expOptions)
        .build()

    // 4. Initialize and register
    val initCtx = ImmutableContext(mapOf("city" to Value("Berlin")))
    provider.initialize(initCtx)

    OpenFeatureAPI.getInstance().setProvider(provider)
    val client = OpenFeatureAPI.getInstance().client

    // 5. Evaluate feature flags
    val ctx = ImmutableContext(
        "user-42",
        mapOf("city" to Value("Berlin"), "os" to Value("android"))
    )

    val currency = client.getStringValue("currency", "USD", ctx)
    println("currency = $currency")

    val darkMode = client.getBooleanValue("dark_mode", false, ctx)
    println("dark_mode = $darkMode")

    val price = client.getIntegerValue("price", 0, ctx)
    println("price = $price")

    // 6. Get full config
    val fullConfig = provider.evaluateConfig(ctx)
    println("Full config: $fullConfig")
}
```

## Configuration Options

### `SuperpositionOptions`

Connection options shared by `HttpDataSource` and `SuperpositionAPIProvider`. Uses Lombok `@Builder`.

| Field         | Type     | Required | Annotation | Description                   |
| ------------- | -------- | -------- | ---------- | ----------------------------- |
| `endpoint`    | `String` | Yes      | `@NonNull` | Superposition server URL      |
| `token`       | `String` | Yes      | `@NonNull` | Authentication token (bearer) |
| `orgId`       | `String` | Yes      | `@NonNull` | Organisation ID               |
| `workspaceId` | `String` | Yes      | `@NonNull` | Workspace ID                  |

```java
SuperpositionOptions options = SuperpositionOptions.builder()
    .endpoint("http://localhost:8080")
    .token("your-api-token")
    .orgId("localorg")
    .workspaceId("test")
    .build();
```

### Refresh Strategies

The `RefreshStrategy` interface has two implementations:

```java
// Polling — periodic background refresh
RefreshStrategy.Polling.of(
    10000,  // interval in milliseconds
    5000    // timeout in milliseconds
)

// OnDemand — fetch on access, cache with TTL
RefreshStrategy.OnDemand.of(
    300000, // TTL in milliseconds
    5000    // timeout in milliseconds
)
```

### `ExperimentationOptions`

Uses Lombok `@Builder`:

| Field                    | Type                     | Required | Description                        |
| ------------------------ | ------------------------ | -------- | ---------------------------------- |
| `refreshStrategy`        | `RefreshStrategy`        | Yes      | How experiment data is refreshed   |
| `evaluationCacheOptions` | `EvaluationCacheOptions` | No       | Cache for experiment evaluations   |
| `defaultToss`            | `Integer`                | No       | Default toss value for experiments |

```java
ExperimentationOptions expOptions = ExperimentationOptions.builder()
    .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000))
    .evaluationCacheOptions(EvaluationCacheOptions.builder()
        .ttl(300)
        .size(1000)
        .build())
    .defaultToss(50)
    .build();
```

### `EvaluationCacheOptions`

| Field  | Type  | Default | Description                     |
| ------ | ----- | ------- | ------------------------------- |
| `ttl`  | `int` | `60`    | Cache time-to-live in seconds   |
| `size` | `int` | `500`   | Maximum number of cache entries |

## Provider Variants

### 1. `LocalResolutionProvider` (Recommended)

Fetches config from a pluggable data source (HTTP), caches locally, and evaluates flags in-process. Accepts an optional fallback data source.

```java
HttpDataSource httpSource = new HttpDataSource(SuperpositionOptions.builder()
    .endpoint("http://localhost:8080")
    .token("token")
    .orgId("localorg")
    .workspaceId("dev")
    .build());

LocalResolutionProvider provider = LocalResolutionProvider.builder()
    .dataSource(httpSource)
    .refreshStrategy(RefreshStrategy.Polling.of(30000, 10000))
    .build();

// Initialize
provider.initialize(new ImmutableContext(Map.of("city", new Value("Berlin"))));

// Resolve all config
EvaluationContext ctx = new ImmutableContext(
    "user-1234",
    Map.of("city", new Value("Berlin")));
Map<String, Object> allConfig = provider.evaluateConfig(ctx);

// Get applicable experiment variants
List<String> variants = provider.getApplicableVariants(ctx);
```

**Key capabilities:**

- **Pluggable data sources** — use `HttpDataSource` for server-backed resolution
- **Optional fallback** — provide a `SuperpositionConfig` that is used when the primary source fails
- **Full config resolution** — resolve all features at once via `evaluateConfig()`
- **Experiment metadata** — get applicable variants via `getApplicableVariants()`

### 2. `SuperpositionAPIProvider` (Remote / Stateless)

A stateless provider that calls the Superposition server on every evaluation. No local caching — each flag evaluation makes an HTTP request. Best for serverless, low-traffic, or scenarios where you always want the latest config.

```java
SuperpositionAPIProvider provider = new SuperpositionAPIProvider(
    SuperpositionOptions.builder()
        .endpoint("http://localhost:8080")
        .token("token")
        .orgId("localorg")
        .workspaceId("dev")
        .build()
);

OpenFeatureAPI api = OpenFeatureAPI.getInstance();
api.setProvider(provider);
Client client = api.getClient();

EvaluationContext ctx = new ImmutableContext(
    "user-42",
    Map.of("city", new Value("Berlin")));

String currency = client.getStringValue("currency", "USD", ctx);
System.out.println("currency = " + currency);
```

### Fallback Configuration

Provides default configuration when the server is unreachable:

```java
SuperpositionConfig fallback = SuperpositionConfig.builder()
    .contexts(contexts)
    .defaultConfigs(defaults)
    .overrides(overrides)
    .build();

LocalResolutionProvider provider = LocalResolutionProvider.builder()
    .dataSource(httpSource)
    .fallbackConfig(fallback)
    .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
    .build();
```

:::tip
The fallback is only consulted during initialization or when the primary source fails. Once the primary source succeeds, the provider uses its data exclusively.
:::

## Evaluation Context

```java
// Java — with targeting key for experiments
EvaluationContext ctx = new ImmutableContext(
    "user-42",                              // targeting key
    Map.of(
        "city", new Value("Berlin"),
        "os", new Value("android"),
        "customers", new Value("platinum")
    )
);
```

```kotlin
// Kotlin — equivalent
val ctx = ImmutableContext(
    "user-42",
    mapOf("city" to Value("Berlin"), "os" to Value("android"))
)
```

- **`targeting_key`** — Used for experiment variant bucketing. Typically a user ID or session ID.
- All other keys map to your Superposition dimensions.

## Supported Value Types

| Method             | Return Type | Description             |
| ------------------ | ----------- | ----------------------- |
| `getBooleanValue`  | `Boolean`   | Boolean flag evaluation |
| `getStringValue`   | `String`    | String flag evaluation  |
| `getIntegerValue`  | `Integer`   | Integer flag evaluation |
| `getDoubleValue`   | `Double`    | Double flag evaluation  |
| `getObjectValue`   | `Value`     | Object/JSON evaluation  |

The `LocalResolutionProvider` also supports provider-specific methods:

```java
// Resolve all features
Map<String, Object> fullConfig = provider.evaluateConfig(ctx);

// Get applicable experiment variant IDs
List<String> variants = provider.getApplicableVariants(ctx);
```

## Android Support

The Java provider includes `URLConnectionTransport`, an Android-compatible HTTP transport that uses `java.net.HttpURLConnection` instead of the default transport:

```java
import io.juspay.superposition.openfeature.transport.URLConnectionTransport;

// The transport is used internally — no explicit configuration needed
// It's automatically compatible with Android's networking restrictions
```

## Dependencies

The Java provider depends on:

- [`dev.openfeature:sdk`](https://central.sonatype.com/artifact/dev.openfeature/sdk) — OpenFeature Java SDK
- Smithy-generated Java SDK for Superposition API
- Native bindings via JNI for local config resolution
- Lombok for builder pattern generation

## Full Example

See the integration test: [`clients/java/provider-sdk-tests/src/main/kotlin/io/juspay/superposition/providertests/Main.kt`](https://github.com/juspay/superposition/blob/main/clients/java/provider-sdk-tests/src/main/kotlin/io/juspay/superposition/providertests/Main.kt)
