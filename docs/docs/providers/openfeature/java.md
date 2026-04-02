---
sidebar_position: 5
title: Java / Kotlin
---

# Java / Kotlin — Superposition OpenFeature Provider

The Java provider supports feature flag evaluation, experimentation, and context-aware configuration. It works with both Java and Kotlin, and includes an Android-compatible HTTP transport.

- **Maven Central:** [`io.juspay.superposition:openfeature-provider`](https://central.sonatype.com/artifact/io.juspay.superposition/openfeature-provider)

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

```java
import dev.openfeature.sdk.*;
import io.juspay.superposition.openfeature.*;
import io.juspay.superposition.openfeature.options.RefreshStrategy;

import java.util.List;
import java.util.Map;

public class Example {
    public static void main(String[] args) {
        // 1. Configure experimentation options
        SuperpositionProviderOptions.ExperimentationOptions expOptions =
            SuperpositionProviderOptions.ExperimentationOptions.builder()
                .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000))
                .build();

        // 2. Configure provider options
        SuperpositionProviderOptions options =
            SuperpositionProviderOptions.builder()
                .orgId("localorg")
                .workspaceId("test")
                .endpoint("http://localhost:8080")
                .token("your-api-token")
                .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
                .experimentationOptions(expOptions)
                .build();

        // 3. Create and initialize the provider
        SuperpositionOpenFeatureProvider provider =
            new SuperpositionOpenFeatureProvider(options);
        EvaluationContext initCtx =
            new ImmutableContext(Map.of("city", new Value("Berlin")));
        provider.initialize(initCtx);

        // 4. Register with OpenFeature
        OpenFeatureAPI api = OpenFeatureAPI.getInstance();
        api.setProvider(provider);
        Client client = api.getClient();

        // 5. Define evaluation context
        EvaluationContext ctx =
            new ImmutableContext("user-123", Map.of(
                "city", new Value("Berlin"),
                "os", new Value("android")
            ));

        // 6. Evaluate feature flags
        boolean darkMode = client.getBooleanValue("dark_mode", false, ctx);
        System.out.println("dark_mode = " + darkMode);

        String currency = client.getStringValue("currency", "USD", ctx);
        System.out.println("currency = " + currency);

        Integer price = client.getIntegerValue("price", 0, ctx);
        System.out.println("price = " + price);

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
import io.juspay.superposition.openfeature.options.RefreshStrategy

fun main() {
    // 1. Configure options
    val expOptions = SuperpositionProviderOptions.ExperimentationOptions.builder()
        .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000))
        .build()

    val options = SuperpositionProviderOptions.builder()
        .orgId("localorg")
        .workspaceId("test")
        .endpoint("http://localhost:8080")
        .token("your-api-token")
        .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
        .experimentationOptions(expOptions)
        .build()

    // 2. Create and initialize provider
    val provider = SuperpositionOpenFeatureProvider(options)
    val initCtx = ImmutableContext(mapOf("city" to Value("Berlin")))
    provider.initialize(initCtx)

    // 3. Register and create client
    OpenFeatureAPI.getInstance().setProvider(provider)
    val client = OpenFeatureAPI.getInstance().client

    // 4. Evaluate feature flags
    val ctx = ImmutableContext(
        "user-123",
        mapOf("city" to Value("Berlin"), "os" to Value("android"))
    )

    val currency = client.getStringValue("currency", "USD", ctx)
    println("currency = $currency")

    val darkMode = client.getBooleanValue("dark_mode", false, ctx)
    println("dark_mode = $darkMode")

    val price = client.getIntegerValue("price", 0, ctx)
    println("price = $price")

    // 5. Get full config
    val fullConfig = provider.evaluateConfig(ctx)
    println("Full config: $fullConfig")
}
```

## Configuration Options

### `SuperpositionProviderOptions`

Uses Lombok `@Builder` — construct using the builder pattern.

| Field                    | Type                       | Required | Annotation      | Description                                |
| ------------------------ | -------------------------- | -------- | --------------- | ------------------------------------------ |
| `endpoint`               | `String`                   | Yes      | `@NonNull`      | Superposition server URL                   |
| `token`                  | `String`                   | Yes      | `@NonNull`      | Authentication token (bearer)              |
| `orgId`                  | `String`                   | Yes      | `@NonNull`      | Organisation ID                            |
| `workspaceId`            | `String`                   | Yes      | `@NonNull`      | Workspace ID                               |
| `refreshStrategy`        | `RefreshStrategy`          | Yes      | `@NonNull`      | How configs are refreshed                  |
| `fallbackConfig`         | `SuperpositionConfig`      | No       |                 | Fallback config for when server is down    |
| `evaluationCacheOptions` | `EvaluationCacheOptions`   | No       |                 | Evaluation result cache settings           |
| `experimentationOptions` | `ExperimentationOptions`   | No       |                 | A/B testing settings                       |

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

```java
SuperpositionProviderOptions.ExperimentationOptions.builder()
    .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000))
    .evaluationCacheOptions(EvaluationCacheOptions.builder()
        .ttl(300)
        .size(1000)
        .build())
    .build()
```

| Field                    | Type                     | Required | Description                            |
| ------------------------ | ------------------------ | -------- | -------------------------------------- |
| `refreshStrategy`        | `RefreshStrategy`        | Yes      | How experiment data is refreshed       |
| `evaluationCacheOptions` | `EvaluationCacheOptions` | No       | Cache for experiment evaluations       |

### `EvaluationCacheOptions`

| Field  | Type  | Default | Description                     |
| ------ | ----- | ------- | ------------------------------- |
| `ttl`  | `int` | —       | Cache time-to-live in seconds   |
| `size` | `int` | —       | Maximum number of cache entries |

### `SuperpositionConfig` (Fallback)

Provides default configuration when the server is unreachable:

```java
SuperpositionConfig fallback = SuperpositionConfig.builder()
    .contexts(contexts)
    .defaultConfigs(defaults)
    .overrides(overrides)
    .build();

SuperpositionProviderOptions.builder()
    // ...
    .fallbackConfig(fallback)
    .build();
```

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

## Provider-Specific Features

Beyond the standard OpenFeature methods, the Java provider offers:

### `evaluateConfig` — Full Config Resolution

Returns the entire resolved configuration as a `Map<String, Object>`:

```java
Map<String, Object> fullConfig = provider.evaluateConfig(ctx);
```

### `getApplicableVariants` — Experiment Variants

Returns a list of experiment variant IDs applicable to the given context:

```java
List<String> variants = provider.getApplicableVariants(ctx);
```

## Supported Value Types

| Method             | Return Type | Description             |
| ------------------ | ----------- | ----------------------- |
| `getBooleanValue`  | `Boolean`   | Boolean flag evaluation |
| `getStringValue`   | `String`    | String flag evaluation  |
| `getIntegerValue`  | `Integer`   | Integer flag evaluation |
| `getDoubleValue`   | `Double`    | Double flag evaluation  |
| `getObjectValue`   | `Value`     | Object/JSON evaluation  |

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
