# Superposition OpenFeature Provider

This project provides an [OpenFeature](https://openfeature.dev/) provider 
implementation for [Superposition](https://juspay.in/superposition).
It enables you to use Superposition as a feature flag and experimentation backend in any 
application that supports OpenFeature.

## Features
- Seamless integration with Superposition for feature flag evaluation.
- Supports all OpenFeature flag types (boolean, string, number, object).
- Experimentation support.
- Customizable refresh strategies and caching.

## Getting Started
To use this provider, add the following repositories and dependencies to your Gradle build configuration:

```kotlin
repositories {
    mavenCentral()
    maven(url = "https://sandbox.assets.juspay.in/m2")
    // ...other repositories
}

dependencies {
    implementation("io.juspay.superposition.openfeature:superposition-provider:0.0.1-dev")
    implementation("dev.openfeature:sdk:1.15.1")
    // ...other dependencies
}
```

## Example Usage
Below is a basic example of how to initialize the Superposition provider with experimentation 
options and use it with the OpenFeature SDK:

```java
import dev.openfeature.sdk.*;
import io.juspay.superposition.openfeature.*;
import io.juspay.superposition.openfeature.options.RefreshStrategy;

import java.util.List;
import java.util.Map;

public class Example {
    public static void main(String[] args) {
        // Configure experimentation options
        SuperpositionProviderOptions.ExperimentationOptions expOptions =
            SuperpositionProviderOptions.ExperimentationOptions.builder()
                .refreshStrategy(RefreshStrategy.Polling.of(5000, 2000)) // 5s timeout, 2s interval
                .build();

        // Configure provider options
        SuperpositionProviderOptions options =
            SuperpositionProviderOptions.builder()
                .orgId("your-org-id")
                .workspaceId("your-workspace-id")
                .endpoint("https://api.superposition.dev")
                .token("your-api-token")
                .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000)) // 10s timeout, 5s interval
                .experimentationOptions(expOptions)
                .build();

        // Initialize provider
        SuperpositionOpenFeatureProvider provider =
            new SuperpositionOpenFeatureProvider(options);
        EvaluationContext initCtx =
            new ImmutableContext(Map.of("foo", new Value("bar")));
        provider.initialize(initCtx);
        OpenFeatureAPI.getInstance().setProvider(provider);
        Client client = OpenFeatureAPI.getInstance().getClient();

        // Create evaluation context (optional)
        EvaluationContext ctx =
            new ImmutableContext(Map.of("userId", new Value("123")));

        // Evaluate a boolean flag
        boolean enabled = client.getBooleanValue("my-feature-flag", false, ctx);
        System.out.println("Feature enabled: " + enabled);


        // Evaluate & get the entire configuration
        System.out.println("Full config: " + provider.evaluateConfig(ctx));

        // Get the applicable variants for the ongoing experiments
        EvaluationContext experimentCtx = new ImmutableContext("your-targeting-key", Map.of("userId", new Value("123")));
        List<String> variants = provider.applicableVariants(experimentCtx);
        System.out.println("Variants: " + variants);
    }
}
```

## License
See LICENSE file for details.
