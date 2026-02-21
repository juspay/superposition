package io.juspay.superposition.examples;

import dev.openfeature.sdk.*;
import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.client.auth.BearerTokenIdentityResolver;
import io.juspay.superposition.provider.*;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;

import java.util.Map;
import java.util.Optional;

/**
 * Local HTTP Example
 *
 * Demonstrates using LocalResolutionProvider with HttpDataSource
 * to fetch configuration from a Superposition server.
 *
 * Usage:
 *   ./gradlew :openfeature-provider:run -PmainClass=io.juspay.superposition.examples.LocalHttpExample
 */
public class LocalHttpExample {

    public static void main(String[] args) throws Exception {
        System.out.println("=== Superposition Local HTTP Example ===\n");

        // Get configuration from environment variables
        String endpoint = System.getenv().getOrDefault("SUPERPOSITION_ENDPOINT", "http://localhost:8080");
        String token = System.getenv().getOrDefault("SUPERPOSITION_TOKEN", "token");
        String orgId = System.getenv().getOrDefault("SUPERPOSITION_ORG_ID", "localorg");
        String workspaceId = System.getenv().getOrDefault("SUPERPOSITION_WORKSPACE", "dev");

        // Create HTTP data source
        var sdkClient = SuperpositionAsyncClient.builder()
            .endpointResolver(EndpointResolver.staticEndpoint(endpoint))
            .addIdentityResolver(new BearerTokenIdentityResolver(token))
            .build();

        HttpDataSource httpSource = new HttpDataSource(sdkClient, orgId, workspaceId);

        // Create provider with manual refresh strategy
        LocalResolutionProvider provider = new LocalResolutionProvider(
            httpSource,
            Optional.empty(), // no fallback
            new RefreshStrategy.Manual(),
            Optional.empty()
        );

        try {
            // Initialize the provider
            System.out.println("Initializing provider...");
            EvaluationContext initContext = new ImmutableContext(Map.of());
            provider.initialize(initContext);
            System.out.println("Provider initialized successfully!\n");

            // Create evaluation context
            EvaluationContext context = new ImmutableContext(Map.of(
                "userId", new Value("1234"),
                "region", new Value("us-east-1")
            ));

            // Resolve all features
            System.out.println("Resolving all features...");
            Map<String, Object> allConfig = provider.resolveAllFeatures(context).get();
            System.out.println("All config: " + allConfig);
            System.out.println();

            // Get applicable variants for experiments
            System.out.println("Getting applicable variants...");
            var variants = provider.getApplicableVariants(context).get();
            System.out.println("Variants: " + variants);
            System.out.println();

            // Demonstrate single flag resolution
            System.out.println("Resolving specific flags:");
            var boolResult = provider.getBooleanEvaluation("feature_enabled", false, context);
            System.out.println("  feature_enabled: " + boolResult.getValue());

            var stringResult = provider.getStringEvaluation("theme", "default", context);
            System.out.println("  theme: " + stringResult.getValue());

            var intResult = provider.getIntegerEvaluation("timeout", 30, context);
            System.out.println("  timeout: " + intResult.getValue());

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        } finally {
            // Cleanup
            System.out.println("\nClosing provider...");
            provider.shutdown();
            System.out.println("Done!");
        }
    }
}
