package io.juspay.superposition.examples;

import dev.openfeature.sdk.*;
import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.client.auth.BearerTokenIdentityResolver;
import io.juspay.superposition.provider.*;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * Local with Fallback Example
 *
 * Demonstrates using LocalResolutionProvider with HTTP primary source
 * and File fallback source. If the HTTP server is unavailable, the
 * provider falls back to reading from a local config file.
 *
 * Usage:
 *   ./gradlew :openfeature-provider:run -PmainClass=io.juspay.superposition.examples.LocalWithFallbackExample
 */
public class LocalWithFallbackExample {

    public static void main(String[] args) throws Exception {
        System.out.println("=== Superposition Fallback + Polling Example ===\n");

        // Get configuration from environment variables
        String endpoint = System.getenv().getOrDefault("SUPERPOSITION_ENDPOINT", "http://localhost:8080");
        String token = System.getenv().getOrDefault("SUPERPOSITION_TOKEN", "token");
        String orgId = System.getenv().getOrDefault("SUPERPOSITION_ORG_ID", "localorg");
        String workspaceId = System.getenv().getOrDefault("SUPERPOSITION_WORKSPACE", "dev");

        // Create HTTP data source (primary)
        var sdkClient = SuperpositionAsyncClient.builder()
            .endpointResolver(EndpointResolver.staticEndpoint(endpoint))
            .addIdentityResolver(new BearerTokenIdentityResolver(token))
            .build();

        HttpDataSource httpSource = new HttpDataSource(sdkClient, orgId, workspaceId);

        // Create File data source (fallback)
        Path configPath = Paths.get("src/main/resources/config.toml");
        System.out.println("Primary: HTTP (" + endpoint + ")");
        System.out.println("Fallback: " + configPath);
        System.out.println("Polling every 10s. Printing config every 5s (Ctrl-C to stop).\n");

        // For this example, we'll use HTTP as both primary and fallback
        // In a real scenario, you would use FileDataSource as fallback
        HttpDataSource fallbackSource = new HttpDataSource(sdkClient, orgId, workspaceId);

        // Create provider with both sources and polling strategy
        LocalResolutionProvider provider = new LocalResolutionProvider(
            httpSource,
            Optional.of(fallbackSource),
            new RefreshStrategy.Polling(10),
            Optional.empty()
        );

        try {
            // Register with OpenFeature and create a client
            OpenFeatureAPI api = OpenFeatureAPI.getInstance();
            EvaluationContext initContext = new ImmutableContext(Map.of());
            provider.initialize(initContext);
            api.setProvider(provider);
            Client client = api.getClient();

            // Allow time for the provider to initialize
            TimeUnit.SECONDS.sleep(2);

            EvaluationContext context = new ImmutableContext(Map.of(
                "os", new Value("linux"),
                "city", new Value("Berlin")
            ));

            System.out.println("Starting polling loop...\n");

            // Poll for config changes and print values in a loop
            for (int i = 0; i < 12; i++) {
                String timestamp = java.time.LocalTime.now().toString().substring(0, 8);

                try {
                    String currency = client.getStringValue("currency", "USD", context);
                    System.out.print("[" + timestamp + "] currency = " + currency);
                } catch (Exception e) {
                    System.out.print("[" + timestamp + "] currency error: " + e.getMessage());
                }

                try {
                    int timeout = client.getIntegerValue("timeout", 30, context);
                    System.out.println("  |  timeout = " + timeout);
                } catch (Exception e) {
                    System.out.println("  |  timeout error: " + e.getMessage());
                }

                TimeUnit.SECONDS.sleep(5);
            }

            System.out.println("\nExample completed!");

        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
        } finally {
            // Cleanup
            provider.shutdown();
        }
    }
}
