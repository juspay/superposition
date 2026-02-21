package io.juspay.superposition.examples;

import dev.openfeature.sdk.*;
import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.client.auth.BearerTokenIdentityResolver;
import io.juspay.superposition.provider.*;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;

import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * Polling Example
 *
 * Demonstrates the polling refresh strategy with LocalResolutionProvider
 * using the OpenFeature client interface.
 *
 * This example connects to a Superposition server via HTTP, polls for config
 * changes every 10 seconds, and prints a config value in a loop using the
 * standard OpenFeature client API. Change the config on the server and watch
 * the printed value update automatically.
 *
 * Usage:
 *   ./gradlew :openfeature-provider:run -PmainClass=io.juspay.superposition.examples.PollingExample
 *
 * Environment variables (all optional, with defaults shown):
 *   SUPERPOSITION_ENDPOINT  http://localhost:8080
 *   SUPERPOSITION_TOKEN     token
 *   SUPERPOSITION_ORG_ID    localorg
 *   SUPERPOSITION_WORKSPACE dev
 *   POLL_INTERVAL           10        (seconds between server polls)
 *   PRINT_INTERVAL          5         (seconds between printing the value)
 *   CONFIG_KEY              max_connections   (the config key to watch)
 */
public class PollingExample {

    public static void main(String[] args) throws Exception {
        // Get configuration from environment variables
        String endpoint = System.getenv().getOrDefault("SUPERPOSITION_ENDPOINT", "http://localhost:8080");
        String token = System.getenv().getOrDefault("SUPERPOSITION_TOKEN", "token");
        String orgId = System.getenv().getOrDefault("SUPERPOSITION_ORG_ID", "localorg");
        String workspaceId = System.getenv().getOrDefault("SUPERPOSITION_WORKSPACE", "dev");
        long pollInterval = Long.parseLong(System.getenv().getOrDefault("POLL_INTERVAL", "10"));
        long printInterval = Long.parseLong(System.getenv().getOrDefault("PRINT_INTERVAL", "5"));
        String configKey = System.getenv().getOrDefault("CONFIG_KEY", "max_connections");

        System.out.println("=== Superposition Polling Example ===");
        System.out.println("Endpoint:        " + endpoint);
        System.out.println("Org / Workspace: " + orgId + " / " + workspaceId);
        System.out.println("Poll interval:   " + pollInterval + "s");
        System.out.println("Print interval:  " + printInterval + "s");
        System.out.println("Watching key:    " + configKey);
        System.out.println();

        // Create HTTP data source
        var sdkClient = SuperpositionAsyncClient.builder()
            .endpointResolver(EndpointResolver.staticEndpoint(endpoint))
            .addIdentityResolver(new BearerTokenIdentityResolver(token))
            .build();

        HttpDataSource httpSource = new HttpDataSource(sdkClient, orgId, workspaceId);

        // Create provider with polling strategy
        LocalResolutionProvider provider = new LocalResolutionProvider(
            httpSource,
            Optional.empty(), // no fallback
            new RefreshStrategy.Polling(pollInterval),
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

            System.out.println("Provider ready. Printing config every " + printInterval + "s (Ctrl-C to stop).\n");

            EvaluationContext context = new ImmutableContext(Map.of());
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm:ss");

            // Print config value in a loop
            for (int i = 0; i < 20; i++) {
                String timestamp = LocalTime.now().format(formatter);

                try {
                    int value = client.getIntegerValue(configKey, 100, context);
                    System.out.println("[" + timestamp + "] " + configKey + " = " + value);
                } catch (Exception e) {
                    System.err.println("[" + timestamp + "] Error resolving " + configKey + ": " + e.getMessage());
                }

                TimeUnit.SECONDS.sleep(printInterval);
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
