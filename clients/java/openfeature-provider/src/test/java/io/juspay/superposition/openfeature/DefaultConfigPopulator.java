package io.juspay.superposition.openfeature;

import io.juspay.superposition.client.SuperpositionClient;
import io.juspay.superposition.model.*;
import software.amazon.smithy.java.auth.api.AuthProperties;
import software.amazon.smithy.java.auth.api.identity.TokenIdentity;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResolver;
import software.amazon.smithy.java.client.core.auth.identity.IdentityResult;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;
import software.amazon.smithy.java.core.serde.document.Document;

import java.net.URI;
import java.net.http.HttpClient;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

import static software.amazon.smithy.java.auth.api.identity.TokenIdentity.create;

/**
 * Utility class to populate Superposition with default configuration data.
 *
 * This class uses the actual data from your local installation to populate default configs.
 *
 * Usage:
 * DefaultConfigPopulator populator = new DefaultConfigPopulator("http://localhost:8080", "test", "localorg");
 * populator.populateFromProvidedData();
 */
public class DefaultConfigPopulator {

    private final SuperpositionClient client;
    private final String workspaceId;
    private final String orgId;
    private final String baseUrl;

    public DefaultConfigPopulator(String baseUrl, String workspaceId, String orgId) {
        this(baseUrl, workspaceId, orgId, null);
    }

    public DefaultConfigPopulator(String baseUrl, String workspaceId, String orgId, String bearerToken) {
        this.baseUrl = baseUrl;
        this.workspaceId = workspaceId;
        this.orgId = orgId;

        // Build Superposition client
        SuperpositionClient.Builder clientBuilder = SuperpositionClient.builder()
            .endpointResolver(EndpointResolver.staticEndpoint(baseUrl))
            .addIdentityResolver(new IdentityResolverImpl(bearerToken));
        this.client = clientBuilder.build();
    }

    /**
     * Populates default configs using the provided data from your local installation.
     */
    public void populateFromProvidedData() throws Exception {
        System.out.println("Populating default configs from provided data...");

        // List existing configs to avoid duplicates
        ListDefaultConfigsOutput existingConfigs = listExistingConfigs();
        List<String> existingKeys = extractExistingKeys(existingConfigs);

        // Create configs from your local installation data
        List<ConfigData> localConfigs = getLocalInstallationConfigs();

        System.out.println("Found " + localConfigs.size() + " default configs to process from provided data");

        for (ConfigData config : localConfigs) {
            if (!existingKeys.contains(config.key)) {
                createDefaultConfig(
                    config.key,
                    config.value,
                    config.schema,
                    config.description,
                    config.changeReason,
                    config.functionName,
                    config.autocompleteFunctionName
                );
                System.out.println("Created default config from data: " + config.key + " (value: " + config.value + ")");
            } else {
                System.out.println("Skipping existing config: " + config.key);
            }
        }

        // create context
        client.createContext(CreateContextInput.builder().
            context(Map.of("and", Document.of(
                List.of(
                    Document.of(
                        Map.of("==", Document.of(
                            List.of(
                                Document.of(Map.of("var", Document.of("d1"))),
                                Document.of(true)
                            )
                            )
                        )
                    )
                )
            )
            )).
            override(Map.of("bool", Document.of(false))).
            description("description").
            changeReason("change").
            workspaceId(this.workspaceId).
            orgId(this.orgId).build()
        );

        System.out.println("Provided data population completed successfully!");
    }

    /**
     * Returns the actual default configs from your local installation
     */
    private List<ConfigData> getLocalInstallationConfigs() {
        List<ConfigData> configs = new ArrayList<>();

        // Config 1: list
        configs.add(new ConfigData(
            "list",
            Document.of(List.of(Document.of("k1"), Document.of("v1"))),
            Map.of("type", Document.of("array")),
            "list",
            "list",
            null,
            null
        ));

        // Config 2: object
        configs.add(new ConfigData(
            "object",
            Document.of(Map.of("k1", Document.of(Map.of("k2", Document.of("v2"))))),
            Map.of("type", Document.of("object")),
            "object",
            "object",
            null,
            null
        ));

        // Config 3: string
        configs.add(new ConfigData(
            "string",
            Document.of("something"),
            Map.of("type", Document.of("string")),
            "string",
            "string",
            null,
            null
        ));

        // Config 4: double
        configs.add(new ConfigData(
            "double",
            Document.of(1.2),
            Map.of("type", Document.of("number")),
            "double",
            "test",
            null,
            null
        ));

        // Config 5: integer
        configs.add(new ConfigData(
            "integer",
            Document.of(1),
            Map.of("type", Document.of("integer")),
            "integer",
            "test",
            null,
            null
        ));

        // Config 6: bool
        configs.add(new ConfigData(
            "bool",
            Document.of(true),
            Map.of("type", Document.of("boolean")),
            "bool",
            "test",
            null,
            null
        ));

        return configs;
    }

    private ListDefaultConfigsOutput listExistingConfigs() throws Exception {
        ListDefaultConfigsInput input = ListDefaultConfigsInput.builder()
            .workspaceId(workspaceId)
            .orgId(orgId)
            .all(true)
            .build();

        return client.listDefaultConfigs(input);
    }

    private List<String> extractExistingKeys(ListDefaultConfigsOutput existingConfigs) {
        List<String> keys = new ArrayList<>();
        ListIterator<DefaultConfigFull> keysIterator = existingConfigs.data().listIterator();

        while (keysIterator.hasNext()) {
            keys.add(keysIterator.next().key());
        }

        return keys;
    }

    private void createDefaultConfig(String key, Document value, Map schema, String description, String changeReason, String functionName, String autocompleteFunctionName) throws Exception {
        CreateDefaultConfigInput.Builder inputBuilder = CreateDefaultConfigInput.builder()
            .key(key)
            .value(Document.of(value))
            .schemaMember(Document.of(schema))
            .description(description)
            .changeReason(changeReason)
            .workspaceId(workspaceId)
            .orgId(orgId);

        // Add optional fields if they exist
        if (functionName != null) {
            inputBuilder.functionName(functionName);
        }
        if (autocompleteFunctionName != null) {
            inputBuilder.autocompleteFunctionName(autocompleteFunctionName);
        }

        CreateDefaultConfigInput input = inputBuilder.build();
        CreateDefaultConfigOutput output = client.createDefaultConfig(input);
    }

    private static class ConfigData {
        final String key;
        final Document value;
        final Map<String, Document> schema;
        final String description;
        final String changeReason;
        final String functionName;
        final String autocompleteFunctionName;

        ConfigData(String key, Document value, Map schema, String description, String changeReason, String functionName, String autocompleteFunctionName) {
            this.key = key;
            this.value = value;
            this.schema = schema;
            this.description = description;
            this.changeReason = changeReason;
            this.functionName = functionName;
            this.autocompleteFunctionName = autocompleteFunctionName;
        }
    }

    private static class IdentityResolverImpl implements IdentityResolver {
        TokenIdentity identity;

        IdentityResolverImpl(String token) {
            this.identity = create(token);
        }

        @Override
        public CompletableFuture<IdentityResult> resolveIdentity(AuthProperties requestProperties) {
            return CompletableFuture.completedFuture(IdentityResult.of(identity));
        }

        @Override
        public Class identityType() {
            return TokenIdentity.class;
        }
    }

    /**
     * Main method for standalone execution
     */
    public static void main(String[] args) {
        try {
            // Default values
            String baseUrl = "http://localhost:8080";
            String workspaceId = "test";
            String orgId = "localorg";
            String bearerToken = "my-token";

            // Parse command line arguments
            if (args.length > 0) {
                baseUrl = args[0];
            }
            if (args.length > 1) {
                workspaceId = args[1];
            }
            if (args.length > 2) {
                orgId = args[2];
            }
            if (args.length > 3) {
                bearerToken = args[3];
            }

            System.out.println("Initializing DefaultConfigPopulator...");
            System.out.println("Base URL: " + baseUrl);
            System.out.println("Workspace ID: " + workspaceId);
            System.out.println("Organization ID: " + orgId);

            DefaultConfigPopulator populator = new DefaultConfigPopulator(baseUrl, workspaceId, orgId, bearerToken);

            // Populate from provided data
            populator.populateFromProvidedData();

        } catch (Exception e) {
            System.err.println("Error during default config population: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
