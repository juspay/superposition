package io.juspay.superposition.openfeature;

import io.juspay.superposition.client.SuperpositionClient;
import io.juspay.superposition.openfeature.options.RefreshStrategy;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import dev.openfeature.sdk.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;

class ProviderTest {
    static {
        System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "DEBUG");
    }

    final static String endpoint = "http://localhost:8080";
    final static String workspace = "test";
    final static String orgId = "localorg";
    final static String token = "my-token";
    final static Logger log = LoggerFactory.getLogger(ProviderTest.class);
    final static SuperpositionProviderOptions.ExperimentationOptions eopts = SuperpositionProviderOptions.ExperimentationOptions.builder()
        .refreshStrategy(RefreshStrategy.Polling.of(1000, 1000))
        .build();
    final static SuperpositionProviderOptions options = SuperpositionProviderOptions.builder()
        .orgId(orgId)
        .workspaceId(workspace)
        .endpoint(endpoint)
        .token(token)
        .refreshStrategy(RefreshStrategy.Polling.of(1000, 100))
        .experimentationOptions(eopts)
        .build();
    SuperpositionOpenFeatureProvider provider;
    FeatureProvider fp;

    @BeforeAll
    static void setupData() {
        DefaultConfigPopulator populator = new DefaultConfigPopulator(endpoint, workspace, orgId, token);
        try {
            populator.populateFromProvidedData();
        } catch (Exception e) {
            System.err.println("Error during default config population: " + e.getMessage());
            e.printStackTrace();
        }
    }

    @BeforeEach
    void setupProvider() {
        provider = new SuperpositionOpenFeatureProvider(options);
        fp = provider;
    }

    @AfterEach
    void shutdownProvider() {
        provider.shutdown();
    }

    @Test
    void testProviderInitialization() {
        var ctx = new ImmutableContext(Map.of("d1", new Value(false)));
        provider.initialize(ctx);
    }

    @Test
    void testBooleanEvaluation() {
        var ctx = new ImmutableContext(Map.of());
        provider.initialize(ctx);
        var pe = provider.getBooleanEvaluation("bool", false, ctx);
        assertEquals(true, pe.getValue());
    }

    @Test
    void testIntegerEvaluation() {
        var ctx = new ImmutableContext(Map.of());
        provider.initialize(ctx);
        var pe = provider.getIntegerEvaluation("integer", 2, ctx);
        assertEquals(1, pe.getValue());
    }

    @Test
    void testDoubleEvaluation() {
        var ctx = new ImmutableContext(Map.of());
        provider.initialize(ctx);
        var pe = provider.getDoubleEvaluation("double", 2d, ctx);
        log.info("Value: {}", pe.getValue());
        assertEquals(1.2d, pe.getValue());
    }

    @Test
    void testStringEvaluation() {
        var ctx = new ImmutableContext(Map.of());
        provider.initialize(ctx);
        var pe = provider.getStringEvaluation("string", "", ctx);
        assertEquals("something", pe.getValue());
    }

    @Test
    void testValueNestedObjectEvaluation() {
        var ctx = new ImmutableContext(Map.of());
        provider.initialize(ctx);
        var pe = provider.getObjectEvaluation("object", new Value(""), ctx);
        assertEquals(Value.objectToValue(Map.of("k1", Map.of("k2", "v2"))), pe.getValue());
    }

    @Test
    void testValueNestedListEvaluation() {
        var ctx = new ImmutableContext(Map.of());
        provider.initialize(ctx);
        var pe = provider.getObjectEvaluation("list", new Value(""), ctx);
        assertEquals(Value.objectToValue(List.of("k1", "v1")), pe.getValue());
    }

    @Test
    void testExperimentation() {
        var ctx = new ImmutableContext("30", Map.of("d1", new Value(true)));
        provider.initialize(ctx);
        var pe = provider.getBooleanEvaluation("bool", true, ctx);
        assertEquals(false, pe.getValue());
    }

    @Test
    void testOpenfeatureClient() {
        var ctx = new ImmutableContext(Map.of());
        provider.initialize(ctx);
        OpenFeatureAPI.getInstance().setProvider(provider);
        Client client = OpenFeatureAPI.getInstance().getClient();
        assertEquals(true, client.getBooleanValue("bool", false, ctx));
    }
}
