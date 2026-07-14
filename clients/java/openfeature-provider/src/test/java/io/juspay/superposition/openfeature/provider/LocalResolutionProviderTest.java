package io.juspay.superposition.openfeature.provider;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import dev.openfeature.sdk.ErrorCode;
import dev.openfeature.sdk.EvaluationContext;
import dev.openfeature.sdk.ImmutableContext;
import dev.openfeature.sdk.ProviderState;
import dev.openfeature.sdk.Value;
import io.juspay.superposition.openfeature.data_source.ConfigData;
import io.juspay.superposition.openfeature.data_source.FetchResponse;
import io.juspay.superposition.openfeature.data_source.FileDataSource;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import io.juspay.superposition.openfeature.options.RefreshStrategy;
import java.nio.file.Files;
import java.time.Instant;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import uniffi.superposition_types.Config;

class LocalResolutionProviderTest {

    private static final String TOML_CONFIG = """
        [default-configs]
        currency = { value = "Rupee", schema = { type = "string" } }
        price = { value = 10000, schema = { type = "number" } }
        enabled = { value = true, schema = { type = "boolean" } }
        ratio = { value = 1.5, schema = { type = "number" } }
        tags = { value = ["a", "b"], schema = { type = "array" } }
        meta = { value = { tier = "gold", credits = 5, limits = { retries = 3 } }, schema = { type = "object" } }
        counts = { value = [1, 2], schema = { type = "array" } }

        [dimensions]
        city = { position = 1, schema = { type = "string" }, type = "REGULAR" }

        [[overrides]]
        _context_ = { city = "Boston" }
        currency = "Dollar"

        [[overrides]]
        _context_ = { city = "Berlin" }
        price = 20000
        """;

    private Path configFile;
    private LocalResolutionProvider provider;

    @BeforeEach
    void setUp(@TempDir Path dir) throws Exception {
        configFile = dir.resolve("config.toml");
        Files.writeString(configFile, TOML_CONFIG);

        provider = new LocalResolutionProvider(
            new FileDataSource(configFile),
            Optional.empty(),
            new RefreshStrategy.Manual(1000));
        provider.initialize(new ImmutableContext());
    }

    @AfterEach
    void tearDown() {
        provider.shutdown();
    }

    private static EvaluationContext contextOf(Map<String, Value> attributes) {
        return new ImmutableContext(attributes);
    }

    @Test
    void initializeMakesTheProviderReady() {
        assertEquals(ProviderState.READY, provider.getState());
        assertEquals("LocalResolutionProvider", provider.getMetadata().getName());
    }

    @Test
    void reinitializingALiveProviderIsANoOpAndKeepsItServing() throws Exception {
        assertEquals(ProviderState.READY, provider.getState());

        // A second initialize() must not strand the first strategy's task or leak the cache; it is
        // simply ignored. The provider stays READY and keeps resolving.
        provider.initialize(new ImmutableContext());

        assertEquals(ProviderState.READY, provider.getState());
        assertEquals("Rupee",
            provider.getStringEvaluation("currency", "none", new ImmutableContext()).getValue());
    }

    @Test
    void resolvesDefaultConfigValuesWithTheirRealTypes() {
        EvaluationContext context = new ImmutableContext();

        // The values are stored JSON-encoded, so a string must come back unquoted.
        assertEquals("Rupee", provider.getStringEvaluation("currency", "none", context).getValue());
        assertEquals(10000, provider.getIntegerEvaluation("price", 0, context).getValue());
        assertEquals(10000.0, provider.getDoubleEvaluation("price", 0.0, context).getValue());
        assertEquals(true, provider.getBooleanEvaluation("enabled", false, context).getValue());
        assertEquals(1.5, provider.getDoubleEvaluation("ratio", 0.0, context).getValue());
    }

    /**
     * The type contract is shared with the Rust and Python clients: no coercion, an integer
     * widens to a float, and nothing else converts. Each assertion here is a value that a
     * client could previously read as the wrong type.
     */
    @Test
    void enforcesTheCrossLanguageTypeContract() {
        EvaluationContext context = new ImmutableContext();

        // A float is not an integer. This used to truncate 1.5 to 1.
        assertEquals(ErrorCode.TYPE_MISMATCH,
            provider.getIntegerEvaluation("ratio", 0, context).getErrorCode());

        // ...but an integer is a float. Widening is lossless, so it is allowed.
        assertEquals(10000.0, provider.getDoubleEvaluation("price", 0.0, context).getValue());

        // A string is not a boolean and a number is not a boolean. Python used to coerce both.
        assertEquals(ErrorCode.TYPE_MISMATCH,
            provider.getBooleanEvaluation("currency", false, context).getErrorCode());
        assertEquals(ErrorCode.TYPE_MISMATCH,
            provider.getBooleanEvaluation("price", false, context).getErrorCode());

        // A primitive is not an object. This used to succeed via Value.objectToValue.
        assertEquals(ErrorCode.TYPE_MISMATCH,
            provider.getObjectEvaluation("currency", new Value(""), context).getErrorCode());

        // A number is not a string.
        assertEquals(ErrorCode.TYPE_MISMATCH,
            provider.getStringEvaluation("price", "none", context).getErrorCode());
    }

    @Test
    void resolvesObjectAndArrayFlags() {
        EvaluationContext context = new ImmutableContext();

        Value meta = provider.getObjectEvaluation("meta", new Value(""), context).getValue();
        assertEquals("gold", meta.asStructure().getValue("tier").asString());

        // Integers keep their type at every depth, rather than becoming 5.0 / 3.0. This is what
        // pins the custom number strategy to nested values and not just the top level.
        assertEquals(5, meta.asStructure().getValue("credits").asInteger());
        assertEquals(3, meta.asStructure().getValue("limits")
            .asStructure().getValue("retries").asInteger());

        Value tags = provider.getObjectEvaluation("tags", new Value(""), context).getValue();
        assertEquals(List.of(new Value("a"), new Value("b")), tags.asList());

        Value counts = provider.getObjectEvaluation("counts", new Value(""), context).getValue();
        assertEquals(List.of(new Value(1), new Value(2)), counts.asList());
    }

    @Test
    void appliesContextOverrides() {
        EvaluationContext boston = contextOf(Map.of("city", new Value("Boston")));
        EvaluationContext berlin = contextOf(Map.of("city", new Value("Berlin")));

        assertEquals("Dollar", provider.getStringEvaluation("currency", "none", boston).getValue());
        assertEquals(10000, provider.getIntegerEvaluation("price", 0, boston).getValue());
        assertEquals(20000, provider.getIntegerEvaluation("price", 0, berlin).getValue());
    }

    @Test
    void reportsFlagNotFoundAndTypeMismatch() {
        EvaluationContext context = new ImmutableContext();

        var missing = provider.getStringEvaluation("nope", "fallback", context);
        assertEquals(ErrorCode.FLAG_NOT_FOUND, missing.getErrorCode());

        var mismatched = provider.getBooleanEvaluation("currency", false, context);
        assertEquals(ErrorCode.TYPE_MISMATCH, mismatched.getErrorCode());
    }

    @Test
    void resolvesAllFeaturesWithPrefixFilter() throws Exception {
        Map<String, String> all = provider.resolveAllFeatures(new ImmutableContext());
        assertEquals(7, all.size());

        Map<String, String> filtered = provider.resolveAllFeaturesWithFilter(
            new ImmutableContext(), Optional.of(List.of("cur")));
        assertEquals(Map.of("currency", "\"Rupee\""), filtered);
    }

    @Test
    void globalContextIsOverriddenByTheEvaluationContext() throws Exception {
        LocalResolutionProvider withGlobal = new LocalResolutionProvider(
            new FileDataSource(configFile), Optional.empty(), new RefreshStrategy.Manual(1000));
        withGlobal.initialize(contextOf(Map.of("city", new Value("Boston"))));

        try {
            // Global context alone resolves the Boston override...
            assertEquals("Dollar",
                withGlobal.getStringEvaluation("currency", "none", new ImmutableContext()).getValue());
            // ...but the evaluation context wins where the two disagree.
            assertEquals(20000, withGlobal.getIntegerEvaluation(
                "price", 0, contextOf(Map.of("city", new Value("Berlin")))).getValue());
        } finally {
            withGlobal.shutdown();
        }
    }

    @Test
    void servesItsCacheAsAFilteredDataSource() throws Exception {
        // Prefix filter prunes the contexts and overrides that no longer contribute a key.
        Config byPrefix = provider
            .fetchFilteredConfig(Optional.empty(), Optional.of(List.of("currency")), Optional.empty())
            .getData().orElseThrow().getData();
        assertEquals(Map.of("currency", "\"Rupee\""), byPrefix.getDefaultConfigs());
        assertEquals(1, byPrefix.getContexts().size());

        // Dimension filter keeps only the contexts that match.
        Config byContext = provider
            .fetchFilteredConfig(Optional.of(Map.of("city", "\"Berlin\"")), Optional.empty(), Optional.empty())
            .getData().orElseThrow().getData();
        assertEquals(1, byContext.getContexts().size());
        assertTrue(byContext.getOverrides().values().iterator().next().containsKey("price"));

        // Unfiltered still serves everything, stamped with the underlying fetch time.
        ConfigData unfiltered = provider.fetchConfig(Optional.empty()).getData().orElseThrow();
        assertEquals(2, unfiltered.getData().getContexts().size());
        assertEquals(7, unfiltered.getData().getDefaultConfigs().size());
    }

    @Test
    void experimentsFollowTheDataSourceCapability() {
        // FileDataSource has no experiments, so neither does the provider built on it.
        assertFalse(provider.supportsExperiments());
        assertThrows(SuperpositionError.class, () -> provider.fetchActiveExperiments(Optional.empty()));
        assertThrows(SuperpositionError.class, () -> provider.fetchCandidateActiveExperiments(
            Optional.empty(), Optional.empty(), Optional.empty()));
    }

    @Test
    void manualRefreshPicksUpFileChanges() throws Exception {
        assertEquals("Rupee", provider.getStringEvaluation("currency", "none", new ImmutableContext()).getValue());

        Files.writeString(configFile, TOML_CONFIG.replace("\"Rupee\"", "\"Yen\""));
        provider.refresh();

        assertEquals("Yen", provider.getStringEvaluation("currency", "none", new ImmutableContext()).getValue());
    }

    @Test
    void refreshIsBoundedByTheStrategyTimeout() throws Exception {
        HangingDataSource source = new HangingDataSource();
        LocalResolutionProvider hanging = new LocalResolutionProvider(
            source, Optional.empty(), new RefreshStrategy.Manual(300));
        hanging.initialize(new ImmutableContext());

        try {
            source.hang = true;

            long startedAt = System.nanoTime();
            SuperpositionError error = assertThrows(SuperpositionError.class, hanging::refresh);
            long elapsedMs = (System.nanoTime() - startedAt) / 1_000_000;

            assertTrue(error.getMessage().contains("timed out"), error.getMessage());
            assertTrue(elapsedMs < 5_000, "refresh blocked for " + elapsedMs + "ms despite a 300ms timeout");

            // The hung refresh must leave the last known good config in place.
            assertEquals("Rupee",
                hanging.getStringEvaluation("currency", "none", new ImmutableContext()).getValue());
        } finally {
            hanging.shutdown();
        }
    }

    @Test
    void aFailedRefreshLeavesTheProviderStaleUntilItRecovers() throws Exception {
        HangingDataSource source = new HangingDataSource();
        LocalResolutionProvider flaky = new LocalResolutionProvider(
            source, Optional.empty(), new RefreshStrategy.Manual(1000));
        flaky.initialize(new ImmutableContext());

        try {
            assertEquals(ProviderState.READY, flaky.getState());

            source.fail = true;
            assertThrows(SuperpositionError.class, flaky::refresh);

            // The cache is frozen at its last known good values, and STALE is the only signal a
            // consumer has that the flags stopped tracking the source of truth.
            assertEquals(ProviderState.STALE, flaky.getState());
            assertEquals("Rupee",
                flaky.getStringEvaluation("currency", "none", new ImmutableContext()).getValue());

            source.fail = false;
            flaky.refresh();
            assertEquals(ProviderState.READY, flaky.getState());
        } finally {
            flaky.shutdown();
        }
    }

    /** Serves config once, then hangs or fails on demand — standing in for a flaky backend. */
    private final class HangingDataSource extends FileDataSource {
        volatile boolean hang = false;
        volatile boolean fail = false;

        HangingDataSource() throws SuperpositionError {
            super(configFile);
        }

        @Override
        public FetchResponse<ConfigData> fetchFilteredConfig(
                Optional<Map<String, String>> context,
                Optional<List<String>> prefixFilter,
                Optional<Instant> ifModifiedSince) throws SuperpositionError {
            if (fail) {
                throw SuperpositionError.networkError("backend is down");
            }
            if (hang) {
                try {
                    Thread.sleep(60_000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
            return super.fetchFilteredConfig(context, prefixFilter, ifModifiedSince);
        }
    }

    @Test
    void watchStrategyRefreshesWhenTheFileChanges(@TempDir Path dir) throws Exception {
        Path watched = dir.resolve("watched.toml");
        Files.writeString(watched, TOML_CONFIG);

        LocalResolutionProvider watching = new LocalResolutionProvider(
            new FileDataSource(watched), Optional.empty(), new RefreshStrategy.Watch(1000, 100));
        watching.initialize(new ImmutableContext());

        try {
            assertEquals("Rupee",
                watching.getStringEvaluation("currency", "none", new ImmutableContext()).getValue());

            Files.writeString(watched, TOML_CONFIG.replace("\"Rupee\"", "\"Yen\""));

            // The JDK falls back to a polling WatchService on macOS, so allow for its interval.
            String currency = null;
            for (int attempt = 0; attempt < 60 && !"Yen".equals(currency); attempt++) {
                Thread.sleep(500);
                currency = watching.getStringEvaluation("currency", "none", new ImmutableContext()).getValue();
            }
            assertEquals("Yen", currency, "watch strategy did not pick up the file change");
        } finally {
            watching.shutdown();
        }
    }

    @Test
    void watchStrategyRejectsADataSourceThatCannotWatch() throws Exception {
        LocalResolutionProvider unwatchable = new LocalResolutionProvider(
            new NonWatchableSource(), Optional.empty(), new RefreshStrategy.Watch(1000, 50));

        SuperpositionError error = assertThrows(SuperpositionError.class,
            () -> unwatchable.initialize(new ImmutableContext()));
        assertTrue(error.getMessage().contains("does not support watching"), error.getMessage());
        assertEquals(ProviderState.ERROR, unwatchable.getState());
    }

    /** A data source that serves config but, like the HTTP one, cannot watch for changes. */
    private final class NonWatchableSource extends FileDataSource {
        NonWatchableSource() throws SuperpositionError {
            super(configFile);
        }

        @Override
        public Optional<io.juspay.superposition.openfeature.data_source.WatchStream> watch() {
            return Optional.empty();
        }
    }
}
