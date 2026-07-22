package io.juspay.superposition.openfeature.data_source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import io.juspay.superposition.openfeature.error.SuperpositionError;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import uniffi.superposition_types.Config;

class FileDataSourceTest {

    private static final String TOML_CONFIG = """
        [default-configs]
        currency = { value = "Rupee", schema = { type = "string" } }
        price = { value = 10000, schema = { type = "number" } }

        [dimensions]
        city = { position = 1, schema = { type = "string" }, type = "REGULAR" }

        [[overrides]]
        _context_ = { city = "Boston" }
        currency = "Dollar"

        [[overrides]]
        _context_ = { city = "Berlin" }
        price = 20000
        """;

    private static final String JSON_CONFIG = """
        {
          "default-configs": {
            "currency": { "value": "Rupee", "schema": { "type": "string" } }
          },
          "dimensions": {
            "city": { "position": 1, "schema": { "type": "string" }, "type": "REGULAR" }
          },
          "overrides": [
            { "_context_": { "city": "Boston" }, "currency": "Dollar" }
          ]
        }
        """;

    private static FileDataSource sourceOf(Path dir, String name, String content) throws Exception {
        Path file = dir.resolve(name);
        Files.writeString(file, content);
        return new FileDataSource(file);
    }

    @Test
    void readsTomlConfig(@TempDir Path dir) throws Exception {
        Config config = sourceOf(dir, "config.toml", TOML_CONFIG)
            .fetchConfig(Optional.empty())
            .getData()
            .orElseThrow()
            .getData();

        assertEquals(2, config.getDefaultConfigs().size());
        assertEquals(2, config.getContexts().size());
        assertEquals(1, config.getDimensions().size());
    }

    @Test
    void readsJsonConfig(@TempDir Path dir) throws Exception {
        Config config = sourceOf(dir, "config.json", JSON_CONFIG)
            .fetchConfig(Optional.empty())
            .getData()
            .orElseThrow()
            .getData();

        assertEquals(1, config.getDefaultConfigs().size());
        assertEquals(1, config.getContexts().size());
    }

    @Test
    void rejectsUnsupportedExtension(@TempDir Path dir) {
        assertThrows(SuperpositionError.class, () -> new FileDataSource(dir.resolve("config.yaml")));
        assertThrows(SuperpositionError.class, () -> new FileDataSource(dir.resolve("config")));
    }

    @Test
    void prefixFilterPrunesOverridesAndContexts(@TempDir Path dir) throws Exception {
        Config config = sourceOf(dir, "config.toml", TOML_CONFIG)
            .fetchFilteredConfig(Optional.empty(), Optional.of(List.of("currency")), Optional.empty())
            .getData()
            .orElseThrow()
            .getData();

        assertEquals(Map.of("currency", "\"Rupee\""), config.getDefaultConfigs());
        // The Berlin context only overrode `price`, so it drops out along with its override.
        assertEquals(1, config.getContexts().size());
        assertEquals(1, config.getOverrides().size());
    }

    @Test
    void contextFilterKeepsOnlyMatchingContexts(@TempDir Path dir) throws Exception {
        Config config = sourceOf(dir, "config.toml", TOML_CONFIG)
            .fetchFilteredConfig(
                Optional.of(Map.of("city", "\"Boston\"")), Optional.empty(), Optional.empty())
            .getData()
            .orElseThrow()
            .getData();

        assertEquals(1, config.getContexts().size());
        assertEquals(1, config.getOverrides().size());
        assertTrue(config.getOverrides().values().iterator().next().containsKey("currency"));
    }

    @Test
    void experimentsAreUnsupported(@TempDir Path dir) throws Exception {
        FileDataSource source = sourceOf(dir, "config.toml", TOML_CONFIG);

        assertFalse(source.supportsExperiments());
        assertThrows(SuperpositionError.class, () -> source.fetchActiveExperiments(Optional.empty()));
    }

    @Test
    void watchFansChangesOutToEverySubscriber(@TempDir Path dir) throws Exception {
        FileDataSource source = sourceOf(dir, "config.toml", TOML_CONFIG);
        WatchStream first = source.watch().orElseThrow();
        WatchStream second = source.watch().orElseThrow();

        Files.writeString(dir.resolve("config.toml"), TOML_CONFIG + "\n");

        assertTrue(first.tryGetNextEvent(20_000), "first subscriber saw no change");
        assertTrue(second.tryGetNextEvent(20_000), "second subscriber saw no change");

        // close() must release consumers blocked on the stream, not leave them parked forever.
        source.close();
        assertFalse(first.getNextEvent());
        assertTrue(first.isClosed());
    }

    @Test
    void ifModifiedSinceReturnsNotModifiedForAnUnchangedFile(@TempDir Path dir) throws Exception {
        FileDataSource source = sourceOf(dir, "config.toml", TOML_CONFIG);
        Instant fetchedAt = source.fetchConfig(Optional.empty()).getData().orElseThrow().getFetchedAt();

        // The file has not changed since it was read, so a conditional fetch is NotModified.
        assertTrue(source.fetchConfig(Optional.of(fetchedAt)).isNotModified());
    }

    @Test
    void ifModifiedSinceReturnsFreshDataAfterTheFileChanges(@TempDir Path dir) throws Exception {
        Path file = dir.resolve("config.toml");
        Files.writeString(file, TOML_CONFIG);
        FileDataSource source = new FileDataSource(file);
        Instant fetchedAt = source.fetchConfig(Optional.empty()).getData().orElseThrow().getFetchedAt();

        // Rewrite the file with a later modified time; the next conditional fetch must re-read.
        Files.writeString(file, TOML_CONFIG.replace("\"Rupee\"", "\"Yen\""));
        Files.setLastModifiedTime(file, FileTime.from(fetchedAt.plusSeconds(2)));

        FetchResponse<ConfigData> response = source.fetchConfig(Optional.of(fetchedAt));
        assertFalse(response.isNotModified());
        assertEquals("\"Yen\"",
            response.getData().orElseThrow().getData().getDefaultConfigs().get("currency"));
    }

    /**
     * A watch that cannot be registered must fail as a checked SuperpositionError and leave nothing
     * behind — no half-registered watcher, no subscriber. Registration used to publish the
     * WatchService to the field *before* registering, so a failure left a non-null watchService
     * with no watch thread running: the next watch() took the "existing watcher" branch and handed
     * back a stream that could never fire.
     */
    @Test
    void aWatchThatCannotBeRegisteredFailsCleanlyAndLeavesTheSourceUsable(@TempDir Path dir)
            throws Exception {
        Path missingDir = dir.resolve("does-not-exist");
        FileDataSource broken = new FileDataSource(missingDir.resolve("config.toml"));

        assertThrows(SuperpositionError.class, broken::watch);
        // Repeatable: the failure must not have poisoned any state.
        assertThrows(SuperpositionError.class, broken::watch);
        broken.close();

        // An unrelated, valid source still watches normally.
        FileDataSource healthy = sourceOf(dir, "config.toml", TOML_CONFIG);
        WatchStream stream = healthy.watch().orElseThrow();
        Files.writeString(dir.resolve("config.toml"), TOML_CONFIG + "\n");
        assertTrue(stream.tryGetNextEvent(20_000), "a healthy watcher stopped delivering events");
        healthy.close();
    }
}
