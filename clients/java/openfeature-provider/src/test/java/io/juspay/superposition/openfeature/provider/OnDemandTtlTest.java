package io.juspay.superposition.openfeature.provider;

import static org.junit.jupiter.api.Assertions.assertEquals;

import dev.openfeature.sdk.ImmutableContext;
import io.juspay.superposition.openfeature.data_source.ConfigData;
import io.juspay.superposition.openfeature.data_source.FetchResponse;
import io.juspay.superposition.openfeature.data_source.FileDataSource;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import io.juspay.superposition.openfeature.options.RefreshStrategy;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * The OnDemand TTL is measured from the last <em>check</em>, not from the config's last
 * <em>change</em>.
 *
 * <p>An HTTP data source stamps {@code ConfigData.fetchedAt} with the server's
 * {@code last-modified} — when the config last changed. Driving the TTL off that made a config
 * which had been stable for longer than the TTL permanently "stale": every evaluation fired a
 * fetch, the 304 that came back left the timestamp untouched, and the next evaluation fired
 * another. The more stable the config, the more load it generated.
 *
 * <p>{@code FileDataSource} stamps {@code Instant.now()} instead, which is why the file-based
 * tests never saw this. {@link ServerLike} reproduces the HTTP behaviour.
 */
class OnDemandTtlTest {

    private static final String TOML = """
        [default-configs]
        currency = { value = "Rupee", schema = { type = "string" } }
        [dimensions]
        city = { position = 1, schema = { type = "string" }, type = "REGULAR" }
        """;

    /** Behaves like HttpDataSource: fetchedAt is the server's last-modified, repeats get a 304. */
    static final class ServerLike extends FileDataSource {
        final AtomicInteger fetches = new AtomicInteger();

        /** The config last changed an hour ago — and is perfectly current. */
        final Instant serverLastModified = Instant.now().minus(1, ChronoUnit.HOURS);

        ServerLike(Path path) throws SuperpositionError {
            super(path);
        }

        @Override
        public FetchResponse<ConfigData> fetchFilteredConfig(
                Optional<Map<String, String>> context,
                Optional<List<String>> prefixFilter,
                Optional<Instant> ifModifiedSince) throws SuperpositionError {
            fetches.incrementAndGet();
            if (ifModifiedSince.isPresent()) {
                return FetchResponse.notModified();
            }
            ConfigData fresh = super.fetchFilteredConfig(context, prefixFilter, Optional.empty())
                    .getData().orElseThrow();
            return FetchResponse.data(new ConfigData(fresh.getData(), serverLastModified));
        }
    }

    private static ServerLike sourceIn(Path dir) throws Exception {
        Path file = dir.resolve("config.toml");
        Files.writeString(file, TOML);
        return new ServerLike(file);
    }

    @Test
    void anUnchangedConfigIsNotRefetchedWithinTheTtl(@TempDir Path dir) throws Exception {
        ServerLike source = sourceIn(dir);
        LocalResolutionProvider provider = new LocalResolutionProvider(
            source, Optional.empty(), new RefreshStrategy.OnDemand(5_000, 60_000));
        provider.initialize(new ImmutableContext());

        try {
            int afterInit = source.fetches.get();

            for (int i = 0; i < 5; i++) {
                provider.getStringEvaluation("currency", "none", new ImmutableContext());
            }

            assertEquals(afterInit, source.fetches.get(),
                "evaluations inside the TTL must not hit the source, however old the config is");
        } finally {
            provider.shutdown();
        }
    }

    @Test
    void theTtlStillExpiresAndA304RestartsIt(@TempDir Path dir) throws Exception {
        ServerLike source = sourceIn(dir);
        LocalResolutionProvider provider = new LocalResolutionProvider(
            source, Optional.empty(), new RefreshStrategy.OnDemand(5_000, 100));
        provider.initialize(new ImmutableContext());

        try {
            int afterInit = source.fetches.get();

            // Inside the TTL: free.
            provider.getStringEvaluation("currency", "none", new ImmutableContext());
            assertEquals(afterInit, source.fetches.get());

            Thread.sleep(200);

            // Past the TTL: exactly one re-check.
            provider.getStringEvaluation("currency", "none", new ImmutableContext());
            assertEquals(afterInit + 1, source.fetches.get(),
                "an expired TTL must re-check the source");

            // The 304 it got back is a successful check, so the clock restarts and this is free.
            provider.getStringEvaluation("currency", "none", new ImmutableContext());
            assertEquals(afterInit + 1, source.fetches.get(),
                "a 304 is a successful check and must restart the TTL");

            // And the cache kept serving the last known good value throughout.
            assertEquals("Rupee",
                provider.getStringEvaluation("currency", "none", new ImmutableContext()).getValue());
        } finally {
            provider.shutdown();
        }
    }
}
