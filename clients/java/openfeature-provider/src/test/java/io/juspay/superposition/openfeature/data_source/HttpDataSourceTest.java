package io.juspay.superposition.openfeature.data_source;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import io.juspay.superposition.openfeature.options.AuthMethod;
import io.juspay.superposition.openfeature.options.SuperpositionOptions;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import uniffi.superposition_client.ExperimentConfig;
import uniffi.superposition_types.ExperimentStatusType;
import uniffi.superposition_types.VariantType;

/**
 * Drives HttpDataSource against a stub of the Superposition HTTP API, so 304 handling and the
 * last-modified round trip are exercised for real rather than mocked out.
 */
class HttpDataSourceTest {

    /** The instant the stub serves as `last-modified`; must survive into ConfigData.getFetchedAt(). */
    private static final Instant LAST_MODIFIED = Instant.parse("2024-03-01T10:15:30Z");

    private static final String CONFIG_BODY = """
        {
          "contexts": [
            {
              "id": "ctx-1",
              "condition": { "city": "Boston" },
              "priority": 1,
              "weight": 1,
              "override_with_keys": ["ovr-1"]
            }
          ],
          "overrides": { "ovr-1": { "currency": "Dollar" } },
          "default_configs": { "currency": "Rupee", "price": 10000 },
          "dimensions": {
            "city": {
              "schema": { "type": "string" },
              "position": 1,
              "dimension_type": { "REGULAR": {} },
              "dependency_graph": {}
            }
          }
        }
        """;

    private static final String EXPERIMENT_BODY = """
        {
          "experiments": [
            {
              "id": "exp-1",
              "created_at": "2024-03-01T10:00:00Z",
              "created_by": "tester",
              "last_modified": "2024-03-01T10:00:00Z",
              "name": "checkout",
              "experiment_type": "DEFAULT",
              "override_keys": ["price"],
              "status": "INPROGRESS",
              "traffic_percentage": 50,
              "context": { "city": "Boston" },
              "variants": [
                {
                  "id": "exp-1-control",
                  "variant_type": "CONTROL",
                  "overrides": { "price": 10000 }
                },
                {
                  "id": "exp-1-experimental",
                  "variant_type": "EXPERIMENTAL",
                  "overrides": { "price": 9000 }
                }
              ],
              "last_modified_by": "tester",
              "description": "checkout experiment",
              "change_reason": "testing"
            }
          ],
          "experiment_groups": [
            {
              "id": "grp-1",
              "context_hash": "hash",
              "name": "checkout-group",
              "description": "d",
              "change_reason": "c",
              "context": { "city": "Boston" },
              "traffic_percentage": 100,
              "member_experiment_ids": ["exp-1"],
              "created_at": "2024-03-01T10:00:00Z",
              "created_by": "tester",
              "last_modified_at": "2024-03-01T10:00:00Z",
              "last_modified_by": "tester",
              "buckets": [{ "experiment_id": "exp-1", "variant_id": "exp-1-control" }],
              "group_type": "USER_CREATED"
            }
          ]
        }
        """;

    private HttpServer server;
    private HttpDataSource source;

    /** Captures the if-modified-since header the SDK actually put on the wire. */
    private final AtomicReference<String> lastIfModifiedSince = new AtomicReference<>();

    /** When set, the stub answers 304 instead of a body. */
    private volatile boolean respondNotModified = false;

    @BeforeEach
    void startServer() throws IOException {
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/config", exchange -> respond(exchange, CONFIG_BODY));
        server.createContext("/experiment-config", exchange -> respond(exchange, EXPERIMENT_BODY));
        server.start();

        source = new HttpDataSource(new SuperpositionOptions(
            "http://127.0.0.1:" + server.getAddress().getPort(),
            new AuthMethod.Token("test-token"), "localorg", "test"));
    }

    @AfterEach
    void stopServer() throws SuperpositionError {
        source.close();
        server.stop(0);
    }

    private void respond(HttpExchange exchange, String body) throws IOException {
        lastIfModifiedSince.set(exchange.getRequestHeaders().getFirst("if-modified-since"));
        // The smithy model types these headers as date-time, so they travel as ISO-8601, not HTTP-date.
        exchange.getResponseHeaders().add("last-modified", LAST_MODIFIED.toString());
        exchange.getResponseHeaders().add("x-config-version", "1");
        exchange.getResponseHeaders().add("content-type", "application/json");

        if (respondNotModified) {
            exchange.sendResponseHeaders(304, -1);
            exchange.close();
            return;
        }

        byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.sendResponseHeaders(200, bytes.length);
        try (OutputStream out = exchange.getResponseBody()) {
            out.write(bytes);
        }
    }

    @Test
    void fetchConfigUsesServerLastModifiedAsFetchedAt() throws Exception {
        ConfigData config = source.fetchConfig(Optional.empty()).getData().orElseThrow();

        assertEquals(LAST_MODIFIED, config.getFetchedAt());
        assertEquals(1, config.getData().getContexts().size());
        assertEquals(2, config.getData().getDefaultConfigs().size());
        // Values cross the FFI boundary JSON-encoded, so a number stays a number.
        assertEquals("10000", config.getData().getDefaultConfigs().get("price"));
    }

    @Test
    void notModifiedResponseIsNotAnError() throws Exception {
        respondNotModified = true;

        FetchResponse<ConfigData> response = source.fetchConfig(Optional.of(LAST_MODIFIED));

        assertTrue(response.isNotModified());
        assertTrue(response.getData().isEmpty());
        assertEquals(LAST_MODIFIED.toString(), lastIfModifiedSince.get());
    }

    @Test
    void notModifiedIsDistinctFromAServerError() {
        server.removeContext("/config");
        server.createContext("/config", exchange -> {
            exchange.sendResponseHeaders(500, -1);
            exchange.close();
        });

        // A 500 whose body/message could contain "304" must still surface as an error.
        assertThrows(SuperpositionError.class, () -> source.fetchConfig(Optional.empty()));
    }

    @Test
    void fetchesExperimentsAndConvertsThemForEvaluation() throws Exception {
        ExperimentData experiments =
            source.fetchActiveExperiments(Optional.empty()).getData().orElseThrow();

        assertEquals(LAST_MODIFIED, experiments.getFetchedAt());

        ExperimentConfig config = experiments.getData();
        assertEquals(1, config.getExperiments().size());
        assertEquals(1, config.getExperimentGroups().size());

        var experiment = config.getExperiments().get(0);
        assertEquals("exp-1", experiment.getId());
        assertEquals(ExperimentStatusType.INPROGRESS, experiment.getStatus());
        assertEquals(Map.of("city", "\"Boston\""), experiment.getContext());
        assertEquals(VariantType.CONTROL, experiment.getVariants().get(0).getVariantType());
        assertEquals("9000", experiment.getVariants().get(1).getOverrides().get("price"));

        var group = config.getExperimentGroups().get(0);
        assertEquals(List.of("exp-1"), group.getMemberExperimentIds());
        assertEquals(1, group.getBuckets().size());
    }

    @Test
    void candidateAndMatchingExperimentsSendTheirDimensionStrategies() throws Exception {
        AtomicReference<String> query = new AtomicReference<>();
        server.removeContext("/experiment-config");
        server.createContext("/experiment-config", exchange -> {
            query.set(exchange.getRequestURI().getQuery());
            respond(exchange, EXPERIMENT_BODY);
        });

        Optional<Map<String, String>> context = Optional.of(Map.of("city", "\"Boston\""));

        source.fetchCandidateActiveExperiments(context, Optional.empty(), Optional.empty());
        assertTrue(query.get().contains("dimension_match_strategy=exact"), query.get());

        source.fetchMatchingActiveExperiments(context, Optional.of(List.of("price")), Optional.empty());
        assertTrue(query.get().contains("dimension_match_strategy=subset"), query.get());
        assertTrue(query.get().contains("prefix=price"), query.get());
    }

    @Test
    void supportsExperiments() {
        assertTrue(source.supportsExperiments());
    }
}
