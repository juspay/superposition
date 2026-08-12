package io.juspay.superposition.openfeature.data_source;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.sun.net.httpserver.HttpServer;
import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.model.GetConfigInput;
import io.juspay.superposition.openfeature.options.AuthMethod;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import org.junit.jupiter.api.Test;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;

/**
 * Documents and guards the reason {@code HttpDataSource} needs {@code PrefixQueryInterceptor}: the
 * smithy-java SDK does not serialize list-typed {@code @httpQuery} members ({@code prefix} /
 * {@code exclude_prefix}) on its own.
 *
 * <p>This builds the same client {@code HttpDataSource} does but WITHOUT the interceptor, sends a
 * request with {@code prefix} and {@code exclude_prefix} set, and asserts they never reach the wire.
 *
 * <p><b>If this test starts failing</b> (the SDK now serializes list query params), the interceptor
 * would double-append them — it should then be removed rather than kept.
 */
class SdkListQuerySerializationTest {

    @Test
    void sdkAloneDropsListTypedQueryParams() throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        AtomicReference<String> query = new AtomicReference<>();
        AtomicBoolean handlerRan = new AtomicBoolean(false);
        server.createContext("/config", exchange -> {
            handlerRan.set(true);
            query.set(exchange.getRequestURI().getRawQuery());
            byte[] body = "{\"contexts\":[],\"overrides\":{},\"default_configs\":{},\"dimensions\":{}}"
                    .getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().add("content-type", "application/json");
            exchange.getResponseHeaders().add("last-modified", "2024-01-01T00:00:00Z");
            exchange.getResponseHeaders().add("x-config-version", "1");
            exchange.sendResponseHeaders(200, body.length);
            try (OutputStream out = exchange.getResponseBody()) {
                out.write(body);
            }
        });
        server.start();

        try {
            SuperpositionAsyncClient sdk = SuperpositionAsyncClient.builder()
                    .endpointResolver(EndpointResolver.staticEndpoint(
                            "http://127.0.0.1:" + server.getAddress().getPort()))
                    .addIdentityResolver(new AuthMethod.Token("test-token").identityResolver())
                    // Deliberately NO PrefixQueryInterceptor — we want the SDK's own behaviour.
                    .build();

            try {
                sdk.getConfig(GetConfigInput.builder()
                        .orgId("localorg")
                        .workspaceId("test")
                        .prefix(List.of("price"))
                        .excludePrefix(List.of("secret"))
                        .build()).get();
            } catch (Exception ignored) {
                // Output parsing may fail on the stub body; the query was captured on arrival anyway.
            }

            String q = query.get();
            // The request must have reached the server, or "no prefix on the wire" would be a false
            // positive (the request simply never went out).
            assertTrue(handlerRan.get(), "request never reached the stub server");
            assertFalse(q != null && q.contains("prefix=price"),
                    "SDK serialized prefix without the interceptor; wire query=" + q);
            assertFalse(q != null && q.contains("exclude_prefix=secret"),
                    "SDK serialized exclude_prefix without the interceptor; wire query=" + q);
        } finally {
            server.stop(0);
        }
    }
}
