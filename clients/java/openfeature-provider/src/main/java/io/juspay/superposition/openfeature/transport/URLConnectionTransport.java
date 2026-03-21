package io.juspay.superposition.openfeature.transport;

import software.amazon.smithy.java.client.core.ClientConfig;
import software.amazon.smithy.java.client.core.ClientTransport;
import software.amazon.smithy.java.client.core.MessageExchange;
import software.amazon.smithy.java.client.http.HttpMessageExchange;
import software.amazon.smithy.java.context.Context;
import software.amazon.smithy.java.http.api.HttpHeaders;
import software.amazon.smithy.java.http.api.HttpRequest;
import software.amazon.smithy.java.http.api.HttpResponse;
import software.amazon.smithy.java.io.datastream.DataStream;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.net.HttpURLConnection;
import java.nio.ByteBuffer;
import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * An HTTP transport implementation using {@link java.net.HttpURLConnection}.
 *
 * <p>This transport is compatible with Android, which does not include the
 * {@code java.net.http.HttpClient} used by the default Smithy Java HTTP transport.
 *
 * <p>Usage with the Superposition client:
 * <pre>{@code
 * var transport = new URLConnectionTransport();
 * var client = SuperpositionClient.builder()
 *     .transport(transport)
 *     .endpointResolver(EndpointResolver.staticEndpoint("https://api.example.com"))
 *     .build();
 * }</pre>
 */
public final class URLConnectionTransport implements ClientTransport<HttpRequest, HttpResponse> {

    private final Duration connectTimeout;
    private final Duration readTimeout;
    private final ExecutorService executor;

    /**
     * Creates a transport with default timeouts (10 seconds connect, 30 seconds read).
     */
    public URLConnectionTransport() {
        this(Duration.ofSeconds(10), Duration.ofSeconds(30));
    }

    /**
     * Creates a transport with the specified timeouts.
     *
     * @param connectTimeout the connection timeout
     * @param readTimeout    the read timeout
     */
    public URLConnectionTransport(Duration connectTimeout, Duration readTimeout) {
        this.connectTimeout = Objects.requireNonNull(connectTimeout, "connectTimeout");
        this.readTimeout = Objects.requireNonNull(readTimeout, "readTimeout");
        this.executor = Executors.newCachedThreadPool(r -> {
            Thread t = new Thread(r, "url-connection-transport");
            t.setDaemon(true);
            return t;
        });
    }

    @Override
    public MessageExchange<HttpRequest, HttpResponse> messageExchange() {
        return HttpMessageExchange.INSTANCE;
    }

    @Override
    public CompletableFuture<HttpResponse> send(Context context, HttpRequest request) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return doSend(request);
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }
        }, executor);
    }

    private HttpResponse doSend(HttpRequest request) throws IOException {
        HttpURLConnection conn = (HttpURLConnection) request.uri().toURL().openConnection();
        try {
            conn.setRequestMethod(request.method());
            conn.setConnectTimeout((int) connectTimeout.toMillis());
            conn.setReadTimeout((int) readTimeout.toMillis());

            // Set headers
            for (Map.Entry<String, List<String>> header : request.headers()) {
                for (String value : header.getValue()) {
                    conn.addRequestProperty(header.getKey(), value);
                }
            }

            // Write request body if present
            DataStream body = request.body();
            if (body != null && body.contentLength() != 0) {
                conn.setDoOutput(true);
                try (OutputStream os = conn.getOutputStream()) {
                    writeBody(body, os);
                }
            }

            // Read the response
            int statusCode = conn.getResponseCode();

            // Build response headers
            Map<String, List<String>> responseHeaders = new HashMap<>();
            for (Map.Entry<String, List<String>> entry : conn.getHeaderFields().entrySet()) {
                if (entry.getKey() != null) {
                    responseHeaders.put(entry.getKey().toLowerCase(), entry.getValue());
                }
            }

            // Read response body
            byte[] responseBody = readResponseBody(conn, statusCode);

            return HttpResponse.builder()
                .statusCode(statusCode)
                .headers(HttpHeaders.of(responseHeaders))
                .body(DataStream.ofBytes(responseBody))
                .build();
        } finally {
            conn.disconnect();
        }
    }

    private static byte[] readResponseBody(HttpURLConnection conn, int statusCode) throws IOException {
        InputStream responseStream = null;
        try {
            responseStream = statusCode >= 400 ? conn.getErrorStream() : conn.getInputStream();
            if (responseStream == null) {
                return new byte[0];
            }
            return readAllBytes(responseStream);
        } finally {
            if (responseStream != null) {
                responseStream.close();
            }
        }
    }

    private static void writeBody(DataStream body, OutputStream os) throws IOException {
        ByteBuffer buffer = body.waitForByteBuffer();
        if (buffer.hasArray()) {
            os.write(buffer.array(), buffer.arrayOffset() + buffer.position(), buffer.remaining());
        } else {
            byte[] bytes = new byte[buffer.remaining()];
            buffer.get(bytes);
            os.write(bytes);
        }
    }

    private static byte[] readAllBytes(InputStream is) throws IOException {
        ByteArrayOutputStream buffer = new ByteArrayOutputStream();
        byte[] data = new byte[8192];
        int bytesRead;
        while ((bytesRead = is.read(data)) != -1) {
            buffer.write(data, 0, bytesRead);
        }
        return buffer.toByteArray();
    }

    @Override
    public void configureClient(ClientConfig.Builder config) {
        // No additional client configuration needed
    }
}
