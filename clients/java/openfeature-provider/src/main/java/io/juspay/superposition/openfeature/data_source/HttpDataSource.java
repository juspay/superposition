package io.juspay.superposition.openfeature.data_source;

import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.client.auth.BearerTokenIdentityResolver;
import io.juspay.superposition.model.DimensionMatchStrategy;
import io.juspay.superposition.model.GetConfigInput;
import io.juspay.superposition.model.GetConfigOutput;
import io.juspay.superposition.model.GetExperimentConfigInput;
import io.juspay.superposition.model.GetExperimentConfigOutput;
import io.juspay.superposition.openfeature.FfiUtils;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import io.juspay.superposition.openfeature.options.SuperpositionOptions;
import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver;
import software.amazon.smithy.java.client.core.interceptors.ClientInterceptor;
import software.amazon.smithy.java.client.core.interceptors.RequestHook;
import software.amazon.smithy.java.client.core.interceptors.ResponseHook;
import software.amazon.smithy.java.core.error.CallException;
import software.amazon.smithy.java.core.error.ErrorFault;
import software.amazon.smithy.java.http.api.HttpRequest;
import software.amazon.smithy.java.http.api.HttpResponse;
import software.amazon.smithy.java.retries.api.RetrySafety;

/**
 * HTTP-based implementation of SuperpositionDataSource.
 *
 * Fetches configuration and experiment data from the Superposition HTTP API
 * using the Smithy-generated SDK client.
 *
 * Supports:
 * - Conditional fetching via if-modified-since timestamps (HTTP 304)
 * - Context and prefix filtering
 * - Exact and subset dimension matching strategies for experiments
 */
public class HttpDataSource implements SuperpositionDataSource {

    private static final Logger log = LoggerFactory.getLogger(HttpDataSource.class);

    private final SuperpositionOptions options;
    private final SuperpositionAsyncClient sdk;

    /**
     * Create a new HTTP data source.
     *
     * @param options connection options (endpoint, token, org_id, workspace_id)
     */
    public HttpDataSource(SuperpositionOptions options) {
        options.validate();
        this.options = options;

        this.sdk = SuperpositionAsyncClient.builder()
            .endpointResolver(EndpointResolver.staticEndpoint(options.getEndpoint()))
            .addIdentityResolver(new BearerTokenIdentityResolver(options.getToken()))
            .addInterceptor(new NotModifiedInterceptor())
            .addInterceptor(new PrefixQueryInterceptor())
            .build();

        log.debug("HttpDataSource initialized for endpoint: {}", options.getEndpoint());
    }

    @Override
    public FetchResponse<ConfigData> fetchFilteredConfig(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince)
            throws SuperpositionError {
        log.debug("Fetching config from Superposition service using SDK");

        GetConfigInput.Builder inputBuilder = GetConfigInput.builder()
            .orgId(options.getOrgId())
            .workspaceId(options.getWorkspaceId());

        context.filter(c -> !c.isEmpty())
            .ifPresent(c -> inputBuilder.context(FfiUtils.contextToDocuments(c)));
        prefixFilter.filter(p -> !p.isEmpty()).ifPresent(inputBuilder::prefix);
        ifModifiedSince.ifPresent(inputBuilder::ifModifiedSince);

        try {
            // get(), not join(): join() ignores interrupts, so a refresh past its timeout could
            // not be cancelled and would wedge the single refresh worker behind a dead request.
            GetConfigOutput output = sdk.getConfig(inputBuilder.build()).get();
            ConfigData configData =
                new ConfigData(FfiUtils.fromGetConfigOutput(output), output.lastModified());
            log.debug("Successfully fetched {}", configData);
            return FetchResponse.data(configData);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw SuperpositionError.networkError("Interrupted while fetching config", e);
        } catch (Exception e) {
            if (isNotModified(e)) {
                return FetchResponse.notModified();
            }
            throw SuperpositionError.networkError("Failed to fetch config: " + e.getMessage(), e);
        }
    }

    @Override
    public FetchResponse<ExperimentData> fetchActiveExperiments(Optional<Instant> ifModifiedSince)
            throws SuperpositionError {
        return fetchExperimentsWithFilters(
            Optional.empty(), Optional.empty(), ifModifiedSince, Optional.empty());
    }

    @Override
    public FetchResponse<ExperimentData> fetchCandidateActiveExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince)
            throws SuperpositionError {
        return fetchExperimentsWithFilters(
            context, prefixFilter, ifModifiedSince, Optional.of(DimensionMatchStrategy.EXACT));
    }

    @Override
    public FetchResponse<ExperimentData> fetchMatchingActiveExperiments(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince)
            throws SuperpositionError {
        return fetchExperimentsWithFilters(
            context, prefixFilter, ifModifiedSince, Optional.of(DimensionMatchStrategy.SUBSET));
    }

    @Override
    public boolean supportsExperiments() {
        return true;
    }

    @Override
    public void close() {
        // SDK client doesn't require explicit close in the async variant
        log.debug("HttpDataSource closed");
    }

    // ============= Private helpers =============

    private FetchResponse<ExperimentData> fetchExperimentsWithFilters(
            Optional<Map<String, String>> context,
            Optional<List<String>> prefixFilter,
            Optional<Instant> ifModifiedSince,
            Optional<DimensionMatchStrategy> matchStrategy)
            throws SuperpositionError {
        log.debug("Fetching experiments from Superposition service using SDK");

        GetExperimentConfigInput.Builder inputBuilder = GetExperimentConfigInput.builder()
            .orgId(options.getOrgId())
            .workspaceId(options.getWorkspaceId());

        context.filter(c -> !c.isEmpty())
            .ifPresent(c -> inputBuilder.context(FfiUtils.contextToDocuments(c)));
        prefixFilter.filter(p -> !p.isEmpty()).ifPresent(inputBuilder::prefix);
        ifModifiedSince.ifPresent(inputBuilder::ifModifiedSince);
        matchStrategy.ifPresent(inputBuilder::dimensionMatchStrategy);

        try {
            // get(), not join() — see fetchFilteredConfig.
            GetExperimentConfigOutput output = sdk.getExperimentConfig(inputBuilder.build()).get();
            ExperimentData experimentData = new ExperimentData(
                FfiUtils.fromGetExperimentConfigOutput(output), output.lastModified());
            log.debug("Successfully fetched {}", experimentData);
            return FetchResponse.data(experimentData);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw SuperpositionError.networkError("Interrupted while fetching experiments", e);
        } catch (Exception e) {
            if (isNotModified(e)) {
                return FetchResponse.notModified();
            }
            throw SuperpositionError.networkError(
                "Failed to list experiments: " + e.getMessage(), e);
        }
    }

    /**
     * 304 surfaces as {@link NotModifiedException} thrown by {@link NotModifiedInterceptor};
     * {@code CompletableFuture} wraps it in an {@code ExecutionException}, so walk the cause chain.
     */
    private static boolean isNotModified(Throwable e) {
        for (Throwable t = e; t != null; t = t.getCause()) {
            if (t instanceof NotModifiedException) {
                return true;
            }
            if (t.getCause() == t) {
                break;
            }
        }
        return false;
    }

    /**
     * Signals an HTTP 304. Extends {@link CallException} so the client pipeline treats it as a
     * terminal client-side outcome rather than something to retry.
     */
    private static final class NotModifiedException extends CallException {
        NotModifiedException() {
            super("Not modified", ErrorFault.CLIENT);
            isRetrySafe(RetrySafety.NO);
        }
    }

    /**
     * The Smithy Java SDK has no modeled shape for 304, so the response would otherwise surface as
     * a generic error. Inspecting the raw status is the only way to tell a real failure from
     * "your cached copy is still current".
     */
    private static final class NotModifiedInterceptor implements ClientInterceptor {
        @Override
        public void readAfterTransmit(ResponseHook<?, ?, ?, ?> hook) {
            if (hook.response() instanceof HttpResponse response && response.statusCode() == 304) {
                throw new NotModifiedException();
            }
        }
    }

    /**
     * The generated SDK never serializes list-typed {@code @httpQuery} members, so a {@code prefix}
     * set on the input silently never reaches the server and the response comes back unfiltered.
     * Until that is fixed in the SDK, put it on the wire here — comma-joined, which is how the
     * service parses it.
     */
    private static final class PrefixQueryInterceptor implements ClientInterceptor {
        @Override
        public <RequestT> RequestT modifyBeforeTransmit(RequestHook<?, ?, RequestT> hook) {
            List<String> prefix = prefixOf(hook.input());
            if (prefix.isEmpty()) {
                return hook.request();
            }
            return hook.mapRequest(HttpRequest.class, h -> {
                URI uri = h.request().uri();
                String param = "prefix="
                    + URLEncoder.encode(String.join(",", prefix), StandardCharsets.UTF_8);
                String query = uri.getRawQuery() == null ? param : uri.getRawQuery() + "&" + param;
                return h.request().toBuilder().uri(withQuery(uri, query)).build();
            });
        }

        private static List<String> prefixOf(Object input) {
            if (input instanceof GetConfigInput config) {
                return config.prefix();
            }
            if (input instanceof GetExperimentConfigInput experiments) {
                return experiments.prefix();
            }
            return List.of();
        }

        private static URI withQuery(URI uri, String rawQuery) {
            String rebuilt = uri.getScheme() + "://" + uri.getRawAuthority() +
                uri.getRawPath() + '?' + rawQuery;
            return URI.create(rebuilt);
        }
    }
}
