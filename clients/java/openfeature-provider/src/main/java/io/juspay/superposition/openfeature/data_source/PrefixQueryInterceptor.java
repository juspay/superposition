package io.juspay.superposition.openfeature.data_source;

import io.juspay.superposition.model.ApplicableVariantsInput;
import io.juspay.superposition.model.GetConfigInput;
import io.juspay.superposition.model.GetExperimentConfigInput;
import io.juspay.superposition.model.GetResolvedConfigWithIdentifierInput;
import java.net.URI;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.List;
import software.amazon.smithy.java.client.core.interceptors.ClientInterceptor;
import software.amazon.smithy.java.client.core.interceptors.RequestHook;
import software.amazon.smithy.java.http.api.HttpRequest;

/**
 * Serializes the list-typed {@code @httpQuery} members {@code prefix} and {@code exclude_prefix} onto
 * the request URI, because the generated smithy-java SDK does not (verified by
 * {@code SdkListQuerySerializationTest}). Without this they silently never reach the server and the
 * response comes back unfiltered. Values are comma-joined, which is how the service parses them.
 *
 * <p>Register this on <em>every</em> client that sends a request carrying these filters: the HTTP
 * data source (config + experiment fetches) and the API provider (resolved-config +
 * applicable-variants). If a new command with a {@code prefix} / {@code exclude_prefix} query member
 * is added, extend {@link #prefixOf} / {@link #excludePrefixOf} to cover its input type.
 * 
 * TODO: Remove this once the smithy-java SDK is fixed to serialize list-typed query members.
 */
public final class PrefixQueryInterceptor implements ClientInterceptor {

    @Override
    public <RequestT> RequestT modifyBeforeTransmit(RequestHook<?, ?, RequestT> hook) {
        List<String> prefix = prefixOf(hook.input());
        List<String> excludePrefix = excludePrefixOf(hook.input());
        if (prefix.isEmpty() && excludePrefix.isEmpty()) {
            return hook.request();
        }
        return hook.mapRequest(HttpRequest.class, h -> {
            URI uri = h.request().uri();
            String query = uri.getRawQuery();
            query = appendParam(query, "prefix", prefix);
            query = appendParam(query, "exclude_prefix", excludePrefix);
            return h.request().toBuilder().uri(withQuery(uri, query)).build();
        });
    }

    private static String appendParam(String query, String name, List<String> values) {
        if (values.isEmpty()) {
            return query;
        }
        String param = name + "="
            + URLEncoder.encode(String.join(",", values), StandardCharsets.UTF_8);
        return query == null ? param : query + "&" + param;
    }

    private static List<String> prefixOf(Object input) {
        if (input instanceof GetConfigInput config) {
            return config.prefix();
        }
        if (input instanceof GetExperimentConfigInput experiments) {
            return experiments.prefix();
        }
        if (input instanceof GetResolvedConfigWithIdentifierInput resolved) {
            return resolved.prefix();
        }
        if (input instanceof ApplicableVariantsInput variants) {
            return variants.prefix();
        }
        return List.of();
    }

    private static List<String> excludePrefixOf(Object input) {
        if (input instanceof GetConfigInput config) {
            return config.excludePrefix();
        }
        if (input instanceof GetExperimentConfigInput experiments) {
            return experiments.excludePrefix();
        }
        if (input instanceof GetResolvedConfigWithIdentifierInput resolved) {
            return resolved.excludePrefix();
        }
        if (input instanceof ApplicableVariantsInput variants) {
            return variants.excludePrefix();
        }
        return List.of();
    }

    private static URI withQuery(URI uri, String rawQuery) {
        String rebuilt = uri.getScheme() + "://" + uri.getRawAuthority()
            + uri.getRawPath() + '?' + rawQuery;
        return URI.create(rebuilt);
    }
}
