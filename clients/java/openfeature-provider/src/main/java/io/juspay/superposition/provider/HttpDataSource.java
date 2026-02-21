package io.juspay.superposition.provider;

import io.juspay.superposition.client.SuperpositionAsyncClient;
import io.juspay.superposition.model.GetConfigInput;
import io.juspay.superposition.model.GetConfigOutput;
import io.juspay.superposition.model.ListExperimentInput;
import io.juspay.superposition.model.ListExperimentOutput;
import io.juspay.superposition.model.ListExperimentGroupsInput;
import io.juspay.superposition.model.ListExperimentGroupsOutput;
import io.juspay.superposition.model.ExperimentResponse;
import io.juspay.superposition.model.ExperimentGroupResponse;
import io.juspay.superposition.model.ExperimentStatusType;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

/**
 * HTTP-based implementation of SuperpositionDataSource using the Superposition SDK client.
 */
public class HttpDataSource implements SuperpositionDataSource {

    private final SuperpositionAsyncClient client;
    private final String orgId;
    private final String workspaceId;

    /**
     * Enum for match types used in experiment filtering.
     */
    private enum MatchType {
        PARTIAL,
        EXACT
    }

    /**
     * Creates a new HttpDataSource.
     *
     * @param client the SuperpositionAsyncClient to use for HTTP calls
     * @param orgId the organization ID
     * @param workspaceId the workspace ID
     */
    public HttpDataSource(SuperpositionAsyncClient client, String orgId, String workspaceId) {
        this.client = client;
        this.orgId = orgId;
        this.workspaceId = workspaceId;
    }

    @Override
    public CompletableFuture<ConfigData> fetchConfig() {
        GetConfigInput input = GetConfigInput.builder()
            .workspaceId(workspaceId)
            .orgId(orgId)
            .build();

        return client.getConfig(input).thenApply(this::convertConfigOutput);
    }

    @Override
    public CompletableFuture<ConfigData> fetchFilteredConfig(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    ) {
        // For now, fetch full config and return (filtering can be added later)
        return fetchConfig();
    }

    @Override
    public CompletableFuture<Optional<ExperimentData>> fetchActiveExperiments() {
        ListExperimentInput experimentInput = ListExperimentInput.builder()
            .workspaceId(workspaceId)
            .orgId(orgId)
            .all(true)
            .status(List.of(ExperimentStatusType.CREATED, ExperimentStatusType.INPROGRESS))
            .build();

        ListExperimentGroupsInput groupsInput = ListExperimentGroupsInput.builder()
            .workspaceId(workspaceId)
            .orgId(orgId)
            .all(true)
            .build();

        CompletableFuture<ListExperimentOutput> experimentsFuture = client.listExperiment(experimentInput);
        CompletableFuture<ListExperimentGroupsOutput> groupsFuture = client.listExperimentGroups(groupsInput);

        return experimentsFuture.thenCombine(groupsFuture, (experiments, groups) -> {
            List<Map<String, Object>> experimentMaps = experiments.data().stream()
                .map(this::convertExperiment)
                .collect(Collectors.toList());

            List<Map<String, Object>> groupMaps = groups.data().stream()
                .map(this::convertExperimentGroup)
                .collect(Collectors.toList());

            return Optional.of(new ExperimentData(experimentMaps, groupMaps));
        });
    }

    @Override
    public CompletableFuture<Optional<ExperimentData>> fetchCandidateActiveExperiments(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    ) {
        return fetchActiveExperiments().thenApply(optData ->
            optData.map(data -> filterExperiments(data, context, prefixFilter, MatchType.PARTIAL))
        );
    }

    @Override
    public CompletableFuture<Optional<ExperimentData>> fetchMatchingActiveExperiments(
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter
    ) {
        return fetchActiveExperiments().thenApply(optData ->
            optData.map(data -> filterExperiments(data, context, prefixFilter, MatchType.EXACT))
        );
    }

    @Override
    public boolean supportsExperiments() {
        return true;
    }

    @Override
    public CompletableFuture<Void> close() {
        return CompletableFuture.completedFuture(null);
    }

    /**
     * Converts GetConfigOutput to ConfigData.
     *
     * @param output the SDK output
     * @return the provider ConfigData
     */
    private ConfigData convertConfigOutput(GetConfigOutput output) {
        Map<String, Object> defaultConfigs = new HashMap<>();
        if (output.defaultConfigs() != null) {
            output.defaultConfigs().forEach((k, v) -> defaultConfigs.put(k, convertDocument(v)));
        }

        List<Map<String, Object>> contexts = new ArrayList<>();
        if (output.contexts() != null) {
            output.contexts().forEach(ctx -> {
                Map<String, Object> ctxMap = new HashMap<>();
                ctxMap.put("id", ctx.id());
                if (ctx.condition() != null) {
                    Map<String, Object> conditionMap = new HashMap<>();
                    ctx.condition().forEach((k, v) -> conditionMap.put(k, convertDocument(v)));
                    ctxMap.put("condition", conditionMap);
                }
                if (ctx.overrideWithKeys() != null) {
                    ctxMap.put("override_with_keys", new ArrayList<>(ctx.overrideWithKeys()));
                }
                ctxMap.put("priority", ctx.priority());
                ctxMap.put("weight", ctx.weight());
                contexts.add(ctxMap);
            });
        }

        Map<String, Object> overrides = new HashMap<>();
        if (output.overrides() != null) {
            output.overrides().forEach((k, v) -> {
                Map<String, Object> innerMap = new HashMap<>();
                if (v != null) {
                    v.forEach((innerK, innerV) -> innerMap.put(innerK, convertDocument(innerV)));
                }
                overrides.put(k, innerMap);
            });
        }

        Map<String, Object> dimensions = new HashMap<>();
        if (output.dimensions() != null) {
            output.dimensions().forEach((k, v) -> {
                Map<String, Object> dimMap = new HashMap<>();
                if (v.schemaMember() != null) {
                    Map<String, Object> schemaMap = new HashMap<>();
                    v.schemaMember().forEach((sk, sv) -> schemaMap.put(sk, convertDocument(sv)));
                    dimMap.put("schema", schemaMap);
                }
                dimMap.put("position", v.position());
                if (v.dimensionType() != null) {
                    Object dimTypeValue = v.dimensionType().getValue();
                    dimMap.put("dimension_type", dimTypeValue != null ? dimTypeValue.toString() : v.dimensionType().type().toString());
                }
                if (v.dependencyGraph() != null) {
                    Map<String, List<String>> depGraph = new HashMap<>();
                    v.dependencyGraph().forEach((dk, dv) -> depGraph.put(dk, new ArrayList<>(dv)));
                    dimMap.put("dependency_graph", depGraph);
                }
                if (v.valueComputeFunctionName() != null) {
                    dimMap.put("value_compute_function_name", v.valueComputeFunctionName());
                }
                dimensions.put(k, dimMap);
            });
        }

        return new ConfigData(defaultConfigs, contexts, overrides, dimensions);
    }

    /**
     * Converts a Document to a plain Java Object.
     *
     * @param doc the Document to convert
     * @return the converted Object
     */
    @SuppressWarnings("unchecked")
    private Object convertDocument(software.amazon.smithy.java.core.serde.document.Document doc) {
        if (doc == null) {
            return null;
        }
        // Try to extract value using toString and pattern matching
        String str = doc.toString();
        
        // Try to parse as different types
        if ("null".equals(str)) {
            return null;
        }
        if ("true".equals(str)) {
            return Boolean.TRUE;
        }
        if ("false".equals(str)) {
            return Boolean.FALSE;
        }
        
        // Try integer
        try {
            return Integer.parseInt(str);
        } catch (NumberFormatException e) {
            // Not an integer
        }
        
        // Try long
        try {
            return Long.parseLong(str);
        } catch (NumberFormatException e) {
            // Not a long
        }
        
        // Try double
        try {
            return Double.parseDouble(str);
        } catch (NumberFormatException e) {
            // Not a double
        }
        
        // Return as string
        return str;
    }

    /**
     * Converts an ExperimentResponse to a Map.
     *
     * @param experiment the experiment response
     * @return the experiment as a Map
     */
    private Map<String, Object> convertExperiment(ExperimentResponse experiment) {
        Map<String, Object> map = new HashMap<>();
        map.put("id", experiment.id());
        map.put("name", experiment.name());
        map.put("status", experiment.status().value());

        if (experiment.context() != null) {
            Map<String, Object> contextMap = new HashMap<>();
            experiment.context().forEach((k, v) -> contextMap.put(k, convertDocument(v)));
            map.put("context", contextMap);
        }

        if (experiment.variants() != null) {
            List<Map<String, Object>> variants = new ArrayList<>();
            experiment.variants().forEach(variant -> {
                Map<String, Object> variantMap = new HashMap<>();
                variantMap.put("id", variant.id());
                if (variant.variantType() != null) {
                    variantMap.put("variant_type", variant.variantType().value());
                }
                if (variant.contextId() != null) {
                    variantMap.put("context_id", variant.contextId());
                }
                if (variant.overrideId() != null) {
                    variantMap.put("override_id", variant.overrideId());
                }
                if (variant.overrides() != null) {
                    Map<String, Object> overrideMap = new HashMap<>();
                    variant.overrides().forEach((k, v) -> overrideMap.put(k, convertDocument(v)));
                    variantMap.put("overrides", overrideMap);
                }
                variants.add(variantMap);
            });
            map.put("variants", variants);
        }

        map.put("traffic_percentage", experiment.trafficPercentage());
        map.put("experiment_type", experiment.experimentType().value());

        if (experiment.overrideKeys() != null) {
            map.put("override_keys", new ArrayList<>(experiment.overrideKeys()));
        }

        if (experiment.experimentGroupId() != null) {
            map.put("experiment_group_id", experiment.experimentGroupId());
        }

        return map;
    }

    /**
     * Converts an ExperimentGroupResponse to a Map.
     *
     * @param group the experiment group response
     * @return the experiment group as a Map
     */
    private Map<String, Object> convertExperimentGroup(ExperimentGroupResponse group) {
        Map<String, Object> map = new HashMap<>();
        map.put("id", group.id());

        if (group.context() != null) {
            Map<String, Object> contextMap = new HashMap<>();
            group.context().forEach((k, v) -> contextMap.put(k, convertDocument(v)));
            map.put("context", contextMap);
        }

        if (group.memberExperimentIds() != null) {
            map.put("member_experiment_ids", new ArrayList<>(group.memberExperimentIds()));
        }

        map.put("traffic_percentage", group.trafficPercentage());
        map.put("group_type", group.groupType().value());

        return map;
    }

    /**
     * Filters experiments and groups based on context and prefix.
     *
     * @param data the experiment data to filter
     * @param context optional context for filtering
     * @param prefixFilter optional prefix filter
     * @param matchType the type of matching to apply
     * @return the filtered ExperimentData
     */
    private ExperimentData filterExperiments(
        ExperimentData data,
        Optional<Map<String, Object>> context,
        Optional<List<String>> prefixFilter,
        MatchType matchType
    ) {
        List<Map<String, Object>> filteredExperiments = data.getExperiments();
        List<Map<String, Object>> filteredGroups = data.getExperimentGroups();

        if (context.isPresent()) {
            filteredExperiments = filteredExperiments.stream()
                .filter(exp -> matchesContext(exp, context.get(), matchType))
                .collect(Collectors.toList());
            filteredGroups = filteredGroups.stream()
                .filter(group -> matchesContext(group, context.get(), matchType))
                .collect(Collectors.toList());
        }

        if (prefixFilter.isPresent()) {
            filteredExperiments = filteredExperiments.stream()
                .filter(exp -> matchesPrefix(exp, prefixFilter.get()))
                .collect(Collectors.toList());
        }

        return new ExperimentData(filteredExperiments, filteredGroups);
    }

    /**
     * Checks if an experiment matches the given context.
     *
     * @param experiment the experiment map
     * @param context the context to match against
     * @param matchType the type of matching
     * @return true if the context matches
     */
    @SuppressWarnings("unchecked")
    private boolean matchesContext(
        Map<String, Object> experiment,
        Map<String, Object> context,
        MatchType matchType
    ) {
        Object expContextObj = experiment.get("context");
        if (!(expContextObj instanceof Map)) {
            return true; // No context to match, consider it a match
        }

        Map<String, Object> expContext = (Map<String, Object>) expContextObj;

        if (matchType == MatchType.EXACT) {
            return expContext.equals(context);
        } else {
            // PARTIAL match - context is a subset of expContext
            for (Map.Entry<String, Object> entry : context.entrySet()) {
                if (!expContext.containsKey(entry.getKey()) ||
                    !expContext.get(entry.getKey()).equals(entry.getValue())) {
                    return false;
                }
            }
            return true;
        }
    }

    /**
     * Checks if an experiment matches any of the given prefixes.
     *
     * @param experiment the experiment map
     * @param prefixes the list of prefixes to match
     * @return true if the experiment matches any prefix
     */
    @SuppressWarnings("unchecked")
    private boolean matchesPrefix(Map<String, Object> experiment, List<String> prefixes) {
        if (prefixes == null || prefixes.isEmpty()) {
            return true;
        }

        String experimentId = (String) experiment.get("id");
        String experimentName = (String) experiment.get("name");

        Object overrideKeysObj = experiment.get("override_keys");
        List<String> overrideKeys = overrideKeysObj instanceof List ?
            (List<String>) overrideKeysObj : Collections.emptyList();

        for (String prefix : prefixes) {
            if (prefix == null || prefix.isEmpty()) {
                continue;
            }

            if (experimentId != null && experimentId.startsWith(prefix)) {
                return true;
            }
            if (experimentName != null && experimentName.startsWith(prefix)) {
                return true;
            }
            for (String key : overrideKeys) {
                if (key != null && key.startsWith(prefix)) {
                    return true;
                }
            }
        }

        return false;
    }
}
