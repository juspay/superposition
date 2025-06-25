
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Experiments implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Experiments");
    private static final Experiments $INSTANCE = new Experiments();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.ofEntries(
        Map.entry("chosen_variant", PreludeSchemas.STRING),
        Map.entry("created_at", SharedSchemas.DATE_TIME),
        Map.entry("description", PreludeSchemas.STRING),
        Map.entry("variants", SharedSchemas.LIST_VARIANT),
        Map.entry("last_modified_by", PreludeSchemas.STRING),
        Map.entry("override_keys", SharedSchemas.LIST_OVERRIDE_KEYS),
        Map.entry("created_by", PreludeSchemas.STRING),
        Map.entry("experiment_type", ExperimentType.$SCHEMA),
        Map.entry("change_reason", PreludeSchemas.STRING),
        Map.entry("metrics_url", PreludeSchemas.STRING),
        Map.entry("traffic_percentage", PreludeSchemas.INTEGER),
        Map.entry("name", PreludeSchemas.STRING),
        Map.entry("context", SharedSchemas.CONDITION),
        Map.entry("started_at", SharedSchemas.DATE_TIME),
        Map.entry("id", PreludeSchemas.STRING),
        Map.entry("metrics", PreludeSchemas.DOCUMENT),
        Map.entry("last_modified", SharedSchemas.DATE_TIME),
        Map.entry("started_by", PreludeSchemas.STRING),
        Map.entry("status", ExperimentStatusType.$SCHEMA));

    private static final List<Schema> $OPERATIONS = List.of(ListExperiment.$SCHEMA,
        CreateExperiment.$SCHEMA,
        ConcludeExperiment.$SCHEMA,
        DiscardExperiment.$SCHEMA,
        RampExperiment.$SCHEMA,
        UpdateOverridesExperiment.$SCHEMA,
        ApplicableVariants.$SCHEMA,
        PauseExperiment.$SCHEMA,
        ResumeExperiment.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Experiments instance() {
        return $INSTANCE;
    }

    private Experiments() {}

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Map<String, Schema> identifiers() {
        return $IDENTIFIERS;
    }

    @Override
    public Map<String, Schema> properties() {
        return $PROPERTIES;
    }

    @Override
    public Schema read() {
        return GetExperiment.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}
