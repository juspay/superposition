
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
    private static final Map<String, Schema> $PROPERTIES = Map.of("chosen_variant", PreludeSchemas.STRING,
        "created_at", SharedSchemas.DATE_TIME,
        "description", PreludeSchemas.STRING,
        "variants", SharedSchemas.LIST_VARIANT,
        "last_modified_by", PreludeSchemas.STRING,
        "override_keys", SharedSchemas.LIST_OVERRIDE_KEYS,
        "created_by", PreludeSchemas.STRING,
        "experiment_type", ExperimentType.$SCHEMA,
        "change_reason", PreludeSchemas.STRING,
        "metrics_url", PreludeSchemas.STRING,
        "traffic_percentage", PreludeSchemas.INTEGER,
        "name", PreludeSchemas.STRING,
        "context", SharedSchemas.CONDITION,
        "started_at", SharedSchemas.DATE_TIME,
        "id", PreludeSchemas.STRING,
        "metrics", PreludeSchemas.DOCUMENT,
        "last_modified", SharedSchemas.DATE_TIME,
        "started_by", PreludeSchemas.STRING,
        "status", ExperimentStatusType.$SCHEMA);

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

