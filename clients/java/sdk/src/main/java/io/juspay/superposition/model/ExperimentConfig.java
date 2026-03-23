
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Represents a configuration of experiments that can be managed together.
 */
@SmithyGenerated
public final class ExperimentConfig implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ExperimentConfig");
    private static final ExperimentConfig $INSTANCE = new ExperimentConfig();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("experiment_groups", SharedSchemas.EXPERIMENT_GROUP_LIST,
        "experiments", SharedSchemas.EXPERIMENT_LIST,
        "last_modified", SharedSchemas.DATE_TIME);

    private static final List<Schema> $OPERATIONS = List.of(GetExperimentConfig.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static ExperimentConfig instance() {
        return $INSTANCE;
    }

    private ExperimentConfig() {}

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
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

