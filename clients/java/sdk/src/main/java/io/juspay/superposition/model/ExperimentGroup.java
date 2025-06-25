
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Represents a group of experiments that can be managed together.
 */
@SmithyGenerated
public final class ExperimentGroup implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ExperimentGroup");
    private static final ExperimentGroup $INSTANCE = new ExperimentGroup();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.ofEntries(
        Map.entry("change_reason", PreludeSchemas.STRING),
        Map.entry("context_hash", PreludeSchemas.STRING),
        Map.entry("traffic_percentage", PreludeSchemas.INTEGER),
        Map.entry("name", PreludeSchemas.STRING),
        Map.entry("context", SharedSchemas.CONDITION),
        Map.entry("description", PreludeSchemas.STRING),
        Map.entry("member_experiment_ids", SharedSchemas.STRING_LIST),
        Map.entry("created_at", SharedSchemas.DATE_TIME),
        Map.entry("id", PreludeSchemas.STRING),
        Map.entry("last_modified_by", PreludeSchemas.STRING),
        Map.entry("created_by", PreludeSchemas.STRING),
        Map.entry("last_modified_at", SharedSchemas.DATE_TIME));

    private static final List<Schema> $OPERATIONS = List.of(ListExperimentGroups.$SCHEMA,
        CreateExperimentGroup.$SCHEMA,
        GetExperimentGroup.$SCHEMA,
        UpdateExperimentGroup.$SCHEMA,
        DeleteExperimentGroup.$SCHEMA,
        AddMembersToGroup.$SCHEMA,
        RemoveMembersFromGroup.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static ExperimentGroup instance() {
        return $INSTANCE;
    }

    private ExperimentGroup() {}

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

