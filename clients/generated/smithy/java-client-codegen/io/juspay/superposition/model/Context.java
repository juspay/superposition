
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Context implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Context");
    private static final Context $INSTANCE = new Context();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("change_reason", PreludeSchemas.STRING,
        "override_id", PreludeSchemas.STRING,
        "weight", SharedSchemas.WEIGHT,
        "description", PreludeSchemas.STRING,
        "created_at", SharedSchemas.DATE_TIME,
        "id", PreludeSchemas.STRING,
        "override", SharedSchemas.OVERRIDES,
        "last_modified_by", PreludeSchemas.STRING,
        "value", SharedSchemas.CONDITION,
        "created_by", PreludeSchemas.STRING,
        "last_modified_at", SharedSchemas.DATE_TIME);

    private static final List<Schema> $OPERATIONS = List.of(MoveContext.$SCHEMA,
        UpdateOverride.$SCHEMA,
        GetContext.$SCHEMA,
        GetContextFromCondition.$SCHEMA,
        ListContexts.$SCHEMA,
        WeightRecompute.$SCHEMA,
        BulkOperation.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Context instance() {
        return $INSTANCE;
    }

    private Context() {}

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
    public Schema put() {
        return CreateContext.$SCHEMA;
    }

    @Override
    public Schema delete() {
        return DeleteContext.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

