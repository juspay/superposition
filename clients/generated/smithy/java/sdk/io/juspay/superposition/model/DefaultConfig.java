
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class DefaultConfig implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DefaultConfig");
    private static final DefaultConfig $INSTANCE = new DefaultConfig();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING,
        "key", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("schema", PreludeSchemas.DOCUMENT,
        "autocomplete_function_name", PreludeSchemas.STRING,
        "change_reason", PreludeSchemas.STRING,
        "function_name", PreludeSchemas.STRING,
        "description", PreludeSchemas.STRING,
        "created_at", SharedSchemas.DATE_TIME,
        "last_modified_by", PreludeSchemas.STRING,
        "value", PreludeSchemas.DOCUMENT,
        "created_by", PreludeSchemas.STRING,
        "last_modified_at", SharedSchemas.DATE_TIME);

    private static final List<Schema> $OPERATIONS = List.of(CreateDefaultConfig.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static DefaultConfig instance() {
        return $INSTANCE;
    }

    private DefaultConfig() {}

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
        return UpdateDefaultConfig.$SCHEMA;
    }

    @Override
    public Schema delete() {
        return DeleteDefaultConfig.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListDefaultConfigs.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

