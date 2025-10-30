
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Variables are key-value pairs used to store configuration values that can be referenced in contexts,
 * webhooks, and other parts of the system.
 */
@SmithyGenerated
public final class Variable implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Variable");
    private static final Variable $INSTANCE = new Variable();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING,
        "name", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("change_reason", PreludeSchemas.STRING,
        "description", PreludeSchemas.STRING,
        "created_at", SharedSchemas.DATE_TIME,
        "last_modified_by", PreludeSchemas.STRING,
        "value", PreludeSchemas.STRING,
        "created_by", PreludeSchemas.STRING,
        "last_modified_at", SharedSchemas.DATE_TIME);

    private static final List<Schema> $OPERATIONS = List.of(CreateVariable.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Variable instance() {
        return $INSTANCE;
    }

    private Variable() {}

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
        return GetVariable.$SCHEMA;
    }

    @Override
    public Schema update() {
        return UpdateVariable.$SCHEMA;
    }

    @Override
    public Schema delete() {
        return DeleteVariable.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListVariables.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

