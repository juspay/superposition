
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Function implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Function");
    private static final Function $INSTANCE = new Function();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING,
        "function_name", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("published_runtime_version", PreludeSchemas.STRING,
        "description", PreludeSchemas.STRING,
        "function_type", FunctionTypes.$SCHEMA,
        "last_modified_by", PreludeSchemas.STRING,
        "last_modified_at", SharedSchemas.DATE_TIME,
        "published_by", PreludeSchemas.STRING,
        "change_reason", PreludeSchemas.STRING,
        "draft_edited_by", PreludeSchemas.STRING,
        "draft_edited_at", SharedSchemas.DATE_TIME,
        "draft_code", PreludeSchemas.STRING,
        "draft_runtime_version", PreludeSchemas.STRING,
        "published_at", SharedSchemas.DATE_TIME,
        "published_code", PreludeSchemas.STRING);

    private static final List<Schema> $OPERATIONS = List.of(CreateFunction.$SCHEMA,
        Test.$SCHEMA,
        Publish.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Function instance() {
        return $INSTANCE;
    }

    private Function() {}

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
        return UpdateFunction.$SCHEMA;
    }

    @Override
    public Schema read() {
        return GetFunction.$SCHEMA;
    }

    @Override
    public Schema delete() {
        return DeleteFunction.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListFunction.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

