
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class TypeTemplates implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#TypeTemplates");
    private static final TypeTemplates $INSTANCE = new TypeTemplates();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "type_name", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("change_reason", PreludeSchemas.STRING,
        "type_schema", PreludeSchemas.DOCUMENT,
        "description", PreludeSchemas.STRING,
        "created_at", SharedSchemas.DATE_TIME,
        "last_modified_by", PreludeSchemas.STRING,
        "created_by", PreludeSchemas.STRING,
        "last_modified_at", SharedSchemas.DATE_TIME);

    private static final List<Schema> $OPERATIONS = List.of(CreateTypeTemplates.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static TypeTemplates instance() {
        return $INSTANCE;
    }

    private TypeTemplates() {}

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
        return UpdateTypeTemplates.$SCHEMA;
    }

    @Override
    public Schema delete() {
        return DeleteTypeTemplates.$SCHEMA;
    }

    @Override
    public Schema list() {
        return GetTypeTemplatesList.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

