
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Dimension implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Dimension");
    private static final Dimension $INSTANCE = new Dimension();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING,
        "dimension", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.ofEntries(
        Map.entry("schema", SharedSchemas.OBJECT),
        Map.entry("autocomplete_function_name", PreludeSchemas.STRING),
        Map.entry("change_reason", PreludeSchemas.STRING),
        Map.entry("dimension_type", DimensionType.$SCHEMA),
        Map.entry("function_name", PreludeSchemas.STRING),
        Map.entry("description", PreludeSchemas.STRING),
        Map.entry("created_at", SharedSchemas.DATE_TIME),
        Map.entry("position", PreludeSchemas.INTEGER),
        Map.entry("last_modified_by", PreludeSchemas.STRING),
        Map.entry("created_by", PreludeSchemas.STRING),
        Map.entry("dependency_graph", SharedSchemas.DEPEDENDENCY_GRAPH),
        Map.entry("last_modified_at", SharedSchemas.DATE_TIME));

    private static final List<Schema> $OPERATIONS = List.of(GetDimension.$SCHEMA,
        CreateDimension.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Dimension instance() {
        return $INSTANCE;
    }

    private Dimension() {}

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
        return UpdateDimension.$SCHEMA;
    }

    @Override
    public Schema delete() {
        return DeleteDimension.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListDimensions.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

