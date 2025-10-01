
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Config implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Config");
    private static final Config $INSTANCE = new Config();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("version", PreludeSchemas.STRING,
        "config", PreludeSchemas.DOCUMENT,
        "last_modified", SharedSchemas.DATE_TIME);

    private static final List<Schema> $OPERATIONS = List.of(GetConfigFast.$SCHEMA,
        GetConfig.$SCHEMA,
        GetResolvedConfig.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Config instance() {
        return $INSTANCE;
    }

    private Config() {}

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

