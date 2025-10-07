
package io.juspay.superposition.model;

import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class ConfigVersion implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ConfigVersion");
    private static final ConfigVersion $INSTANCE = new ConfigVersion();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING,
        "id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("created_at", SharedSchemas.DATE_TIME,
        "description", PreludeSchemas.STRING,
        "config", PreludeSchemas.DOCUMENT,
        "config_hash", PreludeSchemas.STRING,
        "tags", SharedSchemas.STRING_LIST);

    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static ConfigVersion instance() {
        return $INSTANCE;
    }

    private ConfigVersion() {}

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
        return GetVersion.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListVersions.$SCHEMA;
    }

}

