
package io.juspay.superposition.model;

import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Organisation implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Organisation");
    private static final Organisation $INSTANCE = new Organisation();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("id", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.ofEntries(
        Map.entry("country_code", PreludeSchemas.STRING),
        Map.entry("contact_phone", PreludeSchemas.STRING),
        Map.entry("updated_at", SharedSchemas.DATE_TIME),
        Map.entry("name", PreludeSchemas.STRING),
        Map.entry("updated_by", PreludeSchemas.STRING),
        Map.entry("created_at", SharedSchemas.DATE_TIME),
        Map.entry("created_by", PreludeSchemas.STRING),
        Map.entry("sector", PreludeSchemas.STRING),
        Map.entry("admin_email", PreludeSchemas.STRING),
        Map.entry("contact_email", PreludeSchemas.STRING),
        Map.entry("status", OrgStatus.$SCHEMA));

    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Organisation instance() {
        return $INSTANCE;
    }

    private Organisation() {}

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
    public Schema create() {
        return CreateOrganisation.$SCHEMA;
    }

    @Override
    public Schema put() {
        return UpdateOrganisation.$SCHEMA;
    }

    @Override
    public Schema read() {
        return GetOrganisation.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListOrganisation.$SCHEMA;
    }

}

