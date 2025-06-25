
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Webhook implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Webhook");
    private static final Webhook $INSTANCE = new Webhook();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("workspace_id", PreludeSchemas.STRING,
        "org_id", PreludeSchemas.STRING,
        "name", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.ofEntries(
        Map.entry("method", HttpMethod.$SCHEMA),
        Map.entry("max_retries", PreludeSchemas.INTEGER),
        Map.entry("description", PreludeSchemas.STRING),
        Map.entry("created_at", SharedSchemas.DATE_TIME),
        Map.entry("last_modified_by", PreludeSchemas.STRING),
        Map.entry("version", Version.$SCHEMA),
        Map.entry("created_by", PreludeSchemas.STRING),
        Map.entry("enabled", PreludeSchemas.BOOLEAN),
        Map.entry("url", PreludeSchemas.STRING),
        Map.entry("last_modified_at", SharedSchemas.DATE_TIME),
        Map.entry("change_reason", PreludeSchemas.STRING),
        Map.entry("last_triggered_at", SharedSchemas.DATE_TIME),
        Map.entry("events", SharedSchemas.EVENTS),
        Map.entry("custom_headers", SharedSchemas.OBJECT));

    private static final List<Schema> $OPERATIONS = List.of(CreateWebhook.$SCHEMA,
        GetWebhook.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Webhook instance() {
        return $INSTANCE;
    }

    private Webhook() {}

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
        return UpdateWebhook.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListWebhook.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

