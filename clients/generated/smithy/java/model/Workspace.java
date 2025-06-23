
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class Workspace implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#Workspace");
    private static final Workspace $INSTANCE = new Workspace();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of("org_id", PreludeSchemas.STRING,
        "workspace_name", PreludeSchemas.STRING);
    private static final Map<String, Schema> $PROPERTIES = Map.of("workspace_admin_email", PreludeSchemas.STRING,
        "created_at", SharedSchemas.DATE_TIME,
        "organisation_name", PreludeSchemas.STRING,
        "last_modified_by", PreludeSchemas.STRING,
        "created_by", PreludeSchemas.STRING,
        "config_version", PreludeSchemas.STRING,
        "mandatory_dimensions", SharedSchemas.LIST_MANDATORY_DIMENSIONS,
        "workspace_status", WorkspaceStatus.$SCHEMA,
        "last_modified_at", SharedSchemas.DATE_TIME,
        "organisation_id", PreludeSchemas.STRING,
        "allow_experiment_self_approval", PreludeSchemas.BOOLEAN,
        "workspace_schema_name", PreludeSchemas.STRING,
        "strict_mode", PreludeSchemas.BOOLEAN,
        "metrics", PreludeSchemas.DOCUMENT);

    private static final List<Schema> $OPERATIONS = List.of(CreateWorkspace.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static Workspace instance() {
        return $INSTANCE;
    }

    private Workspace() {}

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
        return UpdateWorkspace.$SCHEMA;
    }

    @Override
    public Schema list() {
        return ListWorkspace.$SCHEMA;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

