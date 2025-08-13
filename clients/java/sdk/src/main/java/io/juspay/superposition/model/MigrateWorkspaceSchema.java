
package io.juspay.superposition.model;

import java.util.List;
import software.amazon.smithy.java.core.schema.ApiOperation;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.TypeRegistry;
import software.amazon.smithy.model.pattern.UriPattern;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class MigrateWorkspaceSchema implements ApiOperation<MigrateWorkspaceSchemaInput, MigrateWorkspaceSchemaOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#MigrateWorkspaceSchema");

    private static final MigrateWorkspaceSchema $INSTANCE = new MigrateWorkspaceSchema();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/workspaces/{workspace_name}/db/migrate")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static MigrateWorkspaceSchema instance() {
        return $INSTANCE;
    }

    private MigrateWorkspaceSchema() {}

    @Override
    public ShapeBuilder<MigrateWorkspaceSchemaInput> inputBuilder() {
        return MigrateWorkspaceSchemaInput.builder();
    }

    @Override
    public ShapeBuilder<MigrateWorkspaceSchemaOutput> outputBuilder() {
        return MigrateWorkspaceSchemaOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return MigrateWorkspaceSchemaInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return MigrateWorkspaceSchemaOutput.$SCHEMA;
    }

    @Override
    public TypeRegistry errorRegistry() {
        return TYPE_REGISTRY;
    }

    @Override
    public List<ShapeId> effectiveAuthSchemes() {
        return SCHEMES;
    }

    @Override
    public Schema inputStreamMember() {
        return null;
    }

    @Override
    public Schema outputStreamMember() {
        return null;
    }

    @Override
    public Schema idempotencyTokenMember() {
        return null;
    }

    @Override
    public ApiResource boundResource() {
        return Workspace.instance();
    }
}

