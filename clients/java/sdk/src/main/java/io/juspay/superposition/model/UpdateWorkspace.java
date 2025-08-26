
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

/**
 * Updates an existing workspace configuration, allowing modification of admin settings, mandatory
 * dimensions, and workspace properties. Validates config version existence if provided.
 */
@SmithyGenerated
public final class UpdateWorkspace implements ApiOperation<UpdateWorkspaceInput, UpdateWorkspaceOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateWorkspace");

    private static final UpdateWorkspace $INSTANCE = new UpdateWorkspace();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PUT").code(200).uri(UriPattern.parse("/workspaces/{workspace_name}")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(WorkspaceNotFound.$ID, WorkspaceNotFound.class, WorkspaceNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static UpdateWorkspace instance() {
        return $INSTANCE;
    }

    private UpdateWorkspace() {}

    @Override
    public ShapeBuilder<UpdateWorkspaceInput> inputBuilder() {
        return UpdateWorkspaceInput.builder();
    }

    @Override
    public ShapeBuilder<UpdateWorkspaceOutput> outputBuilder() {
        return UpdateWorkspaceOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return UpdateWorkspaceInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return UpdateWorkspaceOutput.$SCHEMA;
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

