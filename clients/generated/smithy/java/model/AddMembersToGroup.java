
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
 * Adds members to an existing experiment group.
 */
@SmithyGenerated
public final class AddMembersToGroup implements ApiOperation<AddMembersToGroupInput, AddMembersToGroupOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#AddMembersToGroup");

    private static final AddMembersToGroup $INSTANCE = new AddMembersToGroup();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PATCH").code(200).uri(UriPattern.parse("/experiment-groups/{id}/add-members")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(ResourceNotFound.$ID, ResourceNotFound.class, ResourceNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static AddMembersToGroup instance() {
        return $INSTANCE;
    }

    private AddMembersToGroup() {}

    @Override
    public ShapeBuilder<AddMembersToGroupInput> inputBuilder() {
        return AddMembersToGroupInput.builder();
    }

    @Override
    public ShapeBuilder<AddMembersToGroupOutput> outputBuilder() {
        return AddMembersToGroupOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return AddMembersToGroupInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return AddMembersToGroupOutput.$SCHEMA;
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
        return ExperimentGroup.instance();
    }
}

