
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
 * Updates the condition of the mentioned context, if a context with the new condition already exists,
 * it merges the override and effectively deleting the old context
 */
@SmithyGenerated
public final class MoveContext implements ApiOperation<MoveContextInput, MoveContextOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#MoveContext");

    private static final MoveContext $INSTANCE = new MoveContext();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PUT").code(200).uri(UriPattern.parse("/context/move/{id}")).build());

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
    public static MoveContext instance() {
        return $INSTANCE;
    }

    private MoveContext() {}

    @Override
    public ShapeBuilder<MoveContextInput> inputBuilder() {
        return MoveContextInput.builder();
    }

    @Override
    public ShapeBuilder<MoveContextOutput> outputBuilder() {
        return MoveContextOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return MoveContextInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return MoveContextOutput.$SCHEMA;
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
        return Context.instance();
    }
}

