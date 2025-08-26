
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
 * Permanently removes a dimension from the workspace. This operation will fail if the dimension has
 * active dependencies or is referenced by existing configurations.
 */
@SmithyGenerated
public final class DeleteDimension implements ApiOperation<DeleteDimensionInput, DeleteDimensionOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DeleteDimension");

    private static final DeleteDimension $INSTANCE = new DeleteDimension();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("DELETE").code(201).uri(UriPattern.parse("/dimension/{dimension}")).build());

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
    public static DeleteDimension instance() {
        return $INSTANCE;
    }

    private DeleteDimension() {}

    @Override
    public ShapeBuilder<DeleteDimensionInput> inputBuilder() {
        return DeleteDimensionInput.builder();
    }

    @Override
    public ShapeBuilder<DeleteDimensionOutput> outputBuilder() {
        return DeleteDimensionOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return DeleteDimensionInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return DeleteDimensionOutput.$SCHEMA;
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
        return Dimension.instance();
    }
}

