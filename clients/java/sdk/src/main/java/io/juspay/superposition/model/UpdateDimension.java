
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
 * Updates an existing dimension's configuration. Allows modification of schema, position, function
 * mappings, and other properties while maintaining dependency relationships.
 */
@SmithyGenerated
public final class UpdateDimension implements ApiOperation<UpdateDimensionInput, UpdateDimensionOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateDimension");

    private static final UpdateDimension $INSTANCE = new UpdateDimension();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PUT").code(200).uri(UriPattern.parse("/dimension/{dimension}")).build());

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
    public static UpdateDimension instance() {
        return $INSTANCE;
    }

    private UpdateDimension() {}

    @Override
    public ShapeBuilder<UpdateDimensionInput> inputBuilder() {
        return UpdateDimensionInput.builder();
    }

    @Override
    public ShapeBuilder<UpdateDimensionOutput> outputBuilder() {
        return UpdateDimensionOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return UpdateDimensionInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return UpdateDimensionOutput.$SCHEMA;
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

