
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
 * Creates a new dimension with the specified json schema. Dimensions define categorical attributes
 * used for context-based config management.
 */
@SmithyGenerated
public final class CreateDimension implements ApiOperation<CreateDimensionInput, CreateDimensionOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateDimension");

    private static final CreateDimension $INSTANCE = new CreateDimension();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/dimension")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static CreateDimension instance() {
        return $INSTANCE;
    }

    private CreateDimension() {}

    @Override
    public ShapeBuilder<CreateDimensionInput> inputBuilder() {
        return CreateDimensionInput.builder();
    }

    @Override
    public ShapeBuilder<CreateDimensionOutput> outputBuilder() {
        return CreateDimensionOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return CreateDimensionInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return CreateDimensionOutput.$SCHEMA;
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

