
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
 * Retrieves context information by matching against provided conditions. Used to find contexts that
 * would apply to specific scenarios.
 */
@SmithyGenerated
public final class GetContextFromCondition implements ApiOperation<GetContextFromConditionInput, GetContextFromConditionOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetContextFromCondition");

    private static final GetContextFromCondition $INSTANCE = new GetContextFromCondition();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/context/get")).build());

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
    public static GetContextFromCondition instance() {
        return $INSTANCE;
    }

    private GetContextFromCondition() {}

    @Override
    public ShapeBuilder<GetContextFromConditionInput> inputBuilder() {
        return GetContextFromConditionInput.builder();
    }

    @Override
    public ShapeBuilder<GetContextFromConditionOutput> outputBuilder() {
        return GetContextFromConditionOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetContextFromConditionInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetContextFromConditionOutput.$SCHEMA;
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

