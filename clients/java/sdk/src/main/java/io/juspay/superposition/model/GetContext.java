
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
 * Retrieves detailed information about a specific context by its unique identifier, including
 * conditions, overrides, and metadata.
 */
@SmithyGenerated
public final class GetContext implements ApiOperation<GetContextInput, GetContextOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetContext");

    private static final GetContext $INSTANCE = new GetContext();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/context/{id}")).build());

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
    public static GetContext instance() {
        return $INSTANCE;
    }

    private GetContext() {}

    @Override
    public ShapeBuilder<GetContextInput> inputBuilder() {
        return GetContextInput.builder();
    }

    @Override
    public ShapeBuilder<GetContextOutput> outputBuilder() {
        return GetContextOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetContextInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetContextOutput.$SCHEMA;
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

