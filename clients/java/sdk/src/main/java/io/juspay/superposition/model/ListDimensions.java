
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
 * Retrieves a paginated list of all dimensions in the workspace. Dimensions are returned with their
 * details and metadata.
 */
@SmithyGenerated
public final class ListDimensions implements ApiOperation<ListDimensionsInput, ListDimensionsOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListDimensions");

    private static final ListDimensions $INSTANCE = new ListDimensions();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/dimension")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static ListDimensions instance() {
        return $INSTANCE;
    }

    private ListDimensions() {}

    @Override
    public ShapeBuilder<ListDimensionsInput> inputBuilder() {
        return ListDimensionsInput.builder();
    }

    @Override
    public ShapeBuilder<ListDimensionsOutput> outputBuilder() {
        return ListDimensionsOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ListDimensionsInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ListDimensionsOutput.$SCHEMA;
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

