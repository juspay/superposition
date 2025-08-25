
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
 * Executes multiple context operations (PUT, REPLACE, DELETE, MOVE) in a single atomic transaction for
 * efficient batch processing.
 */
@SmithyGenerated
public final class BulkOperation implements ApiOperation<BulkOperationInput, BulkOperationOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#BulkOperation");

    private static final BulkOperation $INSTANCE = new BulkOperation();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PUT").code(200).uri(UriPattern.parse("/context/bulk-operations")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static BulkOperation instance() {
        return $INSTANCE;
    }

    private BulkOperation() {}

    @Override
    public ShapeBuilder<BulkOperationInput> inputBuilder() {
        return BulkOperationInput.builder();
    }

    @Override
    public ShapeBuilder<BulkOperationOutput> outputBuilder() {
        return BulkOperationOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return BulkOperationInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return BulkOperationOutput.$SCHEMA;
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

