
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
 * Cancels a background job that is not in a terminal state (COMPLETED or FAILED). Sends a cancellation
 * request to Kronos and marks the job as FAILED.
 */
@SmithyGenerated
public final class CancelJob implements ApiOperation<CancelJobInput, CancelJobOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CancelJob");

    private static final CancelJob $INSTANCE = new CancelJob();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/jobs/{id}/cancel")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(ResourceNotFound.$ID, ResourceNotFound.class, ResourceNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static CancelJob instance() {
        return $INSTANCE;
    }

    private CancelJob() {}

    @Override
    public ShapeBuilder<CancelJobInput> inputBuilder() {
        return CancelJobInput.builder();
    }

    @Override
    public ShapeBuilder<CancelJobOutput> outputBuilder() {
        return CancelJobOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return CancelJobInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return CancelJobOutput.$SCHEMA;
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
        return Job.instance();
    }
}

