
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
 * Recalculates and updates the priority weights for all contexts in the workspace based on their
 * dimensions.
 */
@SmithyGenerated
public final class WeightRecompute implements ApiOperation<WeightRecomputeInput, WeightRecomputeOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WeightRecompute");

    private static final WeightRecompute $INSTANCE = new WeightRecompute();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PUT").code(200).uri(UriPattern.parse("/context/weight/recompute")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static WeightRecompute instance() {
        return $INSTANCE;
    }

    private WeightRecompute() {}

    @Override
    public ShapeBuilder<WeightRecomputeInput> inputBuilder() {
        return WeightRecomputeInput.builder();
    }

    @Override
    public ShapeBuilder<WeightRecomputeOutput> outputBuilder() {
        return WeightRecomputeOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return WeightRecomputeInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return WeightRecomputeOutput.$SCHEMA;
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

