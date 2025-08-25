
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
 * Concludes an inprogress experiment by selecting a winning variant and transitioning the experiment
 * to a concluded state.
 */
@SmithyGenerated
public final class ConcludeExperiment implements ApiOperation<ConcludeExperimentInput, ConcludeExperimentOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ConcludeExperiment");

    private static final ConcludeExperiment $INSTANCE = new ConcludeExperiment();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PATCH").code(200).uri(UriPattern.parse("/experiments/{id}/conclude")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static ConcludeExperiment instance() {
        return $INSTANCE;
    }

    private ConcludeExperiment() {}

    @Override
    public ShapeBuilder<ConcludeExperimentInput> inputBuilder() {
        return ConcludeExperimentInput.builder();
    }

    @Override
    public ShapeBuilder<ConcludeExperimentOutput> outputBuilder() {
        return ConcludeExperimentOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ConcludeExperimentInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ConcludeExperimentOutput.$SCHEMA;
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
        return Experiments.instance();
    }
}

