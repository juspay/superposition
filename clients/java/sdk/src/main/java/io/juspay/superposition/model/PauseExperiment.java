
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
 * Temporarily pauses an inprogress experiment, suspending its effects while preserving the experiment
 * config for later resumption.
 */
@SmithyGenerated
public final class PauseExperiment implements ApiOperation<PauseExperimentInput, PauseExperimentOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#PauseExperiment");

    private static final PauseExperiment $INSTANCE = new PauseExperiment();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PATCH").code(200).uri(UriPattern.parse("/experiments/{id}/pause")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static PauseExperiment instance() {
        return $INSTANCE;
    }

    private PauseExperiment() {}

    @Override
    public ShapeBuilder<PauseExperimentInput> inputBuilder() {
        return PauseExperimentInput.builder();
    }

    @Override
    public ShapeBuilder<PauseExperimentOutput> outputBuilder() {
        return PauseExperimentOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return PauseExperimentInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return PauseExperimentOutput.$SCHEMA;
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

