
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
 * Discards an experiment without selecting a winner, effectively canceling the experiment and removing
 * its effects.
 */
@SmithyGenerated
public final class DiscardExperiment implements ApiOperation<DiscardExperimentInput, DiscardExperimentOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DiscardExperiment");

    private static final DiscardExperiment $INSTANCE = new DiscardExperiment();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PATCH").code(200).uri(UriPattern.parse("/experiments/{id}/discard")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static DiscardExperiment instance() {
        return $INSTANCE;
    }

    private DiscardExperiment() {}

    @Override
    public ShapeBuilder<DiscardExperimentInput> inputBuilder() {
        return DiscardExperimentInput.builder();
    }

    @Override
    public ShapeBuilder<DiscardExperimentOutput> outputBuilder() {
        return DiscardExperimentOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return DiscardExperimentInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return DiscardExperimentOutput.$SCHEMA;
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

