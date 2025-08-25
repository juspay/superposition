
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
 * Resumes a previously paused experiment, restoring its in-progress state and re-enabling variant
 * evaluation.
 */
@SmithyGenerated
public final class ResumeExperiment implements ApiOperation<ResumeExperimentInput, ResumeExperimentOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ResumeExperiment");

    private static final ResumeExperiment $INSTANCE = new ResumeExperiment();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PATCH").code(200).uri(UriPattern.parse("/experiments/{id}/resume")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static ResumeExperiment instance() {
        return $INSTANCE;
    }

    private ResumeExperiment() {}

    @Override
    public ShapeBuilder<ResumeExperimentInput> inputBuilder() {
        return ResumeExperimentInput.builder();
    }

    @Override
    public ShapeBuilder<ResumeExperimentOutput> outputBuilder() {
        return ResumeExperimentOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ResumeExperimentInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ResumeExperimentOutput.$SCHEMA;
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

