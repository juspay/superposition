
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
 * Updates the overrides for specific variants within an experiment, allowing modification of
 * experiment behavior Updates the overrides for specific variants within an experiment, allowing
 * modification of experiment behavior while it is in the created state.
 */
@SmithyGenerated
public final class UpdateOverridesExperiment implements ApiOperation<UpdateOverridesExperimentInput, UpdateOverridesExperimentOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateOverridesExperiment");

    private static final UpdateOverridesExperiment $INSTANCE = new UpdateOverridesExperiment();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PUT").code(200).uri(UriPattern.parse("/experiments/{id}/overrides")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static UpdateOverridesExperiment instance() {
        return $INSTANCE;
    }

    private UpdateOverridesExperiment() {}

    @Override
    public ShapeBuilder<UpdateOverridesExperimentInput> inputBuilder() {
        return UpdateOverridesExperimentInput.builder();
    }

    @Override
    public ShapeBuilder<UpdateOverridesExperimentOutput> outputBuilder() {
        return UpdateOverridesExperimentOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return UpdateOverridesExperimentInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return UpdateOverridesExperimentOutput.$SCHEMA;
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

