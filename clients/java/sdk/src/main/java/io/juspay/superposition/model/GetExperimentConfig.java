
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
 * Retrieves the experiment configuration for a given workspace and organization. The response includes
 * details of all experiment groups and experiments that match the specified filters.
 */
@SmithyGenerated
public final class GetExperimentConfig implements ApiOperation<GetExperimentConfigInput, GetExperimentConfigOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetExperimentConfig");

    private static final GetExperimentConfig $INSTANCE = new GetExperimentConfig();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/experiment-config")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static GetExperimentConfig instance() {
        return $INSTANCE;
    }

    private GetExperimentConfig() {}

    @Override
    public ShapeBuilder<GetExperimentConfigInput> inputBuilder() {
        return GetExperimentConfigInput.builder();
    }

    @Override
    public ShapeBuilder<GetExperimentConfigOutput> outputBuilder() {
        return GetExperimentConfigOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetExperimentConfigInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetExperimentConfigOutput.$SCHEMA;
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
        return ExperimentConfig.instance();
    }
}

