
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
 * Retrieves an existing experiment group by its ID.
 */
@SmithyGenerated
public final class GetExperimentGroup implements ApiOperation<GetExperimentGroupInput, GetExperimentGroupOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetExperimentGroup");

    private static final GetExperimentGroup $INSTANCE = new GetExperimentGroup();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/experiment-groups/{id}")).build());

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
    public static GetExperimentGroup instance() {
        return $INSTANCE;
    }

    private GetExperimentGroup() {}

    @Override
    public ShapeBuilder<GetExperimentGroupInput> inputBuilder() {
        return GetExperimentGroupInput.builder();
    }

    @Override
    public ShapeBuilder<GetExperimentGroupOutput> outputBuilder() {
        return GetExperimentGroupOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetExperimentGroupInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetExperimentGroupOutput.$SCHEMA;
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
        return ExperimentGroup.instance();
    }
}

