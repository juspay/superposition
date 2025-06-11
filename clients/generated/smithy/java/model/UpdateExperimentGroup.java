
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
 * Updates an existing experiment group. Allows partial updates to specified fields.
 */
@SmithyGenerated
public final class UpdateExperimentGroup implements ApiOperation<UpdateExperimentGroupInput, UpdateExperimentGroupOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateExperimentGroup");

    private static final UpdateExperimentGroup $INSTANCE = new UpdateExperimentGroup();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PATCH").code(200).uri(UriPattern.parse("/experiment-groups/{id}")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(ResourceNotFound.$ID, ResourceNotFound.class, ResourceNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static UpdateExperimentGroup instance() {
        return $INSTANCE;
    }

    private UpdateExperimentGroup() {}

    @Override
    public ShapeBuilder<UpdateExperimentGroupInput> inputBuilder() {
        return UpdateExperimentGroupInput.builder();
    }

    @Override
    public ShapeBuilder<UpdateExperimentGroupOutput> outputBuilder() {
        return UpdateExperimentGroupOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return UpdateExperimentGroupInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return UpdateExperimentGroupOutput.$SCHEMA;
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

