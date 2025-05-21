
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
 * Creates a new experiment group.
 */
@SmithyGenerated
public final class CreateExperimentGroup implements ApiOperation<CreateExperimentGroupInput, CreateExperimentGroupOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateExperimentGroup");

    private static final CreateExperimentGroup $INSTANCE = new CreateExperimentGroup();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/experiment-groups")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static CreateExperimentGroup instance() {
        return $INSTANCE;
    }

    private CreateExperimentGroup() {}

    @Override
    public ShapeBuilder<CreateExperimentGroupInput> inputBuilder() {
        return CreateExperimentGroupInput.builder();
    }

    @Override
    public ShapeBuilder<CreateExperimentGroupOutput> outputBuilder() {
        return CreateExperimentGroupOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return CreateExperimentGroupInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return CreateExperimentGroupOutput.$SCHEMA;
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

