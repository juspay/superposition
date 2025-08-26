
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
 * Creates a new custom function for validation or autocompletion with specified code, runtime version,
 * and function type.
 */
@SmithyGenerated
public final class CreateFunction implements ApiOperation<CreateFunctionInput, CreateFunctionOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateFunction");

    private static final CreateFunction $INSTANCE = new CreateFunction();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/function")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static CreateFunction instance() {
        return $INSTANCE;
    }

    private CreateFunction() {}

    @Override
    public ShapeBuilder<CreateFunctionInput> inputBuilder() {
        return CreateFunctionInput.builder();
    }

    @Override
    public ShapeBuilder<CreateFunctionOutput> outputBuilder() {
        return CreateFunctionOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return CreateFunctionInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return CreateFunctionOutput.$SCHEMA;
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
        return Function.instance();
    }
}

