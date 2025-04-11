
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

@SmithyGenerated
public final class UpdateFunction implements ApiOperation<UpdateFunctionInput, UpdateFunctionOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateFunction");

    private static final UpdateFunction $INSTANCE = new UpdateFunction();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PATCH").code(200).uri(UriPattern.parse("/function/{function_name}")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(FunctionNotFound.$ID, FunctionNotFound.class, FunctionNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static UpdateFunction instance() {
        return $INSTANCE;
    }

    private UpdateFunction() {}

    @Override
    public ShapeBuilder<UpdateFunctionInput> inputBuilder() {
        return UpdateFunctionInput.builder();
    }

    @Override
    public ShapeBuilder<UpdateFunctionOutput> outputBuilder() {
        return UpdateFunctionOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return UpdateFunctionInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return UpdateFunctionOutput.$SCHEMA;
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

