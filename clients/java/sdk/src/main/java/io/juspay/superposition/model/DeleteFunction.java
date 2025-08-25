
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
 * Permanently removes a function from the workspace, deleting both draft and published versions along
 * with all associated code. It fails if already in use
 */
@SmithyGenerated
public final class DeleteFunction implements ApiOperation<DeleteFunctionInput, DeleteFunctionOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DeleteFunction");

    private static final DeleteFunction $INSTANCE = new DeleteFunction();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("DELETE").code(200).uri(UriPattern.parse("/function/{function_name}")).build());

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
    public static DeleteFunction instance() {
        return $INSTANCE;
    }

    private DeleteFunction() {}

    @Override
    public ShapeBuilder<DeleteFunctionInput> inputBuilder() {
        return DeleteFunctionInput.builder();
    }

    @Override
    public ShapeBuilder<DeleteFunctionOutput> outputBuilder() {
        return DeleteFunctionOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return DeleteFunctionInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return DeleteFunctionOutput.$SCHEMA;
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

