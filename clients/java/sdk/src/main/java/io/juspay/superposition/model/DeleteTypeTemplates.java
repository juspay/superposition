
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
 * Permanently removes a type template from the workspace. No checks performed while deleting
 */
@SmithyGenerated
public final class DeleteTypeTemplates implements ApiOperation<DeleteTypeTemplatesInput, DeleteTypeTemplatesOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DeleteTypeTemplates");

    private static final DeleteTypeTemplates $INSTANCE = new DeleteTypeTemplates();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("DELETE").code(200).uri(UriPattern.parse("/types/{type_name}")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(TypeTemplatesNotFound.$ID, TypeTemplatesNotFound.class, TypeTemplatesNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static DeleteTypeTemplates instance() {
        return $INSTANCE;
    }

    private DeleteTypeTemplates() {}

    @Override
    public ShapeBuilder<DeleteTypeTemplatesInput> inputBuilder() {
        return DeleteTypeTemplatesInput.builder();
    }

    @Override
    public ShapeBuilder<DeleteTypeTemplatesOutput> outputBuilder() {
        return DeleteTypeTemplatesOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return DeleteTypeTemplatesInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return DeleteTypeTemplatesOutput.$SCHEMA;
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
        return TypeTemplates.instance();
    }
}

