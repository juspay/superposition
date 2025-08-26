
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
 * Updates an existing type template's schema definition and metadata while preserving its identifier
 * and usage history.
 */
@SmithyGenerated
public final class UpdateTypeTemplates implements ApiOperation<UpdateTypeTemplatesInput, UpdateTypeTemplatesOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateTypeTemplates");

    private static final UpdateTypeTemplates $INSTANCE = new UpdateTypeTemplates();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PUT").code(200).uri(UriPattern.parse("/types/{type_name}")).build());

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
    public static UpdateTypeTemplates instance() {
        return $INSTANCE;
    }

    private UpdateTypeTemplates() {}

    @Override
    public ShapeBuilder<UpdateTypeTemplatesInput> inputBuilder() {
        return UpdateTypeTemplatesInput.builder();
    }

    @Override
    public ShapeBuilder<UpdateTypeTemplatesOutput> outputBuilder() {
        return UpdateTypeTemplatesOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return UpdateTypeTemplatesInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return UpdateTypeTemplatesOutput.$SCHEMA;
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

