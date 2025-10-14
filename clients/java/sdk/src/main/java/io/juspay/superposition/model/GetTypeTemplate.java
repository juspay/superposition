
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
 * Retrieves detailed information about a specific type template including its schema and metadata.
 */
@SmithyGenerated
public final class GetTypeTemplate implements ApiOperation<GetTypeTemplateInput, GetTypeTemplateOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetTypeTemplate");

    private static final GetTypeTemplate $INSTANCE = new GetTypeTemplate();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/types/{type_name}")).build());

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
    public static GetTypeTemplate instance() {
        return $INSTANCE;
    }

    private GetTypeTemplate() {}

    @Override
    public ShapeBuilder<GetTypeTemplateInput> inputBuilder() {
        return GetTypeTemplateInput.builder();
    }

    @Override
    public ShapeBuilder<GetTypeTemplateOutput> outputBuilder() {
        return GetTypeTemplateOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetTypeTemplateInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetTypeTemplateOutput.$SCHEMA;
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

