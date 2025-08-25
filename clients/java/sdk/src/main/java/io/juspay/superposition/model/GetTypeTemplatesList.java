
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
 * Retrieves a paginated list of all type templates in the workspace, including their schemas and
 * metadata for type management.
 */
@SmithyGenerated
public final class GetTypeTemplatesList implements ApiOperation<GetTypeTemplatesListInput, GetTypeTemplatesListOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetTypeTemplatesList");

    private static final GetTypeTemplatesList $INSTANCE = new GetTypeTemplatesList();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/types")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static GetTypeTemplatesList instance() {
        return $INSTANCE;
    }

    private GetTypeTemplatesList() {}

    @Override
    public ShapeBuilder<GetTypeTemplatesListInput> inputBuilder() {
        return GetTypeTemplatesListInput.builder();
    }

    @Override
    public ShapeBuilder<GetTypeTemplatesListOutput> outputBuilder() {
        return GetTypeTemplatesListOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetTypeTemplatesListInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetTypeTemplatesListOutput.$SCHEMA;
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

