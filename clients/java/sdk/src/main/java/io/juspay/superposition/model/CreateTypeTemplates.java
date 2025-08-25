
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
 * Creates a new type template with specified schema definition, providing reusable type definitions
 * for config validation.
 */
@SmithyGenerated
public final class CreateTypeTemplates implements ApiOperation<CreateTypeTemplatesInput, CreateTypeTemplatesOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateTypeTemplates");

    private static final CreateTypeTemplates $INSTANCE = new CreateTypeTemplates();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/types")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static CreateTypeTemplates instance() {
        return $INSTANCE;
    }

    private CreateTypeTemplates() {}

    @Override
    public ShapeBuilder<CreateTypeTemplatesInput> inputBuilder() {
        return CreateTypeTemplatesInput.builder();
    }

    @Override
    public ShapeBuilder<CreateTypeTemplatesOutput> outputBuilder() {
        return CreateTypeTemplatesOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return CreateTypeTemplatesInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return CreateTypeTemplatesOutput.$SCHEMA;
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

