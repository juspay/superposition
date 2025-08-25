
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
 * Creates a new default config entry with specified key, value, schema, and metadata. Default configs
 * serve as fallback values when no specific context matches.
 */
@SmithyGenerated
public final class CreateDefaultConfig implements ApiOperation<CreateDefaultConfigInput, CreateDefaultConfigOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateDefaultConfig");

    private static final CreateDefaultConfig $INSTANCE = new CreateDefaultConfig();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/default-config")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static CreateDefaultConfig instance() {
        return $INSTANCE;
    }

    private CreateDefaultConfig() {}

    @Override
    public ShapeBuilder<CreateDefaultConfigInput> inputBuilder() {
        return CreateDefaultConfigInput.builder();
    }

    @Override
    public ShapeBuilder<CreateDefaultConfigOutput> outputBuilder() {
        return CreateDefaultConfigOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return CreateDefaultConfigInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return CreateDefaultConfigOutput.$SCHEMA;
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
        return DefaultConfig.instance();
    }
}

