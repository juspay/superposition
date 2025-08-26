
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
 * Permanently removes a default config entry from the workspace. This operation cannot be performed if
 * it affects config resolution for contexts that rely on this fallback value.
 */
@SmithyGenerated
public final class DeleteDefaultConfig implements ApiOperation<DeleteDefaultConfigInput, DeleteDefaultConfigOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DeleteDefaultConfig");

    private static final DeleteDefaultConfig $INSTANCE = new DeleteDefaultConfig();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("DELETE").code(201).uri(UriPattern.parse("/default-config/{key}")).build());

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
    public static DeleteDefaultConfig instance() {
        return $INSTANCE;
    }

    private DeleteDefaultConfig() {}

    @Override
    public ShapeBuilder<DeleteDefaultConfigInput> inputBuilder() {
        return DeleteDefaultConfigInput.builder();
    }

    @Override
    public ShapeBuilder<DeleteDefaultConfigOutput> outputBuilder() {
        return DeleteDefaultConfigOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return DeleteDefaultConfigInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return DeleteDefaultConfigOutput.$SCHEMA;
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

