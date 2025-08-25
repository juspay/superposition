
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
 * Resolves and merges config values based on context conditions, applying overrides and merge
 * strategies to produce the final configuration.
 */
@SmithyGenerated
public final class GetResolvedConfig implements ApiOperation<GetResolvedConfigInput, GetResolvedConfigOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetResolvedConfig");

    private static final GetResolvedConfig $INSTANCE = new GetResolvedConfig();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/config/resolve")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static GetResolvedConfig instance() {
        return $INSTANCE;
    }

    private GetResolvedConfig() {}

    @Override
    public ShapeBuilder<GetResolvedConfigInput> inputBuilder() {
        return GetResolvedConfigInput.builder();
    }

    @Override
    public ShapeBuilder<GetResolvedConfigOutput> outputBuilder() {
        return GetResolvedConfigOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetResolvedConfigInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetResolvedConfigOutput.$SCHEMA;
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
        return Config.instance();
    }
}

