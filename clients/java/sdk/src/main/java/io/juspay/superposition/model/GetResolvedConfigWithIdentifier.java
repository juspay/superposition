
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
 * Resolves and merges config values based on context conditions and identifier, applying overrides and
 * merge strategies to produce the final configuration.
 */
@SmithyGenerated
public final class GetResolvedConfigWithIdentifier implements ApiOperation<GetResolvedConfigWithIdentifierInput, GetResolvedConfigWithIdentifierOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetResolvedConfigWithIdentifier");

    private static final GetResolvedConfigWithIdentifier $INSTANCE = new GetResolvedConfigWithIdentifier();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/resolve")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static GetResolvedConfigWithIdentifier instance() {
        return $INSTANCE;
    }

    private GetResolvedConfigWithIdentifier() {}

    @Override
    public ShapeBuilder<GetResolvedConfigWithIdentifierInput> inputBuilder() {
        return GetResolvedConfigWithIdentifierInput.builder();
    }

    @Override
    public ShapeBuilder<GetResolvedConfigWithIdentifierOutput> outputBuilder() {
        return GetResolvedConfigWithIdentifierOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetResolvedConfigWithIdentifierInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetResolvedConfigWithIdentifierOutput.$SCHEMA;
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

