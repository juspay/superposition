
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
 * Resolves config values and returns each key with default-config metadata.
 */
@SmithyGenerated
public final class GetDetailedResolvedConfig implements ApiOperation<GetDetailedResolvedConfigInput, GetDetailedResolvedConfigOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetDetailedResolvedConfig");

    private static final GetDetailedResolvedConfig $INSTANCE = new GetDetailedResolvedConfig();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/config/resolve/detailed")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static GetDetailedResolvedConfig instance() {
        return $INSTANCE;
    }

    private GetDetailedResolvedConfig() {}

    @Override
    public ShapeBuilder<GetDetailedResolvedConfigInput> inputBuilder() {
        return GetDetailedResolvedConfigInput.builder();
    }

    @Override
    public ShapeBuilder<GetDetailedResolvedConfigOutput> outputBuilder() {
        return GetDetailedResolvedConfigOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetDetailedResolvedConfigInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetDetailedResolvedConfigOutput.$SCHEMA;
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

