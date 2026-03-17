
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
 * Retrieves the full config in TOML format, including default configs with schemas, dimensions, and
 * overrides. This endpoint is optimized for clients that prefer TOML format for configuration
 * management.
 */
@SmithyGenerated
public final class GetConfigToml implements ApiOperation<GetConfigTomlInput, GetConfigTomlOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetConfigToml");

    private static final GetConfigToml $INSTANCE = new GetConfigToml();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/config/toml")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static GetConfigToml instance() {
        return $INSTANCE;
    }

    private GetConfigToml() {}

    @Override
    public ShapeBuilder<GetConfigTomlInput> inputBuilder() {
        return GetConfigTomlInput.builder();
    }

    @Override
    public ShapeBuilder<GetConfigTomlOutput> outputBuilder() {
        return GetConfigTomlOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetConfigTomlInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetConfigTomlOutput.$SCHEMA;
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

