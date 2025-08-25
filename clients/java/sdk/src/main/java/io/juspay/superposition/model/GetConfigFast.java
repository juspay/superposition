
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
 * Retrieves the latest config with no processing for high-performance access.
 */
@SmithyGenerated
public final class GetConfigFast implements ApiOperation<GetConfigFastInput, GetConfigFastOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetConfigFast");

    private static final GetConfigFast $INSTANCE = new GetConfigFast();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/config/fast")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static GetConfigFast instance() {
        return $INSTANCE;
    }

    private GetConfigFast() {}

    @Override
    public ShapeBuilder<GetConfigFastInput> inputBuilder() {
        return GetConfigFastInput.builder();
    }

    @Override
    public ShapeBuilder<GetConfigFastOutput> outputBuilder() {
        return GetConfigFastOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetConfigFastInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetConfigFastOutput.$SCHEMA;
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

