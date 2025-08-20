
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
 * Retrieves config data with context evaluation, including applicable contexts, overrides, and default
 * values based on provided conditions.
 */
@SmithyGenerated
public final class GetConfig implements ApiOperation<GetConfigInput, GetConfigOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetConfig");

    private static final GetConfig $INSTANCE = new GetConfig();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/config")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static GetConfig instance() {
        return $INSTANCE;
    }

    private GetConfig() {}

    @Override
    public ShapeBuilder<GetConfigInput> inputBuilder() {
        return GetConfigInput.builder();
    }

    @Override
    public ShapeBuilder<GetConfigOutput> outputBuilder() {
        return GetConfigOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetConfigInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetConfigOutput.$SCHEMA;
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

