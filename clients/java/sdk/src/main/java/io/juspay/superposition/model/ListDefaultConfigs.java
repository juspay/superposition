
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
 * Retrieves a paginated list of all default config entries in the workspace, including their values,
 * schemas, and metadata.
 */
@SmithyGenerated
public final class ListDefaultConfigs implements ApiOperation<ListDefaultConfigsInput, ListDefaultConfigsOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListDefaultConfigs");

    private static final ListDefaultConfigs $INSTANCE = new ListDefaultConfigs();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/default-config")).build());

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
    public static ListDefaultConfigs instance() {
        return $INSTANCE;
    }

    private ListDefaultConfigs() {}

    @Override
    public ShapeBuilder<ListDefaultConfigsInput> inputBuilder() {
        return ListDefaultConfigsInput.builder();
    }

    @Override
    public ShapeBuilder<ListDefaultConfigsOutput> outputBuilder() {
        return ListDefaultConfigsOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ListDefaultConfigsInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ListDefaultConfigsOutput.$SCHEMA;
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

