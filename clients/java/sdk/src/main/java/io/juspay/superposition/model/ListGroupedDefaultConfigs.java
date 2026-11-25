
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
public final class ListGroupedDefaultConfigs implements ApiOperation<ListGroupedDefaultConfigsInput, ListGroupedDefaultConfigsOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListGroupedDefaultConfigs");

    private static final ListGroupedDefaultConfigs $INSTANCE = new ListGroupedDefaultConfigs();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/default-config?grouped=true")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static ListGroupedDefaultConfigs instance() {
        return $INSTANCE;
    }

    private ListGroupedDefaultConfigs() {}

    @Override
    public ShapeBuilder<ListGroupedDefaultConfigsInput> inputBuilder() {
        return ListGroupedDefaultConfigsInput.builder();
    }

    @Override
    public ShapeBuilder<ListGroupedDefaultConfigsOutput> outputBuilder() {
        return ListGroupedDefaultConfigsOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ListGroupedDefaultConfigsInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ListGroupedDefaultConfigsOutput.$SCHEMA;
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

