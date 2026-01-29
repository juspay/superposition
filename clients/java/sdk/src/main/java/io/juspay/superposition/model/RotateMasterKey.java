
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
 * Rotates the master key encryption key across all workspaces
 */
@SmithyGenerated
public final class RotateMasterKey implements ApiOperation<RotateMasterKeyInput, RotateMasterKeyOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#RotateMasterKey");

    private static final RotateMasterKey $INSTANCE = new RotateMasterKey();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/master-key/rotate")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static RotateMasterKey instance() {
        return $INSTANCE;
    }

    private RotateMasterKey() {}

    @Override
    public ShapeBuilder<RotateMasterKeyInput> inputBuilder() {
        return RotateMasterKeyInput.builder();
    }

    @Override
    public ShapeBuilder<RotateMasterKeyOutput> outputBuilder() {
        return RotateMasterKeyOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return RotateMasterKeyInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return RotateMasterKeyOutput.$SCHEMA;
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
        return MasterKey.instance();
    }
}

