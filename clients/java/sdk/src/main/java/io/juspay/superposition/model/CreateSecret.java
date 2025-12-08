
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
 * Creates a new encrypted secret with the specified name and value. The secret is encrypted with the
 * workspace's current encryption key. Secret values are never returned in responses for security.
 */
@SmithyGenerated
public final class CreateSecret implements ApiOperation<CreateSecretInput, CreateSecretOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateSecret");

    private static final CreateSecret $INSTANCE = new CreateSecret();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/secrets")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static CreateSecret instance() {
        return $INSTANCE;
    }

    private CreateSecret() {}

    @Override
    public ShapeBuilder<CreateSecretInput> inputBuilder() {
        return CreateSecretInput.builder();
    }

    @Override
    public ShapeBuilder<CreateSecretOutput> outputBuilder() {
        return CreateSecretOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return CreateSecretInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return CreateSecretOutput.$SCHEMA;
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
        return Secret.instance();
    }
}

