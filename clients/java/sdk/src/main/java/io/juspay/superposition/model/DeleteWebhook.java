
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
 * Permanently removes a webhook config from the workspace, stopping all future event notifications to
 * that endpoint.
 */
@SmithyGenerated
public final class DeleteWebhook implements ApiOperation<DeleteWebhookInput, DeleteWebhookOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#DeleteWebhook");

    private static final DeleteWebhook $INSTANCE = new DeleteWebhook();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("DELETE").code(204).uri(UriPattern.parse("/webhook/{name}")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(ResourceNotFound.$ID, ResourceNotFound.class, ResourceNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static DeleteWebhook instance() {
        return $INSTANCE;
    }

    private DeleteWebhook() {}

    @Override
    public ShapeBuilder<DeleteWebhookInput> inputBuilder() {
        return DeleteWebhookInput.builder();
    }

    @Override
    public ShapeBuilder<DeleteWebhookOutput> outputBuilder() {
        return DeleteWebhookOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return DeleteWebhookInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return DeleteWebhookOutput.$SCHEMA;
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
        return Webhook.instance();
    }
}

