
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
 * Updates an existing webhook config, allowing modification of URL, events, headers, and other webhook
 * properties.
 */
@SmithyGenerated
public final class UpdateWebhook implements ApiOperation<UpdateWebhookInput, UpdateWebhookOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateWebhook");

    private static final UpdateWebhook $INSTANCE = new UpdateWebhook();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PATCH").code(200).uri(UriPattern.parse("/webhook/{name}")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(WebhookNotFound.$ID, WebhookNotFound.class, WebhookNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static UpdateWebhook instance() {
        return $INSTANCE;
    }

    private UpdateWebhook() {}

    @Override
    public ShapeBuilder<UpdateWebhookInput> inputBuilder() {
        return UpdateWebhookInput.builder();
    }

    @Override
    public ShapeBuilder<UpdateWebhookOutput> outputBuilder() {
        return UpdateWebhookOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return UpdateWebhookInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return UpdateWebhookOutput.$SCHEMA;
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

