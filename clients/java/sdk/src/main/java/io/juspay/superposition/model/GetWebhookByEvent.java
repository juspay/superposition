
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
 * Retrieves a webhook configuration based on a specific event type, allowing users to find which
 * webhook is set to trigger for that event.
 */
@SmithyGenerated
public final class GetWebhookByEvent implements ApiOperation<GetWebhookByEventInput, GetWebhookByEventOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#GetWebhookByEvent");

    private static final GetWebhookByEvent $INSTANCE = new GetWebhookByEvent();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/webhook/event/{event}")).build());

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
    public static GetWebhookByEvent instance() {
        return $INSTANCE;
    }

    private GetWebhookByEvent() {}

    @Override
    public ShapeBuilder<GetWebhookByEventInput> inputBuilder() {
        return GetWebhookByEventInput.builder();
    }

    @Override
    public ShapeBuilder<GetWebhookByEventOutput> outputBuilder() {
        return GetWebhookByEventOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return GetWebhookByEventInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return GetWebhookByEventOutput.$SCHEMA;
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

