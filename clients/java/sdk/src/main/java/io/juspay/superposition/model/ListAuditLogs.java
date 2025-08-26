
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
 * Retrieves a paginated list of audit logs with support for filtering by date range, table names,
 * actions, and usernames for compliance and monitoring purposes.
 */
@SmithyGenerated
public final class ListAuditLogs implements ApiOperation<ListAuditLogsInput, ListAuditLogsOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListAuditLogs");

    private static final ListAuditLogs $INSTANCE = new ListAuditLogs();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/audit")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static ListAuditLogs instance() {
        return $INSTANCE;
    }

    private ListAuditLogs() {}

    @Override
    public ShapeBuilder<ListAuditLogsInput> inputBuilder() {
        return ListAuditLogsInput.builder();
    }

    @Override
    public ShapeBuilder<ListAuditLogsOutput> outputBuilder() {
        return ListAuditLogsOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ListAuditLogsInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ListAuditLogsOutput.$SCHEMA;
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
        return AuditLog.instance();
    }
}

