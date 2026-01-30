
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
 * Rotates the workspace encryption key. Generates a new encryption key and re-encrypts all secrets
 * with the new key. This is a critical operation that should be done during low-traffic periods.
 */
@SmithyGenerated
public final class RotateWorkspaceEncryptionKey implements ApiOperation<RotateWorkspaceEncryptionKeyInput, RotateWorkspaceEncryptionKeyOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#RotateWorkspaceEncryptionKey");

    private static final RotateWorkspaceEncryptionKey $INSTANCE = new RotateWorkspaceEncryptionKey();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/workspaces/{workspace_name}/rotate-encryption-key")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static RotateWorkspaceEncryptionKey instance() {
        return $INSTANCE;
    }

    private RotateWorkspaceEncryptionKey() {}

    @Override
    public ShapeBuilder<RotateWorkspaceEncryptionKeyInput> inputBuilder() {
        return RotateWorkspaceEncryptionKeyInput.builder();
    }

    @Override
    public ShapeBuilder<RotateWorkspaceEncryptionKeyOutput> outputBuilder() {
        return RotateWorkspaceEncryptionKeyOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return RotateWorkspaceEncryptionKeyInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return RotateWorkspaceEncryptionKeyOutput.$SCHEMA;
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
        return Workspace.instance();
    }
}

