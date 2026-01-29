
package io.juspay.superposition.model;

import java.util.List;
import java.util.Map;
import software.amazon.smithy.java.core.schema.ApiResource;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Secrets are encrypted key-value pairs stored at the workspace level. Secret values are encrypted
 * with workspace-specific encryption keys and support key rotation.
 */
@SmithyGenerated
public final class MasterKey implements ApiResource {
    public static final ShapeId $ID = ShapeId.from("io.superposition#MasterKey");
    private static final MasterKey $INSTANCE = new MasterKey();
    private static final Map<String, Schema> $IDENTIFIERS = Map.of();
    private static final Map<String, Schema> $PROPERTIES = Map.of();

    private static final List<Schema> $OPERATIONS = List.of(GenerateMasterKey.$SCHEMA,
        RotateMasterKey.$SCHEMA);
    private static final Schema $SCHEMA = Schema.createResource($ID);

    /**
     * Get an instance of this {@code ApiResource}.
     *
     * @return An instance of this class.
     */
    public static MasterKey instance() {
        return $INSTANCE;
    }

    private MasterKey() {}

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Map<String, Schema> identifiers() {
        return $IDENTIFIERS;
    }

    @Override
    public Map<String, Schema> properties() {
        return $PROPERTIES;
    }

    @Override
    public List<Schema> operations() {
        return $OPERATIONS;
    }
}

