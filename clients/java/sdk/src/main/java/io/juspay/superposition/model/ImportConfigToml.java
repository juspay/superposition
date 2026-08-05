
package io.juspay.superposition.model;

import java.util.List;
import software.amazon.smithy.java.core.schema.ApiOperation;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.TypeRegistry;
import software.amazon.smithy.model.pattern.UriPattern;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Imports a full config from a TOML document, persisting dimensions, default-configs and contexts in a
 * single transaction after validating the document.
 */
@SmithyGenerated
public final class ImportConfigToml implements ApiOperation<ImportConfigTomlInput, ImportConfigTomlOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ImportConfigToml");

    private static final ImportConfigToml $INSTANCE = new ImportConfigToml();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/config/toml/import")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBasicAuth"), ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static ImportConfigToml instance() {
        return $INSTANCE;
    }

    private ImportConfigToml() {}

    @Override
    public ShapeBuilder<ImportConfigTomlInput> inputBuilder() {
        return ImportConfigTomlInput.builder();
    }

    @Override
    public ShapeBuilder<ImportConfigTomlOutput> outputBuilder() {
        return ImportConfigTomlOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ImportConfigTomlInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ImportConfigTomlOutput.$SCHEMA;
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
}

