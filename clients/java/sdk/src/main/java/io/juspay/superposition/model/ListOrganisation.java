
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
 * Retrieves a paginated list of all organisations with their basic information and status details.
 */
@SmithyGenerated
public final class ListOrganisation implements ApiOperation<ListOrganisationInput, ListOrganisationOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ListOrganisation");

    private static final ListOrganisation $INSTANCE = new ListOrganisation();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("GET").code(200).uri(UriPattern.parse("/superposition/organisations")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static ListOrganisation instance() {
        return $INSTANCE;
    }

    private ListOrganisation() {}

    @Override
    public ShapeBuilder<ListOrganisationInput> inputBuilder() {
        return ListOrganisationInput.builder();
    }

    @Override
    public ShapeBuilder<ListOrganisationOutput> outputBuilder() {
        return ListOrganisationOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ListOrganisationInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ListOrganisationOutput.$SCHEMA;
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
        return Organisation.instance();
    }
}

