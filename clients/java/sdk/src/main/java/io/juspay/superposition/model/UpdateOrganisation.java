
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
 * Updates an existing organisation's information including contact details, status, and administrative
 * properties.
 */
@SmithyGenerated
public final class UpdateOrganisation implements ApiOperation<UpdateOrganisationInput, UpdateOrganisationOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateOrganisation");

    private static final UpdateOrganisation $INSTANCE = new UpdateOrganisation();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("PUT").code(200).uri(UriPattern.parse("/superposition/organisations/{id}")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .putType(OrganisationNotFound.$ID, OrganisationNotFound.class, OrganisationNotFound::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static UpdateOrganisation instance() {
        return $INSTANCE;
    }

    private UpdateOrganisation() {}

    @Override
    public ShapeBuilder<UpdateOrganisationInput> inputBuilder() {
        return UpdateOrganisationInput.builder();
    }

    @Override
    public ShapeBuilder<UpdateOrganisationOutput> outputBuilder() {
        return UpdateOrganisationOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return UpdateOrganisationInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return UpdateOrganisationOutput.$SCHEMA;
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

