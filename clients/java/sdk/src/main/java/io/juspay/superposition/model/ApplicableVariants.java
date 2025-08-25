
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
 * Determines which experiment variants are applicable to a given context, used for experiment
 * evaluation and variant selection.
 */
@SmithyGenerated
public final class ApplicableVariants implements ApiOperation<ApplicableVariantsInput, ApplicableVariantsOutput> {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ApplicableVariants");

    private static final ApplicableVariants $INSTANCE = new ApplicableVariants();

    static final Schema $SCHEMA = Schema.createOperation($ID,
            HttpTrait.builder().method("POST").code(200).uri(UriPattern.parse("/experiments/applicable-variants")).build());

    private static final TypeRegistry TYPE_REGISTRY = TypeRegistry.builder()
        .putType(InternalServerError.$ID, InternalServerError.class, InternalServerError::builder)
        .build();

    private static final List<ShapeId> SCHEMES = List.of(ShapeId.from("smithy.api#httpBearerAuth"));

    /**
     * Get an instance of this {@code ApiOperation}.
     *
     * @return An instance of this class.
     */
    public static ApplicableVariants instance() {
        return $INSTANCE;
    }

    private ApplicableVariants() {}

    @Override
    public ShapeBuilder<ApplicableVariantsInput> inputBuilder() {
        return ApplicableVariantsInput.builder();
    }

    @Override
    public ShapeBuilder<ApplicableVariantsOutput> outputBuilder() {
        return ApplicableVariantsOutput.builder();
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public Schema inputSchema() {
        return ApplicableVariantsInput.$SCHEMA;
    }

    @Override
    public Schema outputSchema() {
        return ApplicableVariantsOutput.$SCHEMA;
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
        return Experiments.instance();
    }
}

