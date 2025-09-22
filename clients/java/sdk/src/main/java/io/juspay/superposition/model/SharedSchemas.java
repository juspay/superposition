
package io.juspay.superposition.model;

import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.LengthTrait;
import software.amazon.smithy.model.traits.SparseTrait;
import software.amazon.smithy.model.traits.TimestampFormatTrait;

/**
 * Defines shared shapes across the model package that are not part of another code-generated type.
 */
final class SharedSchemas {

    static final Schema STRING_LIST = Schema.listBuilder(ShapeId.from("io.superposition#StringList"))
        .putMember("member", PreludeSchemas.STRING)
        .build();

    static final Schema BUCKETS = Schema.listBuilder(ShapeId.from("io.superposition#Buckets"),
            new SparseTrait())
        .putMember("member", Bucket.$SCHEMA)
        .build();

    static final Schema CONDITION = Schema.mapBuilder(ShapeId.from("io.superposition#Condition"))
        .putMember("key", PreludeSchemas.STRING)
        .putMember("value", PreludeSchemas.DOCUMENT)
        .build();

    static final Schema DATE_TIME = Schema.createTimestamp(ShapeId.from("io.superposition#DateTime"),
            new TimestampFormatTrait("date-time"));

    static final Schema LIST_VARIANT = Schema.listBuilder(ShapeId.from("io.superposition#ListVariant"))
        .putMember("member", Variant.$SCHEMA)
        .build();

    static final Schema AUDIT_LOG_LIST = Schema.listBuilder(ShapeId.from("io.superposition#AuditLogList"))
        .putMember("member", AuditLogFull.$SCHEMA)
        .build();

    static final Schema OVERRIDES = Schema.mapBuilder(ShapeId.from("io.superposition#Overrides"))
        .putMember("key", PreludeSchemas.STRING)
        .putMember("value", PreludeSchemas.DOCUMENT)
        .build();

    static final Schema BULK_OPERATION_LIST = Schema.listBuilder(ShapeId.from("io.superposition#BulkOperationList"))
        .putMember("member", ContextAction.$SCHEMA)
        .build();

    static final Schema WEIGHT = Schema.createString(ShapeId.from("io.superposition#Weight"));

    static final Schema BULK_OPERATION_OUT_LIST = Schema.listBuilder(ShapeId.from("io.superposition#BulkOperationOutList"))
        .putMember("member", ContextActionOut.$SCHEMA)
        .build();

    static final Schema LIST_OVERRIDE_KEYS = Schema.listBuilder(ShapeId.from("io.superposition#ListOverrideKeys"))
        .putMember("member", PreludeSchemas.STRING)
        .build();

    static final Schema CONTEXT_MAP = Schema.mapBuilder(ShapeId.from("io.superposition#ContextMap"))
        .putMember("key", PreludeSchemas.STRING)
        .putMember("value", PreludeSchemas.DOCUMENT)
        .build();

    static final Schema OVERRIDE_WITH_KEYS = Schema.listBuilder(ShapeId.from("io.superposition#OverrideWithKeys"),
            LengthTrait.builder().max(1L).build())
        .putMember("member", PreludeSchemas.STRING)
        .build();

    static final Schema CONTEXT_LIST = Schema.listBuilder(ShapeId.from("io.superposition#ContextList"))
        .putMember("member", ContextPartial.$SCHEMA)
        .build();

    static final Schema OBJECT = Schema.mapBuilder(ShapeId.from("io.superposition#Object"))
        .putMember("key", PreludeSchemas.STRING)
        .putMember("value", PreludeSchemas.DOCUMENT)
        .build();

    static final Schema OVERRIDES_MAP = Schema.mapBuilder(ShapeId.from("io.superposition#OverridesMap"))
        .putMember("key", PreludeSchemas.STRING)
        .putMember("value", SharedSchemas.OVERRIDES)
        .build();

    static final Schema LIST_VERSIONS_OUT = Schema.listBuilder(ShapeId.from("io.superposition#ListVersionsOut"))
        .putMember("member", ListVersionsMember.$SCHEMA)
        .build();

    static final Schema LIST_CONTEXT_OUT = Schema.listBuilder(ShapeId.from("io.superposition#ListContextOut"))
        .putMember("member", ContextResponse.$SCHEMA)
        .build();

    static final Schema WEIGHT_RECOMPUTE_RESPONSES = Schema.listBuilder(ShapeId.from("io.superposition#WeightRecomputeResponses"))
        .putMember("member", WeightRecomputeResponse.$SCHEMA)
        .build();

    static final Schema EVENTS = Schema.listBuilder(ShapeId.from("io.superposition#Events"))
        .putMember("member", PreludeSchemas.STRING)
        .build();

    static final Schema LIST_MANDATORY_DIMENSIONS = Schema.listBuilder(ShapeId.from("io.superposition#ListMandatoryDimensions"))
        .putMember("member", PreludeSchemas.STRING)
        .build();

    static final Schema LIST_DEFAULT_CONFIG_OUT = Schema.listBuilder(ShapeId.from("io.superposition#ListDefaultConfigOut"))
        .putMember("member", DefaultConfigFull.$SCHEMA)
        .build();

    static final Schema DIMENSION_EXT_LIST = Schema.listBuilder(ShapeId.from("io.superposition#DimensionExtList"))
        .putMember("member", DimensionExt.$SCHEMA)
        .build();

    static final Schema EXPERIMENT_GROUP_LIST = Schema.listBuilder(ShapeId.from("io.superposition#ExperimentGroupList"))
        .putMember("member", ExperimentGroupResponse.$SCHEMA)
        .build();

    static final Schema EXPERIMENT_LIST = Schema.listBuilder(ShapeId.from("io.superposition#ExperimentList"))
        .putMember("member", ExperimentResponse.$SCHEMA)
        .build();

    static final Schema LIST_VARIANT_UPDATE_REQUEST = Schema.listBuilder(ShapeId.from("io.superposition#ListVariantUpdateRequest"))
        .putMember("member", VariantUpdateRequest.$SCHEMA)
        .build();

    static final Schema FUNCTION_LIST_RESPONSE = Schema.listBuilder(ShapeId.from("io.superposition#FunctionListResponse"))
        .putMember("member", FunctionResponse.$SCHEMA)
        .build();

    static final Schema TYPE_TEMPLATES_LIST = Schema.listBuilder(ShapeId.from("io.superposition#TypeTemplatesList"))
        .putMember("member", TypeTemplatesResponse.$SCHEMA)
        .build();

    static final Schema ORGANISATION_LIST = Schema.listBuilder(ShapeId.from("io.superposition#OrganisationList"))
        .putMember("member", OrganisationResponse.$SCHEMA)
        .build();

    static final Schema WEBHOOK_LIST = Schema.listBuilder(ShapeId.from("io.superposition#WebhookList"))
        .putMember("member", WebhookResponse.$SCHEMA)
        .build();

    static final Schema WORKSPACE_LIST = Schema.listBuilder(ShapeId.from("io.superposition#WorkspaceList"))
        .putMember("member", WorkspaceResponse.$SCHEMA)
        .build();

    private SharedSchemas() {}
}

