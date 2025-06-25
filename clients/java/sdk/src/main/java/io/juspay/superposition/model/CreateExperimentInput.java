
package io.juspay.superposition.model;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.PresenceTracker;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class CreateExperimentInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateExperimentRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("experiment_type", ExperimentType.$SCHEMA)
        .putMember("context", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("variants", SharedSchemas.LIST_VARIANT,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("metrics", PreludeSchemas.DOCUMENT)
        .putMember("experiment_group_id", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_EXPERIMENT_TYPE = $SCHEMA.member("experiment_type");
    private static final Schema $SCHEMA_CONTEXT = $SCHEMA.member("context");
    private static final Schema $SCHEMA_VARIANTS = $SCHEMA.member("variants");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_METRICS = $SCHEMA.member("metrics");
    private static final Schema $SCHEMA_EXPERIMENT_GROUP_ID = $SCHEMA.member("experiment_group_id");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String name;
    private final transient ExperimentType experimentType;
    private final transient Map<String, Document> context;
    private final transient List<Variant> variants;
    private final transient String description;
    private final transient String changeReason;
    private final transient Document metrics;
    private final transient String experimentGroupId;

    private CreateExperimentInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.name = builder.name;
        this.experimentType = builder.experimentType;
        this.context = Collections.unmodifiableMap(builder.context);
        this.variants = Collections.unmodifiableList(builder.variants);
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.metrics = builder.metrics;
        this.experimentGroupId = builder.experimentGroupId;
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public String name() {
        return name;
    }

    public ExperimentType experimentType() {
        return experimentType;
    }

    public Map<String, Document> context() {
        return context;
    }

    public boolean hasContext() {
        return true;
    }

    public List<Variant> variants() {
        return variants;
    }

    public boolean hasVariants() {
        return true;
    }

    public String description() {
        return description;
    }

    public String changeReason() {
        return changeReason;
    }

    public Document metrics() {
        return metrics;
    }

    public String experimentGroupId() {
        return experimentGroupId;
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }
        CreateExperimentInput that = (CreateExperimentInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.experimentType, that.experimentType)
               && Objects.equals(this.context, that.context)
               && Objects.equals(this.variants, that.variants)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.metrics, that.metrics)
               && Objects.equals(this.experimentGroupId, that.experimentGroupId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, name, experimentType, context, variants, description, changeReason, metrics, experimentGroupId);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_NAME, name);
        if (experimentType != null) {
            serializer.writeString($SCHEMA_EXPERIMENT_TYPE, experimentType.value());
        }
        serializer.writeMap($SCHEMA_CONTEXT, context, context.size(), SharedSerde.ConditionSerializer.INSTANCE);
        serializer.writeList($SCHEMA_VARIANTS, variants, variants.size(), SharedSerde.ListVariantSerializer.INSTANCE);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        if (metrics != null) {
            serializer.writeDocument($SCHEMA_METRICS, metrics);
        }
        if (experimentGroupId != null) {
            serializer.writeString($SCHEMA_EXPERIMENT_GROUP_ID, experimentGroupId);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, context);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_VARIANTS, member, variants);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_TYPE, member, experimentType);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, metrics);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_ID, member, experimentGroupId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateExperimentInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.name(this.name);
        builder.experimentType(this.experimentType);
        builder.context(this.context);
        builder.variants(this.variants);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        builder.metrics(this.metrics);
        builder.experimentGroupId(this.experimentGroupId);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateExperimentInput}.
     */
    public static final class Builder implements ShapeBuilder<CreateExperimentInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String name;
        private ExperimentType experimentType;
        private Map<String, Document> context;
        private List<Variant> variants;
        private String description;
        private String changeReason;
        private Document metrics;
        private String experimentGroupId;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder workspaceId(String workspaceId) {
            this.workspaceId = Objects.requireNonNull(workspaceId, "workspaceId cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder orgId(String orgId) {
            this.orgId = Objects.requireNonNull(orgId, "orgId cannot be null");
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder name(String name) {
            this.name = Objects.requireNonNull(name, "name cannot be null");
            tracker.setMember($SCHEMA_NAME);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder experimentType(ExperimentType experimentType) {
            this.experimentType = experimentType;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder context(Map<String, Document> context) {
            this.context = Objects.requireNonNull(context, "context cannot be null");
            tracker.setMember($SCHEMA_CONTEXT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder variants(List<Variant> variants) {
            this.variants = Objects.requireNonNull(variants, "variants cannot be null");
            tracker.setMember($SCHEMA_VARIANTS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder description(String description) {
            this.description = Objects.requireNonNull(description, "description cannot be null");
            tracker.setMember($SCHEMA_DESCRIPTION);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder changeReason(String changeReason) {
            this.changeReason = Objects.requireNonNull(changeReason, "changeReason cannot be null");
            tracker.setMember($SCHEMA_CHANGE_REASON);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder metrics(Document metrics) {
            this.metrics = metrics;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder experimentGroupId(String experimentGroupId) {
            this.experimentGroupId = experimentGroupId;
            return this;
        }

        @Override
        public CreateExperimentInput build() {
            tracker.validate();
            return new CreateExperimentInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 2 -> context((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, value));
                case 3 -> variants((List<Variant>) SchemaUtils.validateSameMember($SCHEMA_VARIANTS, member, value));
                case 4 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 5 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 6 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 7 -> experimentType((ExperimentType) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_TYPE, member, value));
                case 8 -> metrics((Document) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, value));
                case 9 -> experimentGroupId((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateExperimentInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT)) {
                context(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_VARIANTS)) {
                variants(Collections.emptyList());
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
            }
            return this;
        }

        @Override
        public Builder deserialize(ShapeDeserializer decoder) {
            decoder.readStruct($SCHEMA, this, $InnerDeserializer.INSTANCE);
            return this;
        }

        @Override
        public Builder deserializeMember(ShapeDeserializer decoder, Schema schema) {
            decoder.readStruct(schema.assertMemberTargetIs($SCHEMA), this, $InnerDeserializer.INSTANCE);
            return this;
        }

        private static final class $InnerDeserializer implements ShapeDeserializer.StructMemberConsumer<Builder> {
            private static final $InnerDeserializer INSTANCE = new $InnerDeserializer();

            @Override
            public void accept(Builder builder, Schema member, ShapeDeserializer de) {
                switch (member.memberIndex()) {
                    case 0 -> builder.workspaceId(de.readString(member));
                    case 1 -> builder.name(de.readString(member));
                    case 2 -> builder.context(SharedSerde.deserializeCondition(member, de));
                    case 3 -> builder.variants(SharedSerde.deserializeListVariant(member, de));
                    case 4 -> builder.description(de.readString(member));
                    case 5 -> builder.changeReason(de.readString(member));
                    case 6 -> builder.orgId(de.readString(member));
                    case 7 -> builder.experimentType(ExperimentType.builder().deserializeMember(de, member).build());
                    case 8 -> builder.metrics(de.readDocument());
                    case 9 -> builder.experimentGroupId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

