
package io.juspay.superposition.model;

import java.math.BigDecimal;
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
import software.amazon.smithy.model.traits.RangeTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Input structure for creating a new experiment group.
 */
@SmithyGenerated
public final class CreateExperimentGroupInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateExperimentGroupRequest");

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
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("context", SharedSchemas.CONDITION,
                new RequiredTrait())
        .putMember("traffic_percentage", PreludeSchemas.INTEGER,
                RangeTrait.builder().min(new BigDecimal("0")).max(new BigDecimal("100")).build(),
                new RequiredTrait())
        .putMember("member_experiment_ids", SharedSchemas.STRING_LIST)
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_CONTEXT = $SCHEMA.member("context");
    private static final Schema $SCHEMA_TRAFFIC_PERCENTAGE = $SCHEMA.member("traffic_percentage");
    private static final Schema $SCHEMA_MEMBER_EXPERIMENT_IDS = $SCHEMA.member("member_experiment_ids");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String name;
    private final transient String description;
    private final transient String changeReason;
    private final transient Map<String, Document> context;
    private final transient int trafficPercentage;
    private final transient List<String> memberExperimentIds;

    private CreateExperimentGroupInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.name = builder.name;
        this.description = builder.description;
        this.changeReason = builder.changeReason;
        this.context = Collections.unmodifiableMap(builder.context);
        this.trafficPercentage = builder.trafficPercentage;
        this.memberExperimentIds = builder.memberExperimentIds == null ? null : Collections.unmodifiableList(builder.memberExperimentIds);
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

    public String description() {
        return description;
    }

    /**
     * Reason for creating this experiment group.
     */
    public String changeReason() {
        return changeReason;
    }

    public Map<String, Document> context() {
        return context;
    }

    public boolean hasContext() {
        return true;
    }

    public int trafficPercentage() {
        return trafficPercentage;
    }

    /**
     * List of experiment IDs that are members of this group.
     */
    public List<String> memberExperimentIds() {
        if (memberExperimentIds == null) {
            return Collections.emptyList();
        }
        return memberExperimentIds;
    }

    public boolean hasMemberExperimentIds() {
        return memberExperimentIds != null;
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
        CreateExperimentGroupInput that = (CreateExperimentGroupInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.name, that.name)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.context, that.context)
               && this.trafficPercentage == that.trafficPercentage
               && Objects.equals(this.memberExperimentIds, that.memberExperimentIds);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, name, description, changeReason, context, trafficPercentage, memberExperimentIds);
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
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        serializer.writeMap($SCHEMA_CONTEXT, context, context.size(), SharedSerde.ConditionSerializer.INSTANCE);
        serializer.writeInteger($SCHEMA_TRAFFIC_PERCENTAGE, trafficPercentage);
        if (memberExperimentIds != null) {
            serializer.writeList($SCHEMA_MEMBER_EXPERIMENT_IDS, memberExperimentIds, memberExperimentIds.size(), SharedSerde.StringListSerializer.INSTANCE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, context);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_TRAFFIC_PERCENTAGE, member, trafficPercentage);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_MEMBER_EXPERIMENT_IDS, member, memberExperimentIds);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateExperimentGroupInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.name(this.name);
        builder.description(this.description);
        builder.changeReason(this.changeReason);
        builder.context(this.context);
        builder.trafficPercentage(this.trafficPercentage);
        builder.memberExperimentIds(this.memberExperimentIds);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateExperimentGroupInput}.
     */
    public static final class Builder implements ShapeBuilder<CreateExperimentGroupInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String name;
        private String description;
        private String changeReason;
        private Map<String, Document> context;
        private int trafficPercentage;
        private List<String> memberExperimentIds;

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
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder description(String description) {
            this.description = Objects.requireNonNull(description, "description cannot be null");
            tracker.setMember($SCHEMA_DESCRIPTION);
            return this;
        }

        /**
         * Reason for creating this experiment group.
         *
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder changeReason(String changeReason) {
            this.changeReason = Objects.requireNonNull(changeReason, "changeReason cannot be null");
            tracker.setMember($SCHEMA_CHANGE_REASON);
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
        public Builder trafficPercentage(int trafficPercentage) {
            this.trafficPercentage = trafficPercentage;
            tracker.setMember($SCHEMA_TRAFFIC_PERCENTAGE);
            return this;
        }

        /**
         * List of experiment IDs that are members of this group.
         *
         * @return this builder.
         */
        public Builder memberExperimentIds(List<String> memberExperimentIds) {
            this.memberExperimentIds = memberExperimentIds;
            return this;
        }

        @Override
        public CreateExperimentGroupInput build() {
            tracker.validate();
            return new CreateExperimentGroupInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 2 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 3 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 4 -> context((Map<String, Document>) SchemaUtils.validateSameMember($SCHEMA_CONTEXT, member, value));
                case 5 -> trafficPercentage((int) SchemaUtils.validateSameMember($SCHEMA_TRAFFIC_PERCENTAGE, member, value));
                case 6 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 7 -> memberExperimentIds((List<String>) SchemaUtils.validateSameMember($SCHEMA_MEMBER_EXPERIMENT_IDS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateExperimentGroupInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_CHANGE_REASON)) {
                changeReason("");
            }
            if (!tracker.checkMember($SCHEMA_CONTEXT)) {
                context(Collections.emptyMap());
            }
            if (!tracker.checkMember($SCHEMA_TRAFFIC_PERCENTAGE)) {
                tracker.setMember($SCHEMA_TRAFFIC_PERCENTAGE);
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
                    case 2 -> builder.description(de.readString(member));
                    case 3 -> builder.changeReason(de.readString(member));
                    case 4 -> builder.context(SharedSerde.deserializeCondition(member, de));
                    case 5 -> builder.trafficPercentage(de.readInteger(member));
                    case 6 -> builder.orgId(de.readString(member));
                    case 7 -> builder.memberExperimentIds(SharedSerde.deserializeStringList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

