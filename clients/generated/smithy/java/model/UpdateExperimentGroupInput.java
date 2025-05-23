
package io.juspay.superposition.model;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
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
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.RangeTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Input structure for updating an existing experiment group.
 */
@SmithyGenerated
public final class UpdateExperimentGroupInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateExperimentGroupRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("experiment_group_id", PreludeSchemas.STRING,
                new HttpLabelTrait(),
                new RequiredTrait())
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("traffic_percentage", PreludeSchemas.INTEGER,
                RangeTrait.builder().min(new BigDecimal("0")).max(new BigDecimal("100")).build())
        .putMember("member_experiment_ids", SharedSchemas.STRING_LIST)
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_EXPERIMENT_GROUP_ID = $SCHEMA.member("experiment_group_id");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_TRAFFIC_PERCENTAGE = $SCHEMA.member("traffic_percentage");
    private static final Schema $SCHEMA_MEMBER_EXPERIMENT_IDS = $SCHEMA.member("member_experiment_ids");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String experimentGroupId;
    private final transient String changeReason;
    private final transient Integer trafficPercentage;
    private final transient List<String> memberExperimentIds;

    private UpdateExperimentGroupInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.experimentGroupId = builder.experimentGroupId;
        this.changeReason = builder.changeReason;
        this.trafficPercentage = builder.trafficPercentage;
        this.memberExperimentIds = builder.memberExperimentIds == null ? null : Collections.unmodifiableList(builder.memberExperimentIds);
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public String experimentGroupId() {
        return experimentGroupId;
    }

    /**
     * Reason for this update.
     */
    public String changeReason() {
        return changeReason;
    }

    /**
     * Optional new traffic percentage for the group.
     */
    public Integer trafficPercentage() {
        return trafficPercentage;
    }

    /**
     * Optional new list of member experiment IDs. Replaces the existing list if provided.
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
        UpdateExperimentGroupInput that = (UpdateExperimentGroupInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.experimentGroupId, that.experimentGroupId)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.trafficPercentage, that.trafficPercentage)
               && Objects.equals(this.memberExperimentIds, that.memberExperimentIds);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, experimentGroupId, changeReason, trafficPercentage, memberExperimentIds);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_EXPERIMENT_GROUP_ID, experimentGroupId);
        serializer.writeString($SCHEMA_CHANGE_REASON, changeReason);
        if (trafficPercentage != null) {
            serializer.writeInteger($SCHEMA_TRAFFIC_PERCENTAGE, trafficPercentage);
        }
        if (memberExperimentIds != null) {
            serializer.writeList($SCHEMA_MEMBER_EXPERIMENT_IDS, memberExperimentIds, memberExperimentIds.size(), SharedSerde.StringListSerializer.INSTANCE);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_ID, member, experimentGroupId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_TRAFFIC_PERCENTAGE, member, trafficPercentage);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_MEMBER_EXPERIMENT_IDS, member, memberExperimentIds);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link UpdateExperimentGroupInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.experimentGroupId(this.experimentGroupId);
        builder.changeReason(this.changeReason);
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
     * Builder for {@link UpdateExperimentGroupInput}.
     */
    public static final class Builder implements ShapeBuilder<UpdateExperimentGroupInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String experimentGroupId;
        private String changeReason;
        private Integer trafficPercentage;
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
        public Builder experimentGroupId(String experimentGroupId) {
            this.experimentGroupId = Objects.requireNonNull(experimentGroupId, "experimentGroupId cannot be null");
            tracker.setMember($SCHEMA_EXPERIMENT_GROUP_ID);
            return this;
        }

        /**
         * Reason for this update.
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
         * Optional new traffic percentage for the group.
         *
         * @return this builder.
         */
        public Builder trafficPercentage(int trafficPercentage) {
            this.trafficPercentage = trafficPercentage;
            return this;
        }

        /**
         * Optional new list of member experiment IDs. Replaces the existing list if provided.
         *
         * @return this builder.
         */
        public Builder memberExperimentIds(List<String> memberExperimentIds) {
            this.memberExperimentIds = memberExperimentIds;
            return this;
        }

        @Override
        public UpdateExperimentGroupInput build() {
            tracker.validate();
            return new UpdateExperimentGroupInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> experimentGroupId((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_ID, member, value));
                case 2 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 3 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 4 -> trafficPercentage((int) SchemaUtils.validateSameMember($SCHEMA_TRAFFIC_PERCENTAGE, member, value));
                case 5 -> memberExperimentIds((List<String>) SchemaUtils.validateSameMember($SCHEMA_MEMBER_EXPERIMENT_IDS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<UpdateExperimentGroupInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_EXPERIMENT_GROUP_ID)) {
                experimentGroupId("");
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
                    case 1 -> builder.experimentGroupId(de.readString(member));
                    case 2 -> builder.changeReason(de.readString(member));
                    case 3 -> builder.orgId(de.readString(member));
                    case 4 -> builder.trafficPercentage(de.readInteger(member));
                    case 5 -> builder.memberExperimentIds(SharedSerde.deserializeStringList(member, de));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

