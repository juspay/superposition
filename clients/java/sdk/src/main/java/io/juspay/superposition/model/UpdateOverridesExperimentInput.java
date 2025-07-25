
package io.juspay.superposition.model;

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
import software.amazon.smithy.java.core.serde.document.Document;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.DefaultTrait;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.HttpLabelTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class UpdateOverridesExperimentInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateOverrideRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-tenant"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("id", PreludeSchemas.STRING,
                new HttpLabelTrait(),
                new RequiredTrait())
        .putMember("variant_list", SharedSchemas.LIST_VARIANT_UPDATE_REQUEST,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING)
        .putMember("change_reason", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("metrics", PreludeSchemas.DOCUMENT)
        .putMember("experiment_group_id", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_VARIANT_LIST = $SCHEMA.member("variant_list");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_CHANGE_REASON = $SCHEMA.member("change_reason");
    private static final Schema $SCHEMA_METRICS = $SCHEMA.member("metrics");
    private static final Schema $SCHEMA_EXPERIMENT_GROUP_ID = $SCHEMA.member("experiment_group_id");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String id;
    private final transient List<VariantUpdateRequest> variantList;
    private final transient String description;
    private final transient String changeReason;
    private final transient Document metrics;
    private final transient String experimentGroupId;

    private UpdateOverridesExperimentInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.id = builder.id;
        this.variantList = Collections.unmodifiableList(builder.variantList);
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

    public String id() {
        return id;
    }

    public List<VariantUpdateRequest> variantList() {
        return variantList;
    }

    public boolean hasVariantList() {
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
        UpdateOverridesExperimentInput that = (UpdateOverridesExperimentInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.id, that.id)
               && Objects.equals(this.variantList, that.variantList)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.changeReason, that.changeReason)
               && Objects.equals(this.metrics, that.metrics)
               && Objects.equals(this.experimentGroupId, that.experimentGroupId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, id, variantList, description, changeReason, metrics, experimentGroupId);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeList($SCHEMA_VARIANT_LIST, variantList, variantList.size(), SharedSerde.ListVariantUpdateRequestSerializer.INSTANCE);
        if (description != null) {
            serializer.writeString($SCHEMA_DESCRIPTION, description);
        }
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
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_VARIANT_LIST, member, variantList);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, changeReason);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, metrics);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_ID, member, experimentGroupId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link UpdateOverridesExperimentInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.id(this.id);
        builder.variantList(this.variantList);
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
     * Builder for {@link UpdateOverridesExperimentInput}.
     */
    public static final class Builder implements ShapeBuilder<UpdateOverridesExperimentInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId = ORG_ID_DEFAULT;
        private String id;
        private List<VariantUpdateRequest> variantList;
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
        public Builder id(String id) {
            this.id = Objects.requireNonNull(id, "id cannot be null");
            tracker.setMember($SCHEMA_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder variantList(List<VariantUpdateRequest> variantList) {
            this.variantList = Objects.requireNonNull(variantList, "variantList cannot be null");
            tracker.setMember($SCHEMA_VARIANT_LIST);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder description(String description) {
            this.description = description;
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
        public UpdateOverridesExperimentInput build() {
            tracker.validate();
            return new UpdateOverridesExperimentInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 2 -> variantList((List<VariantUpdateRequest>) SchemaUtils.validateSameMember($SCHEMA_VARIANT_LIST, member, value));
                case 3 -> changeReason((String) SchemaUtils.validateSameMember($SCHEMA_CHANGE_REASON, member, value));
                case 4 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 5 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 6 -> metrics((Document) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, value));
                case 7 -> experimentGroupId((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_GROUP_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<UpdateOverridesExperimentInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_VARIANT_LIST)) {
                variantList(Collections.emptyList());
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
                    case 1 -> builder.id(de.readString(member));
                    case 2 -> builder.variantList(SharedSerde.deserializeListVariantUpdateRequest(member, de));
                    case 3 -> builder.changeReason(de.readString(member));
                    case 4 -> builder.orgId(de.readString(member));
                    case 5 -> builder.description(de.readString(member));
                    case 6 -> builder.metrics(de.readDocument());
                    case 7 -> builder.experimentGroupId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

