
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class BulkOperationInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#BulkOperationInput");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-workspace"),
                new RequiredTrait())
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("config_tags", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-config-tags"))
        .putMember("operations", SharedSchemas.BULK_OPERATION_LIST,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_ID = $SCHEMA.member("workspace_id");
    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_CONFIG_TAGS = $SCHEMA.member("config_tags");
    private static final Schema $SCHEMA_OPERATIONS = $SCHEMA.member("operations");

    private final transient String workspaceId;
    private final transient String orgId;
    private final transient String configTags;
    private final transient List<ContextAction> operations;

    private BulkOperationInput(Builder builder) {
        this.workspaceId = builder.workspaceId;
        this.orgId = builder.orgId;
        this.configTags = builder.configTags;
        this.operations = Collections.unmodifiableList(builder.operations);
    }

    public String workspaceId() {
        return workspaceId;
    }

    public String orgId() {
        return orgId;
    }

    public String configTags() {
        return configTags;
    }

    public List<ContextAction> operations() {
        return operations;
    }

    public boolean hasOperations() {
        return true;
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
        BulkOperationInput that = (BulkOperationInput) other;
        return Objects.equals(this.workspaceId, that.workspaceId)
               && Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.configTags, that.configTags)
               && Objects.equals(this.operations, that.operations);
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceId, orgId, configTags, operations);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_ID, workspaceId);
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        if (configTags != null) {
            serializer.writeString($SCHEMA_CONFIG_TAGS, configTags);
        }
        serializer.writeList($SCHEMA_OPERATIONS, operations, operations.size(), SharedSerde.BulkOperationListSerializer.INSTANCE);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, workspaceId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_OPERATIONS, member, operations);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG_TAGS, member, configTags);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link BulkOperationInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceId(this.workspaceId);
        builder.orgId(this.orgId);
        builder.configTags(this.configTags);
        builder.operations(this.operations);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link BulkOperationInput}.
     */
    public static final class Builder implements ShapeBuilder<BulkOperationInput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceId;
        private String orgId;
        private String configTags;
        private List<ContextAction> operations;

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
            tracker.setMember($SCHEMA_ORG_ID);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder configTags(String configTags) {
            this.configTags = configTags;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder operations(List<ContextAction> operations) {
            this.operations = Objects.requireNonNull(operations, "operations cannot be null");
            tracker.setMember($SCHEMA_OPERATIONS);
            return this;
        }

        @Override
        public BulkOperationInput build() {
            tracker.validate();
            return new BulkOperationInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceId((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ID, member, value));
                case 1 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 2 -> operations((List<ContextAction>) SchemaUtils.validateSameMember($SCHEMA_OPERATIONS, member, value));
                case 3 -> configTags((String) SchemaUtils.validateSameMember($SCHEMA_CONFIG_TAGS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<BulkOperationInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ID)) {
                workspaceId("");
            }
            if (!tracker.checkMember($SCHEMA_ORG_ID)) {
                orgId("");
            }
            if (!tracker.checkMember($SCHEMA_OPERATIONS)) {
                operations(Collections.emptyList());
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
                    case 1 -> builder.orgId(de.readString(member));
                    case 2 -> builder.operations(SharedSerde.deserializeBulkOperationList(member, de));
                    case 3 -> builder.configTags(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

