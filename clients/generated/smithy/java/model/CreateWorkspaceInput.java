
package io.juspay.superposition.model;

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
public final class CreateWorkspaceInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateWorkspaceRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("workspace_admin_email", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("workspace_name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("workspace_status", WorkspaceStatus.$SCHEMA)
        .putMember("strict_mode", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .putMember("metrics", PreludeSchemas.DOCUMENT)
        .putMember("allow_experiment_self_approval", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_WORKSPACE_ADMIN_EMAIL = $SCHEMA.member("workspace_admin_email");
    private static final Schema $SCHEMA_WORKSPACE_NAME = $SCHEMA.member("workspace_name");
    private static final Schema $SCHEMA_WORKSPACE_STATUS = $SCHEMA.member("workspace_status");
    private static final Schema $SCHEMA_STRICT_MODE = $SCHEMA.member("strict_mode");
    private static final Schema $SCHEMA_METRICS = $SCHEMA.member("metrics");
    private static final Schema $SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL = $SCHEMA.member("allow_experiment_self_approval");

    private final transient String orgId;
    private final transient String workspaceAdminEmail;
    private final transient String workspaceName;
    private final transient WorkspaceStatus workspaceStatus;
    private final transient boolean strictMode;
    private final transient Document metrics;
    private final transient boolean allowExperimentSelfApproval;

    private CreateWorkspaceInput(Builder builder) {
        this.orgId = builder.orgId;
        this.workspaceAdminEmail = builder.workspaceAdminEmail;
        this.workspaceName = builder.workspaceName;
        this.workspaceStatus = builder.workspaceStatus;
        this.strictMode = builder.strictMode;
        this.metrics = builder.metrics;
        this.allowExperimentSelfApproval = builder.allowExperimentSelfApproval;
    }

    public String orgId() {
        return orgId;
    }

    public String workspaceAdminEmail() {
        return workspaceAdminEmail;
    }

    public String workspaceName() {
        return workspaceName;
    }

    public WorkspaceStatus workspaceStatus() {
        return workspaceStatus;
    }

    public boolean strictMode() {
        return strictMode;
    }

    public Document metrics() {
        return metrics;
    }

    public boolean allowExperimentSelfApproval() {
        return allowExperimentSelfApproval;
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
        CreateWorkspaceInput that = (CreateWorkspaceInput) other;
        return Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.workspaceAdminEmail, that.workspaceAdminEmail)
               && Objects.equals(this.workspaceName, that.workspaceName)
               && Objects.equals(this.workspaceStatus, that.workspaceStatus)
               && this.strictMode == that.strictMode
               && Objects.equals(this.metrics, that.metrics)
               && this.allowExperimentSelfApproval == that.allowExperimentSelfApproval;
    }

    @Override
    public int hashCode() {
        return Objects.hash(orgId, workspaceAdminEmail, workspaceName, workspaceStatus, strictMode, metrics, allowExperimentSelfApproval);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_WORKSPACE_ADMIN_EMAIL, workspaceAdminEmail);
        serializer.writeString($SCHEMA_WORKSPACE_NAME, workspaceName);
        if (workspaceStatus != null) {
            serializer.writeString($SCHEMA_WORKSPACE_STATUS, workspaceStatus.value());
        }
        serializer.writeBoolean($SCHEMA_STRICT_MODE, strictMode);
        if (metrics != null) {
            serializer.writeDocument($SCHEMA_METRICS, metrics);
        }
        serializer.writeBoolean($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, allowExperimentSelfApproval);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ADMIN_EMAIL, member, workspaceAdminEmail);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_NAME, member, workspaceName);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_STRICT_MODE, member, strictMode);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, member, allowExperimentSelfApproval);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_STATUS, member, workspaceStatus);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, metrics);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateWorkspaceInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.orgId(this.orgId);
        builder.workspaceAdminEmail(this.workspaceAdminEmail);
        builder.workspaceName(this.workspaceName);
        builder.workspaceStatus(this.workspaceStatus);
        builder.strictMode(this.strictMode);
        builder.metrics(this.metrics);
        builder.allowExperimentSelfApproval(this.allowExperimentSelfApproval);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateWorkspaceInput}.
     */
    public static final class Builder implements ShapeBuilder<CreateWorkspaceInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String orgId = ORG_ID_DEFAULT;
        private String workspaceAdminEmail;
        private String workspaceName;
        private WorkspaceStatus workspaceStatus;
        private boolean strictMode;
        private Document metrics;
        private boolean allowExperimentSelfApproval;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
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
        public Builder workspaceAdminEmail(String workspaceAdminEmail) {
            this.workspaceAdminEmail = Objects.requireNonNull(workspaceAdminEmail, "workspaceAdminEmail cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_ADMIN_EMAIL);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder workspaceName(String workspaceName) {
            this.workspaceName = Objects.requireNonNull(workspaceName, "workspaceName cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_NAME);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder workspaceStatus(WorkspaceStatus workspaceStatus) {
            this.workspaceStatus = workspaceStatus;
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder strictMode(boolean strictMode) {
            this.strictMode = strictMode;
            tracker.setMember($SCHEMA_STRICT_MODE);
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
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder allowExperimentSelfApproval(boolean allowExperimentSelfApproval) {
            this.allowExperimentSelfApproval = allowExperimentSelfApproval;
            tracker.setMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL);
            return this;
        }

        @Override
        public CreateWorkspaceInput build() {
            tracker.validate();
            return new CreateWorkspaceInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceAdminEmail((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ADMIN_EMAIL, member, value));
                case 1 -> workspaceName((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_NAME, member, value));
                case 2 -> strictMode((boolean) SchemaUtils.validateSameMember($SCHEMA_STRICT_MODE, member, value));
                case 3 -> allowExperimentSelfApproval((boolean) SchemaUtils.validateSameMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, member, value));
                case 4 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 5 -> workspaceStatus((WorkspaceStatus) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_STATUS, member, value));
                case 6 -> metrics((Document) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateWorkspaceInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ADMIN_EMAIL)) {
                workspaceAdminEmail("");
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_NAME)) {
                workspaceName("");
            }
            if (!tracker.checkMember($SCHEMA_STRICT_MODE)) {
                tracker.setMember($SCHEMA_STRICT_MODE);
            }
            if (!tracker.checkMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL)) {
                tracker.setMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL);
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
                    case 0 -> builder.workspaceAdminEmail(de.readString(member));
                    case 1 -> builder.workspaceName(de.readString(member));
                    case 2 -> builder.strictMode(de.readBoolean(member));
                    case 3 -> builder.allowExperimentSelfApproval(de.readBoolean(member));
                    case 4 -> builder.orgId(de.readString(member));
                    case 5 -> builder.workspaceStatus(WorkspaceStatus.builder().deserializeMember(de, member).build());
                    case 6 -> builder.metrics(de.readDocument());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

