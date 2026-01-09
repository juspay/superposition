
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.HttpHeaderTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class CreateWorkspaceInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateWorkspaceRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("org_id", PreludeSchemas.STRING,
                new HttpHeaderTrait("x-org-id"),
                new RequiredTrait())
        .putMember("workspace_admin_email", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("workspace_name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("workspace_status", WorkspaceStatus.$SCHEMA)
        .putMember("metrics", PreludeSchemas.DOCUMENT)
        .putMember("allow_experiment_self_approval", PreludeSchemas.BOOLEAN)
        .putMember("auto_populate_control", PreludeSchemas.BOOLEAN)
        .putMember("enable_context_validation", PreludeSchemas.BOOLEAN)
        .putMember("enable_change_reason_validation", PreludeSchemas.BOOLEAN)
        .build();

    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_WORKSPACE_ADMIN_EMAIL = $SCHEMA.member("workspace_admin_email");
    private static final Schema $SCHEMA_WORKSPACE_NAME = $SCHEMA.member("workspace_name");
    private static final Schema $SCHEMA_WORKSPACE_STATUS = $SCHEMA.member("workspace_status");
    private static final Schema $SCHEMA_METRICS = $SCHEMA.member("metrics");
    private static final Schema $SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL = $SCHEMA.member("allow_experiment_self_approval");
    private static final Schema $SCHEMA_AUTO_POPULATE_CONTROL = $SCHEMA.member("auto_populate_control");
    private static final Schema $SCHEMA_ENABLE_CONTEXT_VALIDATION = $SCHEMA.member("enable_context_validation");
    private static final Schema $SCHEMA_ENABLE_CHANGE_REASON_VALIDATION = $SCHEMA.member("enable_change_reason_validation");

    private final transient String orgId;
    private final transient String workspaceAdminEmail;
    private final transient String workspaceName;
    private final transient WorkspaceStatus workspaceStatus;
    private final transient Document metrics;
    private final transient Boolean allowExperimentSelfApproval;
    private final transient Boolean autoPopulateControl;
    private final transient Boolean enableContextValidation;
    private final transient Boolean enableChangeReasonValidation;

    private CreateWorkspaceInput(Builder builder) {
        this.orgId = builder.orgId;
        this.workspaceAdminEmail = builder.workspaceAdminEmail;
        this.workspaceName = builder.workspaceName;
        this.workspaceStatus = builder.workspaceStatus;
        this.metrics = builder.metrics;
        this.allowExperimentSelfApproval = builder.allowExperimentSelfApproval;
        this.autoPopulateControl = builder.autoPopulateControl;
        this.enableContextValidation = builder.enableContextValidation;
        this.enableChangeReasonValidation = builder.enableChangeReasonValidation;
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

    public Document metrics() {
        return metrics;
    }

    public Boolean allowExperimentSelfApproval() {
        return allowExperimentSelfApproval;
    }

    public Boolean autoPopulateControl() {
        return autoPopulateControl;
    }

    public Boolean enableContextValidation() {
        return enableContextValidation;
    }

    public Boolean enableChangeReasonValidation() {
        return enableChangeReasonValidation;
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
               && Objects.equals(this.metrics, that.metrics)
               && Objects.equals(this.allowExperimentSelfApproval, that.allowExperimentSelfApproval)
               && Objects.equals(this.autoPopulateControl, that.autoPopulateControl)
               && Objects.equals(this.enableContextValidation, that.enableContextValidation)
               && Objects.equals(this.enableChangeReasonValidation, that.enableChangeReasonValidation);
    }

    @Override
    public int hashCode() {
        return Objects.hash(orgId, workspaceAdminEmail, workspaceName, workspaceStatus, metrics, allowExperimentSelfApproval, autoPopulateControl, enableContextValidation, enableChangeReasonValidation);
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
        if (metrics != null) {
            serializer.writeDocument($SCHEMA_METRICS, metrics);
        }
        if (allowExperimentSelfApproval != null) {
            serializer.writeBoolean($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, allowExperimentSelfApproval);
        }
        if (autoPopulateControl != null) {
            serializer.writeBoolean($SCHEMA_AUTO_POPULATE_CONTROL, autoPopulateControl);
        }
        if (enableContextValidation != null) {
            serializer.writeBoolean($SCHEMA_ENABLE_CONTEXT_VALIDATION, enableContextValidation);
        }
        if (enableChangeReasonValidation != null) {
            serializer.writeBoolean($SCHEMA_ENABLE_CHANGE_REASON_VALIDATION, enableChangeReasonValidation);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ADMIN_EMAIL, member, workspaceAdminEmail);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_NAME, member, workspaceName);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_STATUS, member, workspaceStatus);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, metrics);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, member, allowExperimentSelfApproval);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_AUTO_POPULATE_CONTROL, member, autoPopulateControl);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_ENABLE_CONTEXT_VALIDATION, member, enableContextValidation);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_ENABLE_CHANGE_REASON_VALIDATION, member, enableChangeReasonValidation);
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
        builder.metrics(this.metrics);
        builder.allowExperimentSelfApproval(this.allowExperimentSelfApproval);
        builder.autoPopulateControl(this.autoPopulateControl);
        builder.enableContextValidation(this.enableContextValidation);
        builder.enableChangeReasonValidation(this.enableChangeReasonValidation);
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
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String orgId;
        private String workspaceAdminEmail;
        private String workspaceName;
        private WorkspaceStatus workspaceStatus;
        private Document metrics;
        private Boolean allowExperimentSelfApproval;
        private Boolean autoPopulateControl;
        private Boolean enableContextValidation;
        private Boolean enableChangeReasonValidation;

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
            tracker.setMember($SCHEMA_ORG_ID);
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
         * @return this builder.
         */
        public Builder metrics(Document metrics) {
            this.metrics = metrics;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder allowExperimentSelfApproval(boolean allowExperimentSelfApproval) {
            this.allowExperimentSelfApproval = allowExperimentSelfApproval;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder autoPopulateControl(boolean autoPopulateControl) {
            this.autoPopulateControl = autoPopulateControl;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder enableContextValidation(boolean enableContextValidation) {
            this.enableContextValidation = enableContextValidation;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder enableChangeReasonValidation(boolean enableChangeReasonValidation) {
            this.enableChangeReasonValidation = enableChangeReasonValidation;
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
                case 0 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 1 -> workspaceAdminEmail((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ADMIN_EMAIL, member, value));
                case 2 -> workspaceName((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_NAME, member, value));
                case 3 -> workspaceStatus((WorkspaceStatus) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_STATUS, member, value));
                case 4 -> metrics((Document) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, value));
                case 5 -> allowExperimentSelfApproval((boolean) SchemaUtils.validateSameMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, member, value));
                case 6 -> autoPopulateControl((boolean) SchemaUtils.validateSameMember($SCHEMA_AUTO_POPULATE_CONTROL, member, value));
                case 7 -> enableContextValidation((boolean) SchemaUtils.validateSameMember($SCHEMA_ENABLE_CONTEXT_VALIDATION, member, value));
                case 8 -> enableChangeReasonValidation((boolean) SchemaUtils.validateSameMember($SCHEMA_ENABLE_CHANGE_REASON_VALIDATION, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateWorkspaceInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ORG_ID)) {
                orgId("");
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ADMIN_EMAIL)) {
                workspaceAdminEmail("");
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_NAME)) {
                workspaceName("");
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
                    case 0 -> builder.orgId(de.readString(member));
                    case 1 -> builder.workspaceAdminEmail(de.readString(member));
                    case 2 -> builder.workspaceName(de.readString(member));
                    case 3 -> builder.workspaceStatus(WorkspaceStatus.builder().deserializeMember(de, member).build());
                    case 4 -> builder.metrics(de.readDocument());
                    case 5 -> builder.allowExperimentSelfApproval(de.readBoolean(member));
                    case 6 -> builder.autoPopulateControl(de.readBoolean(member));
                    case 7 -> builder.enableContextValidation(de.readBoolean(member));
                    case 8 -> builder.enableChangeReasonValidation(de.readBoolean(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

