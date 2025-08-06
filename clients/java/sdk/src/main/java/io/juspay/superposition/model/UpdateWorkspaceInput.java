
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
public final class UpdateWorkspaceInput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#UpdateWorkspaceRequest");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("org_id", PreludeSchemas.STRING,
                new DefaultTrait(Node.from("juspay")),
                new RequiredTrait(),
                new HttpHeaderTrait("x-org-id"))
        .putMember("workspace_name", PreludeSchemas.STRING,
                new HttpLabelTrait(),
                new RequiredTrait())
        .putMember("workspace_admin_email", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("config_version", PreludeSchemas.STRING)
        .putMember("mandatory_dimensions", SharedSchemas.LIST_MANDATORY_DIMENSIONS)
        .putMember("workspace_status", WorkspaceStatus.$SCHEMA)
        .putMember("metrics", PreludeSchemas.DOCUMENT)
        .putMember("allow_experiment_self_approval", PreludeSchemas.BOOLEAN)
        .putMember("auto_populate_control", PreludeSchemas.BOOLEAN)
        .build();

    private static final Schema $SCHEMA_ORG_ID = $SCHEMA.member("org_id");
    private static final Schema $SCHEMA_WORKSPACE_NAME = $SCHEMA.member("workspace_name");
    private static final Schema $SCHEMA_WORKSPACE_ADMIN_EMAIL = $SCHEMA.member("workspace_admin_email");
    private static final Schema $SCHEMA_CONFIG_VERSION = $SCHEMA.member("config_version");
    private static final Schema $SCHEMA_MANDATORY_DIMENSIONS = $SCHEMA.member("mandatory_dimensions");
    private static final Schema $SCHEMA_WORKSPACE_STATUS = $SCHEMA.member("workspace_status");
    private static final Schema $SCHEMA_METRICS = $SCHEMA.member("metrics");
    private static final Schema $SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL = $SCHEMA.member("allow_experiment_self_approval");
    private static final Schema $SCHEMA_AUTO_POPULATE_CONTROL = $SCHEMA.member("auto_populate_control");

    private final transient String orgId;
    private final transient String workspaceName;
    private final transient String workspaceAdminEmail;
    private final transient String configVersion;
    private final transient List<String> mandatoryDimensions;
    private final transient WorkspaceStatus workspaceStatus;
    private final transient Document metrics;
    private final transient Boolean allowExperimentSelfApproval;
    private final transient Boolean autoPopulateControl;

    private UpdateWorkspaceInput(Builder builder) {
        this.orgId = builder.orgId;
        this.workspaceName = builder.workspaceName;
        this.workspaceAdminEmail = builder.workspaceAdminEmail;
        this.configVersion = builder.configVersion;
        this.mandatoryDimensions = builder.mandatoryDimensions == null ? null : Collections.unmodifiableList(builder.mandatoryDimensions);
        this.workspaceStatus = builder.workspaceStatus;
        this.metrics = builder.metrics;
        this.allowExperimentSelfApproval = builder.allowExperimentSelfApproval;
        this.autoPopulateControl = builder.autoPopulateControl;
    }

    public String orgId() {
        return orgId;
    }

    public String workspaceName() {
        return workspaceName;
    }

    public String workspaceAdminEmail() {
        return workspaceAdminEmail;
    }

    public String configVersion() {
        return configVersion;
    }

    public List<String> mandatoryDimensions() {
        if (mandatoryDimensions == null) {
            return Collections.emptyList();
        }
        return mandatoryDimensions;
    }

    public boolean hasMandatoryDimensions() {
        return mandatoryDimensions != null;
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
        UpdateWorkspaceInput that = (UpdateWorkspaceInput) other;
        return Objects.equals(this.orgId, that.orgId)
               && Objects.equals(this.workspaceName, that.workspaceName)
               && Objects.equals(this.workspaceAdminEmail, that.workspaceAdminEmail)
               && Objects.equals(this.configVersion, that.configVersion)
               && Objects.equals(this.mandatoryDimensions, that.mandatoryDimensions)
               && Objects.equals(this.workspaceStatus, that.workspaceStatus)
               && Objects.equals(this.metrics, that.metrics)
               && Objects.equals(this.allowExperimentSelfApproval, that.allowExperimentSelfApproval)
               && Objects.equals(this.autoPopulateControl, that.autoPopulateControl);
    }

    @Override
    public int hashCode() {
        return Objects.hash(orgId, workspaceName, workspaceAdminEmail, configVersion, mandatoryDimensions, workspaceStatus, metrics, allowExperimentSelfApproval, autoPopulateControl);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ORG_ID, orgId);
        serializer.writeString($SCHEMA_WORKSPACE_NAME, workspaceName);
        serializer.writeString($SCHEMA_WORKSPACE_ADMIN_EMAIL, workspaceAdminEmail);
        if (configVersion != null) {
            serializer.writeString($SCHEMA_CONFIG_VERSION, configVersion);
        }
        if (mandatoryDimensions != null) {
            serializer.writeList($SCHEMA_MANDATORY_DIMENSIONS, mandatoryDimensions, mandatoryDimensions.size(), SharedSerde.ListMandatoryDimensionsSerializer.INSTANCE);
        }
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
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_NAME, member, workspaceName);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ADMIN_EMAIL, member, workspaceAdminEmail);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, orgId);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG_VERSION, member, configVersion);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_MANDATORY_DIMENSIONS, member, mandatoryDimensions);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_STATUS, member, workspaceStatus);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, metrics);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, member, allowExperimentSelfApproval);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_AUTO_POPULATE_CONTROL, member, autoPopulateControl);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link UpdateWorkspaceInput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.orgId(this.orgId);
        builder.workspaceName(this.workspaceName);
        builder.workspaceAdminEmail(this.workspaceAdminEmail);
        builder.configVersion(this.configVersion);
        builder.mandatoryDimensions(this.mandatoryDimensions);
        builder.workspaceStatus(this.workspaceStatus);
        builder.metrics(this.metrics);
        builder.allowExperimentSelfApproval(this.allowExperimentSelfApproval);
        builder.autoPopulateControl(this.autoPopulateControl);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link UpdateWorkspaceInput}.
     */
    public static final class Builder implements ShapeBuilder<UpdateWorkspaceInput> {
        private static final String ORG_ID_DEFAULT = "juspay";
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String orgId = ORG_ID_DEFAULT;
        private String workspaceName;
        private String workspaceAdminEmail;
        private String configVersion;
        private List<String> mandatoryDimensions;
        private WorkspaceStatus workspaceStatus;
        private Document metrics;
        private Boolean allowExperimentSelfApproval;
        private Boolean autoPopulateControl;

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
        public Builder workspaceName(String workspaceName) {
            this.workspaceName = Objects.requireNonNull(workspaceName, "workspaceName cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_NAME);
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
         * @return this builder.
         */
        public Builder configVersion(String configVersion) {
            this.configVersion = configVersion;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder mandatoryDimensions(List<String> mandatoryDimensions) {
            this.mandatoryDimensions = mandatoryDimensions;
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

        @Override
        public UpdateWorkspaceInput build() {
            tracker.validate();
            return new UpdateWorkspaceInput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceName((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_NAME, member, value));
                case 1 -> workspaceAdminEmail((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ADMIN_EMAIL, member, value));
                case 2 -> orgId((String) SchemaUtils.validateSameMember($SCHEMA_ORG_ID, member, value));
                case 3 -> configVersion((String) SchemaUtils.validateSameMember($SCHEMA_CONFIG_VERSION, member, value));
                case 4 -> mandatoryDimensions((List<String>) SchemaUtils.validateSameMember($SCHEMA_MANDATORY_DIMENSIONS, member, value));
                case 5 -> workspaceStatus((WorkspaceStatus) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_STATUS, member, value));
                case 6 -> metrics((Document) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, value));
                case 7 -> allowExperimentSelfApproval((boolean) SchemaUtils.validateSameMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, member, value));
                case 8 -> autoPopulateControl((boolean) SchemaUtils.validateSameMember($SCHEMA_AUTO_POPULATE_CONTROL, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<UpdateWorkspaceInput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_NAME)) {
                workspaceName("");
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ADMIN_EMAIL)) {
                workspaceAdminEmail("");
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
                    case 0 -> builder.workspaceName(de.readString(member));
                    case 1 -> builder.workspaceAdminEmail(de.readString(member));
                    case 2 -> builder.orgId(de.readString(member));
                    case 3 -> builder.configVersion(de.readString(member));
                    case 4 -> builder.mandatoryDimensions(SharedSerde.deserializeListMandatoryDimensions(member, de));
                    case 5 -> builder.workspaceStatus(WorkspaceStatus.builder().deserializeMember(de, member).build());
                    case 6 -> builder.metrics(de.readDocument());
                    case 7 -> builder.allowExperimentSelfApproval(de.readBoolean(member));
                    case 8 -> builder.autoPopulateControl(de.readBoolean(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

