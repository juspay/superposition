
package io.juspay.superposition.model;

import java.time.Instant;
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

@SmithyGenerated
public final class MigrateWorkspaceSchemaOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WorkspaceResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("workspace_name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("organisation_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("organisation_name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("workspace_schema_name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("workspace_status", WorkspaceStatus.$SCHEMA,
                new RequiredTrait())
        .putMember("workspace_admin_email", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("config_version", PreludeSchemas.STRING)
        .putMember("created_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("last_modified_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("last_modified_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("mandatory_dimensions", SharedSchemas.LIST_MANDATORY_DIMENSIONS)
        .putMember("strict_mode", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .putMember("metrics", PreludeSchemas.DOCUMENT)
        .putMember("allow_experiment_self_approval", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .putMember("auto_populate_control", PreludeSchemas.BOOLEAN,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_WORKSPACE_NAME = $SCHEMA.member("workspace_name");
    private static final Schema $SCHEMA_ORGANISATION_ID = $SCHEMA.member("organisation_id");
    private static final Schema $SCHEMA_ORGANISATION_NAME = $SCHEMA.member("organisation_name");
    private static final Schema $SCHEMA_WORKSPACE_SCHEMA_NAME = $SCHEMA.member("workspace_schema_name");
    private static final Schema $SCHEMA_WORKSPACE_STATUS = $SCHEMA.member("workspace_status");
    private static final Schema $SCHEMA_WORKSPACE_ADMIN_EMAIL = $SCHEMA.member("workspace_admin_email");
    private static final Schema $SCHEMA_CONFIG_VERSION = $SCHEMA.member("config_version");
    private static final Schema $SCHEMA_CREATED_BY = $SCHEMA.member("created_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_BY = $SCHEMA.member("last_modified_by");
    private static final Schema $SCHEMA_LAST_MODIFIED_AT = $SCHEMA.member("last_modified_at");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_MANDATORY_DIMENSIONS = $SCHEMA.member("mandatory_dimensions");
    private static final Schema $SCHEMA_STRICT_MODE = $SCHEMA.member("strict_mode");
    private static final Schema $SCHEMA_METRICS = $SCHEMA.member("metrics");
    private static final Schema $SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL = $SCHEMA.member("allow_experiment_self_approval");
    private static final Schema $SCHEMA_AUTO_POPULATE_CONTROL = $SCHEMA.member("auto_populate_control");

    private final transient String workspaceName;
    private final transient String organisationId;
    private final transient String organisationName;
    private final transient String workspaceSchemaName;
    private final transient WorkspaceStatus workspaceStatus;
    private final transient String workspaceAdminEmail;
    private final transient String configVersion;
    private final transient String createdBy;
    private final transient String lastModifiedBy;
    private final transient Instant lastModifiedAt;
    private final transient Instant createdAt;
    private final transient List<String> mandatoryDimensions;
    private final transient boolean strictMode;
    private final transient Document metrics;
    private final transient boolean allowExperimentSelfApproval;
    private final transient boolean autoPopulateControl;

    private MigrateWorkspaceSchemaOutput(Builder builder) {
        this.workspaceName = builder.workspaceName;
        this.organisationId = builder.organisationId;
        this.organisationName = builder.organisationName;
        this.workspaceSchemaName = builder.workspaceSchemaName;
        this.workspaceStatus = builder.workspaceStatus;
        this.workspaceAdminEmail = builder.workspaceAdminEmail;
        this.configVersion = builder.configVersion;
        this.createdBy = builder.createdBy;
        this.lastModifiedBy = builder.lastModifiedBy;
        this.lastModifiedAt = builder.lastModifiedAt;
        this.createdAt = builder.createdAt;
        this.mandatoryDimensions = builder.mandatoryDimensions == null ? null : Collections.unmodifiableList(builder.mandatoryDimensions);
        this.strictMode = builder.strictMode;
        this.metrics = builder.metrics;
        this.allowExperimentSelfApproval = builder.allowExperimentSelfApproval;
        this.autoPopulateControl = builder.autoPopulateControl;
    }

    public String workspaceName() {
        return workspaceName;
    }

    public String organisationId() {
        return organisationId;
    }

    public String organisationName() {
        return organisationName;
    }

    public String workspaceSchemaName() {
        return workspaceSchemaName;
    }

    public WorkspaceStatus workspaceStatus() {
        return workspaceStatus;
    }

    public String workspaceAdminEmail() {
        return workspaceAdminEmail;
    }

    public String configVersion() {
        return configVersion;
    }

    public String createdBy() {
        return createdBy;
    }

    public String lastModifiedBy() {
        return lastModifiedBy;
    }

    public Instant lastModifiedAt() {
        return lastModifiedAt;
    }

    public Instant createdAt() {
        return createdAt;
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

    public boolean strictMode() {
        return strictMode;
    }

    public Document metrics() {
        return metrics;
    }

    public boolean allowExperimentSelfApproval() {
        return allowExperimentSelfApproval;
    }

    public boolean autoPopulateControl() {
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
        MigrateWorkspaceSchemaOutput that = (MigrateWorkspaceSchemaOutput) other;
        return Objects.equals(this.workspaceName, that.workspaceName)
               && Objects.equals(this.organisationId, that.organisationId)
               && Objects.equals(this.organisationName, that.organisationName)
               && Objects.equals(this.workspaceSchemaName, that.workspaceSchemaName)
               && Objects.equals(this.workspaceStatus, that.workspaceStatus)
               && Objects.equals(this.workspaceAdminEmail, that.workspaceAdminEmail)
               && Objects.equals(this.configVersion, that.configVersion)
               && Objects.equals(this.createdBy, that.createdBy)
               && Objects.equals(this.lastModifiedBy, that.lastModifiedBy)
               && Objects.equals(this.lastModifiedAt, that.lastModifiedAt)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.mandatoryDimensions, that.mandatoryDimensions)
               && this.strictMode == that.strictMode
               && Objects.equals(this.metrics, that.metrics)
               && this.allowExperimentSelfApproval == that.allowExperimentSelfApproval
               && this.autoPopulateControl == that.autoPopulateControl;
    }

    @Override
    public int hashCode() {
        return Objects.hash(workspaceName, organisationId, organisationName, workspaceSchemaName, workspaceStatus, workspaceAdminEmail, configVersion, createdBy, lastModifiedBy, lastModifiedAt, createdAt, mandatoryDimensions, strictMode, metrics, allowExperimentSelfApproval, autoPopulateControl);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_WORKSPACE_NAME, workspaceName);
        serializer.writeString($SCHEMA_ORGANISATION_ID, organisationId);
        serializer.writeString($SCHEMA_ORGANISATION_NAME, organisationName);
        serializer.writeString($SCHEMA_WORKSPACE_SCHEMA_NAME, workspaceSchemaName);
        serializer.writeString($SCHEMA_WORKSPACE_STATUS, workspaceStatus.value());
        serializer.writeString($SCHEMA_WORKSPACE_ADMIN_EMAIL, workspaceAdminEmail);
        if (configVersion != null) {
            serializer.writeString($SCHEMA_CONFIG_VERSION, configVersion);
        }
        serializer.writeString($SCHEMA_CREATED_BY, createdBy);
        serializer.writeString($SCHEMA_LAST_MODIFIED_BY, lastModifiedBy);
        serializer.writeTimestamp($SCHEMA_LAST_MODIFIED_AT, lastModifiedAt);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        if (mandatoryDimensions != null) {
            serializer.writeList($SCHEMA_MANDATORY_DIMENSIONS, mandatoryDimensions, mandatoryDimensions.size(), SharedSerde.ListMandatoryDimensionsSerializer.INSTANCE);
        }
        serializer.writeBoolean($SCHEMA_STRICT_MODE, strictMode);
        if (metrics != null) {
            serializer.writeDocument($SCHEMA_METRICS, metrics);
        }
        serializer.writeBoolean($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, allowExperimentSelfApproval);
        serializer.writeBoolean($SCHEMA_AUTO_POPULATE_CONTROL, autoPopulateControl);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_NAME, member, workspaceName);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORGANISATION_ID, member, organisationId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_ORGANISATION_NAME, member, organisationName);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_SCHEMA_NAME, member, workspaceSchemaName);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_STATUS, member, workspaceStatus);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ADMIN_EMAIL, member, workspaceAdminEmail);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, createdBy);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, lastModifiedBy);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, lastModifiedAt);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_STRICT_MODE, member, strictMode);
            case 11 -> (T) SchemaUtils.validateSameMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, member, allowExperimentSelfApproval);
            case 12 -> (T) SchemaUtils.validateSameMember($SCHEMA_AUTO_POPULATE_CONTROL, member, autoPopulateControl);
            case 13 -> (T) SchemaUtils.validateSameMember($SCHEMA_CONFIG_VERSION, member, configVersion);
            case 14 -> (T) SchemaUtils.validateSameMember($SCHEMA_MANDATORY_DIMENSIONS, member, mandatoryDimensions);
            case 15 -> (T) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, metrics);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link MigrateWorkspaceSchemaOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.workspaceName(this.workspaceName);
        builder.organisationId(this.organisationId);
        builder.organisationName(this.organisationName);
        builder.workspaceSchemaName(this.workspaceSchemaName);
        builder.workspaceStatus(this.workspaceStatus);
        builder.workspaceAdminEmail(this.workspaceAdminEmail);
        builder.configVersion(this.configVersion);
        builder.createdBy(this.createdBy);
        builder.lastModifiedBy(this.lastModifiedBy);
        builder.lastModifiedAt(this.lastModifiedAt);
        builder.createdAt(this.createdAt);
        builder.mandatoryDimensions(this.mandatoryDimensions);
        builder.strictMode(this.strictMode);
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
     * Builder for {@link MigrateWorkspaceSchemaOutput}.
     */
    public static final class Builder implements ShapeBuilder<MigrateWorkspaceSchemaOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String workspaceName;
        private String organisationId;
        private String organisationName;
        private String workspaceSchemaName;
        private WorkspaceStatus workspaceStatus;
        private String workspaceAdminEmail;
        private String configVersion;
        private String createdBy;
        private String lastModifiedBy;
        private Instant lastModifiedAt;
        private Instant createdAt;
        private List<String> mandatoryDimensions;
        private boolean strictMode;
        private Document metrics;
        private boolean allowExperimentSelfApproval;
        private boolean autoPopulateControl;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
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
        public Builder organisationId(String organisationId) {
            this.organisationId = Objects.requireNonNull(organisationId, "organisationId cannot be null");
            tracker.setMember($SCHEMA_ORGANISATION_ID);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder organisationName(String organisationName) {
            this.organisationName = Objects.requireNonNull(organisationName, "organisationName cannot be null");
            tracker.setMember($SCHEMA_ORGANISATION_NAME);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder workspaceSchemaName(String workspaceSchemaName) {
            this.workspaceSchemaName = Objects.requireNonNull(workspaceSchemaName, "workspaceSchemaName cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_SCHEMA_NAME);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder workspaceStatus(WorkspaceStatus workspaceStatus) {
            this.workspaceStatus = Objects.requireNonNull(workspaceStatus, "workspaceStatus cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_STATUS);
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
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdBy(String createdBy) {
            this.createdBy = Objects.requireNonNull(createdBy, "createdBy cannot be null");
            tracker.setMember($SCHEMA_CREATED_BY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lastModifiedBy(String lastModifiedBy) {
            this.lastModifiedBy = Objects.requireNonNull(lastModifiedBy, "lastModifiedBy cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_BY);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lastModifiedAt(Instant lastModifiedAt) {
            this.lastModifiedAt = Objects.requireNonNull(lastModifiedAt, "lastModifiedAt cannot be null");
            tracker.setMember($SCHEMA_LAST_MODIFIED_AT);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder createdAt(Instant createdAt) {
            this.createdAt = Objects.requireNonNull(createdAt, "createdAt cannot be null");
            tracker.setMember($SCHEMA_CREATED_AT);
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

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder autoPopulateControl(boolean autoPopulateControl) {
            this.autoPopulateControl = autoPopulateControl;
            tracker.setMember($SCHEMA_AUTO_POPULATE_CONTROL);
            return this;
        }

        @Override
        public MigrateWorkspaceSchemaOutput build() {
            tracker.validate();
            return new MigrateWorkspaceSchemaOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> workspaceName((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_NAME, member, value));
                case 1 -> organisationId((String) SchemaUtils.validateSameMember($SCHEMA_ORGANISATION_ID, member, value));
                case 2 -> organisationName((String) SchemaUtils.validateSameMember($SCHEMA_ORGANISATION_NAME, member, value));
                case 3 -> workspaceSchemaName((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_SCHEMA_NAME, member, value));
                case 4 -> workspaceStatus((WorkspaceStatus) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_STATUS, member, value));
                case 5 -> workspaceAdminEmail((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_ADMIN_EMAIL, member, value));
                case 6 -> createdBy((String) SchemaUtils.validateSameMember($SCHEMA_CREATED_BY, member, value));
                case 7 -> lastModifiedBy((String) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_BY, member, value));
                case 8 -> lastModifiedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_LAST_MODIFIED_AT, member, value));
                case 9 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 10 -> strictMode((boolean) SchemaUtils.validateSameMember($SCHEMA_STRICT_MODE, member, value));
                case 11 -> allowExperimentSelfApproval((boolean) SchemaUtils.validateSameMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL, member, value));
                case 12 -> autoPopulateControl((boolean) SchemaUtils.validateSameMember($SCHEMA_AUTO_POPULATE_CONTROL, member, value));
                case 13 -> configVersion((String) SchemaUtils.validateSameMember($SCHEMA_CONFIG_VERSION, member, value));
                case 14 -> mandatoryDimensions((List<String>) SchemaUtils.validateSameMember($SCHEMA_MANDATORY_DIMENSIONS, member, value));
                case 15 -> metrics((Document) SchemaUtils.validateSameMember($SCHEMA_METRICS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<MigrateWorkspaceSchemaOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_NAME)) {
                workspaceName("");
            }
            if (!tracker.checkMember($SCHEMA_ORGANISATION_ID)) {
                organisationId("");
            }
            if (!tracker.checkMember($SCHEMA_ORGANISATION_NAME)) {
                organisationName("");
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_SCHEMA_NAME)) {
                workspaceSchemaName("");
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_STATUS)) {
                workspaceStatus(WorkspaceStatus.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_ADMIN_EMAIL)) {
                workspaceAdminEmail("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_BY)) {
                createdBy("");
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_BY)) {
                lastModifiedBy("");
            }
            if (!tracker.checkMember($SCHEMA_LAST_MODIFIED_AT)) {
                lastModifiedAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_STRICT_MODE)) {
                tracker.setMember($SCHEMA_STRICT_MODE);
            }
            if (!tracker.checkMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL)) {
                tracker.setMember($SCHEMA_ALLOW_EXPERIMENT_SELF_APPROVAL);
            }
            if (!tracker.checkMember($SCHEMA_AUTO_POPULATE_CONTROL)) {
                tracker.setMember($SCHEMA_AUTO_POPULATE_CONTROL);
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
                    case 1 -> builder.organisationId(de.readString(member));
                    case 2 -> builder.organisationName(de.readString(member));
                    case 3 -> builder.workspaceSchemaName(de.readString(member));
                    case 4 -> builder.workspaceStatus(WorkspaceStatus.builder().deserializeMember(de, member).build());
                    case 5 -> builder.workspaceAdminEmail(de.readString(member));
                    case 6 -> builder.createdBy(de.readString(member));
                    case 7 -> builder.lastModifiedBy(de.readString(member));
                    case 8 -> builder.lastModifiedAt(de.readTimestamp(member));
                    case 9 -> builder.createdAt(de.readTimestamp(member));
                    case 10 -> builder.strictMode(de.readBoolean(member));
                    case 11 -> builder.allowExperimentSelfApproval(de.readBoolean(member));
                    case 12 -> builder.autoPopulateControl(de.readBoolean(member));
                    case 13 -> builder.configVersion(de.readString(member));
                    case 14 -> builder.mandatoryDimensions(SharedSerde.deserializeListMandatoryDimensions(member, de));
                    case 15 -> builder.metrics(de.readDocument());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

