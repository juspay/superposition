
package io.juspay.superposition.model;

import java.time.Instant;
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

/**
 * Full job detail including Kronos execution information.
 */
@SmithyGenerated
public final class JobDetailResponse implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#JobDetailResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("kronos_job_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("description", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("job_type", BackgroundJobType.$SCHEMA,
                new RequiredTrait())
        .putMember("status", BackgroundJobStatus.$SCHEMA,
                new RequiredTrait())
        .putMember("name", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("progress", PreludeSchemas.INTEGER,
                new RequiredTrait())
        .putMember("workspace_schema", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("created_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("logs", PreludeSchemas.DOCUMENT,
                new RequiredTrait())
        .putMember("execution", ExecutionDetails.$SCHEMA)
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_KRONOS_JOB_ID = $SCHEMA.member("kronos_job_id");
    private static final Schema $SCHEMA_DESCRIPTION = $SCHEMA.member("description");
    private static final Schema $SCHEMA_JOB_TYPE = $SCHEMA.member("job_type");
    private static final Schema $SCHEMA_STATUS = $SCHEMA.member("status");
    private static final Schema $SCHEMA_NAME = $SCHEMA.member("name");
    private static final Schema $SCHEMA_PROGRESS = $SCHEMA.member("progress");
    private static final Schema $SCHEMA_WORKSPACE_SCHEMA = $SCHEMA.member("workspace_schema");
    private static final Schema $SCHEMA_CREATED_AT = $SCHEMA.member("created_at");
    private static final Schema $SCHEMA_LOGS = $SCHEMA.member("logs");
    private static final Schema $SCHEMA_EXECUTION = $SCHEMA.member("execution");

    private final transient String id;
    private final transient String kronosJobId;
    private final transient String description;
    private final transient BackgroundJobType jobType;
    private final transient BackgroundJobStatus status;
    private final transient String name;
    private final transient int progress;
    private final transient String workspaceSchema;
    private final transient Instant createdAt;
    private final transient Document logs;
    private final transient ExecutionDetails execution;

    private JobDetailResponse(Builder builder) {
        this.id = builder.id;
        this.kronosJobId = builder.kronosJobId;
        this.description = builder.description;
        this.jobType = builder.jobType;
        this.status = builder.status;
        this.name = builder.name;
        this.progress = builder.progress;
        this.workspaceSchema = builder.workspaceSchema;
        this.createdAt = builder.createdAt;
        this.logs = builder.logs;
        this.execution = builder.execution;
    }

    public String id() {
        return id;
    }

    public String kronosJobId() {
        return kronosJobId;
    }

    public String description() {
        return description;
    }

    public BackgroundJobType jobType() {
        return jobType;
    }

    public BackgroundJobStatus status() {
        return status;
    }

    public String name() {
        return name;
    }

    public int progress() {
        return progress;
    }

    public String workspaceSchema() {
        return workspaceSchema;
    }

    public Instant createdAt() {
        return createdAt;
    }

    public Document logs() {
        return logs;
    }

    public ExecutionDetails execution() {
        return execution;
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
        JobDetailResponse that = (JobDetailResponse) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.kronosJobId, that.kronosJobId)
               && Objects.equals(this.description, that.description)
               && Objects.equals(this.jobType, that.jobType)
               && Objects.equals(this.status, that.status)
               && Objects.equals(this.name, that.name)
               && this.progress == that.progress
               && Objects.equals(this.workspaceSchema, that.workspaceSchema)
               && Objects.equals(this.createdAt, that.createdAt)
               && Objects.equals(this.logs, that.logs)
               && Objects.equals(this.execution, that.execution);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, kronosJobId, description, jobType, status, name, progress, workspaceSchema, createdAt, logs, execution);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeString($SCHEMA_KRONOS_JOB_ID, kronosJobId);
        serializer.writeString($SCHEMA_DESCRIPTION, description);
        serializer.writeString($SCHEMA_JOB_TYPE, jobType.value());
        serializer.writeString($SCHEMA_STATUS, status.value());
        serializer.writeString($SCHEMA_NAME, name);
        serializer.writeInteger($SCHEMA_PROGRESS, progress);
        serializer.writeString($SCHEMA_WORKSPACE_SCHEMA, workspaceSchema);
        serializer.writeTimestamp($SCHEMA_CREATED_AT, createdAt);
        serializer.writeDocument($SCHEMA_LOGS, logs);
        if (execution != null) {
            serializer.writeStruct($SCHEMA_EXECUTION, execution);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_KRONOS_JOB_ID, member, kronosJobId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, description);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_JOB_TYPE, member, jobType);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, status);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_NAME, member, name);
            case 6 -> (T) SchemaUtils.validateSameMember($SCHEMA_PROGRESS, member, progress);
            case 7 -> (T) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_SCHEMA, member, workspaceSchema);
            case 8 -> (T) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, createdAt);
            case 9 -> (T) SchemaUtils.validateSameMember($SCHEMA_LOGS, member, logs);
            case 10 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXECUTION, member, execution);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link JobDetailResponse}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.kronosJobId(this.kronosJobId);
        builder.description(this.description);
        builder.jobType(this.jobType);
        builder.status(this.status);
        builder.name(this.name);
        builder.progress(this.progress);
        builder.workspaceSchema(this.workspaceSchema);
        builder.createdAt(this.createdAt);
        builder.logs(this.logs);
        builder.execution(this.execution);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link JobDetailResponse}.
     */
    public static final class Builder implements ShapeBuilder<JobDetailResponse> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private String kronosJobId;
        private String description;
        private BackgroundJobType jobType;
        private BackgroundJobStatus status;
        private String name;
        private int progress;
        private String workspaceSchema;
        private Instant createdAt;
        private Document logs;
        private ExecutionDetails execution;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
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
        public Builder kronosJobId(String kronosJobId) {
            this.kronosJobId = Objects.requireNonNull(kronosJobId, "kronosJobId cannot be null");
            tracker.setMember($SCHEMA_KRONOS_JOB_ID);
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
        public Builder jobType(BackgroundJobType jobType) {
            this.jobType = Objects.requireNonNull(jobType, "jobType cannot be null");
            tracker.setMember($SCHEMA_JOB_TYPE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder status(BackgroundJobStatus status) {
            this.status = Objects.requireNonNull(status, "status cannot be null");
            tracker.setMember($SCHEMA_STATUS);
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
        public Builder progress(int progress) {
            this.progress = progress;
            tracker.setMember($SCHEMA_PROGRESS);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder workspaceSchema(String workspaceSchema) {
            this.workspaceSchema = Objects.requireNonNull(workspaceSchema, "workspaceSchema cannot be null");
            tracker.setMember($SCHEMA_WORKSPACE_SCHEMA);
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
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder logs(Document logs) {
            this.logs = Objects.requireNonNull(logs, "logs cannot be null");
            tracker.setMember($SCHEMA_LOGS);
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder execution(ExecutionDetails execution) {
            this.execution = execution;
            return this;
        }

        @Override
        public JobDetailResponse build() {
            tracker.validate();
            return new JobDetailResponse(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> kronosJobId((String) SchemaUtils.validateSameMember($SCHEMA_KRONOS_JOB_ID, member, value));
                case 2 -> description((String) SchemaUtils.validateSameMember($SCHEMA_DESCRIPTION, member, value));
                case 3 -> jobType((BackgroundJobType) SchemaUtils.validateSameMember($SCHEMA_JOB_TYPE, member, value));
                case 4 -> status((BackgroundJobStatus) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, value));
                case 5 -> name((String) SchemaUtils.validateSameMember($SCHEMA_NAME, member, value));
                case 6 -> progress((int) SchemaUtils.validateSameMember($SCHEMA_PROGRESS, member, value));
                case 7 -> workspaceSchema((String) SchemaUtils.validateSameMember($SCHEMA_WORKSPACE_SCHEMA, member, value));
                case 8 -> createdAt((Instant) SchemaUtils.validateSameMember($SCHEMA_CREATED_AT, member, value));
                case 9 -> logs((Document) SchemaUtils.validateSameMember($SCHEMA_LOGS, member, value));
                case 10 -> execution((ExecutionDetails) SchemaUtils.validateSameMember($SCHEMA_EXECUTION, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<JobDetailResponse> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_KRONOS_JOB_ID)) {
                kronosJobId("");
            }
            if (!tracker.checkMember($SCHEMA_DESCRIPTION)) {
                description("");
            }
            if (!tracker.checkMember($SCHEMA_JOB_TYPE)) {
                jobType(BackgroundJobType.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_STATUS)) {
                status(BackgroundJobStatus.unknown(""));
            }
            if (!tracker.checkMember($SCHEMA_NAME)) {
                name("");
            }
            if (!tracker.checkMember($SCHEMA_PROGRESS)) {
                tracker.setMember($SCHEMA_PROGRESS);
            }
            if (!tracker.checkMember($SCHEMA_WORKSPACE_SCHEMA)) {
                workspaceSchema("");
            }
            if (!tracker.checkMember($SCHEMA_CREATED_AT)) {
                createdAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_LOGS)) {
                tracker.setMember($SCHEMA_LOGS);
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
                    case 0 -> builder.id(de.readString(member));
                    case 1 -> builder.kronosJobId(de.readString(member));
                    case 2 -> builder.description(de.readString(member));
                    case 3 -> builder.jobType(BackgroundJobType.builder().deserializeMember(de, member).build());
                    case 4 -> builder.status(BackgroundJobStatus.builder().deserializeMember(de, member).build());
                    case 5 -> builder.name(de.readString(member));
                    case 6 -> builder.progress(de.readInteger(member));
                    case 7 -> builder.workspaceSchema(de.readString(member));
                    case 8 -> builder.createdAt(de.readTimestamp(member));
                    case 9 -> builder.logs(de.readDocument());
                    case 10 -> builder.execution(ExecutionDetails.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

