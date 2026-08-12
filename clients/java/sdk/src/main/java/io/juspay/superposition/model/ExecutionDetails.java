
package io.juspay.superposition.model;

import java.time.Instant;
import java.util.Objects;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.SerializableStruct;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Execution details fetched from Kronos for a job.
 */
@SmithyGenerated
public final class ExecutionDetails implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#ExecutionDetails");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("attempt_count", PreludeSchemas.LONG)
        .putMember("max_attempts", PreludeSchemas.LONG)
        .putMember("started_at", SharedSchemas.DATE_TIME)
        .putMember("completed_at", SharedSchemas.DATE_TIME)
        .putMember("duration_ms", PreludeSchemas.LONG)
        .putMember("execution_status", PreludeSchemas.STRING)
        .build();

    private static final Schema $SCHEMA_ATTEMPT_COUNT = $SCHEMA.member("attempt_count");
    private static final Schema $SCHEMA_MAX_ATTEMPTS = $SCHEMA.member("max_attempts");
    private static final Schema $SCHEMA_STARTED_AT = $SCHEMA.member("started_at");
    private static final Schema $SCHEMA_COMPLETED_AT = $SCHEMA.member("completed_at");
    private static final Schema $SCHEMA_DURATION_MS = $SCHEMA.member("duration_ms");
    private static final Schema $SCHEMA_EXECUTION_STATUS = $SCHEMA.member("execution_status");

    private final transient Long attemptCount;
    private final transient Long maxAttempts;
    private final transient Instant startedAt;
    private final transient Instant completedAt;
    private final transient Long durationMs;
    private final transient String executionStatus;

    private ExecutionDetails(Builder builder) {
        this.attemptCount = builder.attemptCount;
        this.maxAttempts = builder.maxAttempts;
        this.startedAt = builder.startedAt;
        this.completedAt = builder.completedAt;
        this.durationMs = builder.durationMs;
        this.executionStatus = builder.executionStatus;
    }

    public Long attemptCount() {
        return attemptCount;
    }

    public Long maxAttempts() {
        return maxAttempts;
    }

    public Instant startedAt() {
        return startedAt;
    }

    public Instant completedAt() {
        return completedAt;
    }

    public Long durationMs() {
        return durationMs;
    }

    public String executionStatus() {
        return executionStatus;
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
        ExecutionDetails that = (ExecutionDetails) other;
        return Objects.equals(this.attemptCount, that.attemptCount)
               && Objects.equals(this.maxAttempts, that.maxAttempts)
               && Objects.equals(this.startedAt, that.startedAt)
               && Objects.equals(this.completedAt, that.completedAt)
               && Objects.equals(this.durationMs, that.durationMs)
               && Objects.equals(this.executionStatus, that.executionStatus);
    }

    @Override
    public int hashCode() {
        return Objects.hash(attemptCount, maxAttempts, startedAt, completedAt, durationMs, executionStatus);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        if (attemptCount != null) {
            serializer.writeLong($SCHEMA_ATTEMPT_COUNT, attemptCount);
        }
        if (maxAttempts != null) {
            serializer.writeLong($SCHEMA_MAX_ATTEMPTS, maxAttempts);
        }
        if (startedAt != null) {
            serializer.writeTimestamp($SCHEMA_STARTED_AT, startedAt);
        }
        if (completedAt != null) {
            serializer.writeTimestamp($SCHEMA_COMPLETED_AT, completedAt);
        }
        if (durationMs != null) {
            serializer.writeLong($SCHEMA_DURATION_MS, durationMs);
        }
        if (executionStatus != null) {
            serializer.writeString($SCHEMA_EXECUTION_STATUS, executionStatus);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ATTEMPT_COUNT, member, attemptCount);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_MAX_ATTEMPTS, member, maxAttempts);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_STARTED_AT, member, startedAt);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_COMPLETED_AT, member, completedAt);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_DURATION_MS, member, durationMs);
            case 5 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXECUTION_STATUS, member, executionStatus);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ExecutionDetails}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.attemptCount(this.attemptCount);
        builder.maxAttempts(this.maxAttempts);
        builder.startedAt(this.startedAt);
        builder.completedAt(this.completedAt);
        builder.durationMs(this.durationMs);
        builder.executionStatus(this.executionStatus);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ExecutionDetails}.
     */
    public static final class Builder implements ShapeBuilder<ExecutionDetails> {
        private Long attemptCount;
        private Long maxAttempts;
        private Instant startedAt;
        private Instant completedAt;
        private Long durationMs;
        private String executionStatus;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * @return this builder.
         */
        public Builder attemptCount(long attemptCount) {
            this.attemptCount = attemptCount;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder maxAttempts(long maxAttempts) {
            this.maxAttempts = maxAttempts;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder startedAt(Instant startedAt) {
            this.startedAt = startedAt;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder completedAt(Instant completedAt) {
            this.completedAt = completedAt;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder durationMs(long durationMs) {
            this.durationMs = durationMs;
            return this;
        }

        /**
         * @return this builder.
         */
        public Builder executionStatus(String executionStatus) {
            this.executionStatus = executionStatus;
            return this;
        }

        @Override
        public ExecutionDetails build() {
            return new ExecutionDetails(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> attemptCount((long) SchemaUtils.validateSameMember($SCHEMA_ATTEMPT_COUNT, member, value));
                case 1 -> maxAttempts((long) SchemaUtils.validateSameMember($SCHEMA_MAX_ATTEMPTS, member, value));
                case 2 -> startedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_STARTED_AT, member, value));
                case 3 -> completedAt((Instant) SchemaUtils.validateSameMember($SCHEMA_COMPLETED_AT, member, value));
                case 4 -> durationMs((long) SchemaUtils.validateSameMember($SCHEMA_DURATION_MS, member, value));
                case 5 -> executionStatus((String) SchemaUtils.validateSameMember($SCHEMA_EXECUTION_STATUS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
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
                    case 0 -> builder.attemptCount(de.readLong(member));
                    case 1 -> builder.maxAttempts(de.readLong(member));
                    case 2 -> builder.startedAt(de.readTimestamp(member));
                    case 3 -> builder.completedAt(de.readTimestamp(member));
                    case 4 -> builder.durationMs(de.readLong(member));
                    case 5 -> builder.executionStatus(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

