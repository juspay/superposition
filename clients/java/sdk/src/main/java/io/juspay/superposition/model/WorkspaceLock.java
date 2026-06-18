
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Metadata for an active workspace write lock. Present only while another write operation is holding
 * the workspace lease.
 */
@SmithyGenerated
public final class WorkspaceLock implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WorkspaceLock");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("lock_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("operation", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("locked_by", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("acquired_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .putMember("expires_at", SharedSchemas.DATE_TIME,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_LOCK_ID = $SCHEMA.member("lock_id");
    private static final Schema $SCHEMA_OPERATION = $SCHEMA.member("operation");
    private static final Schema $SCHEMA_LOCKED_BY = $SCHEMA.member("locked_by");
    private static final Schema $SCHEMA_ACQUIRED_AT = $SCHEMA.member("acquired_at");
    private static final Schema $SCHEMA_EXPIRES_AT = $SCHEMA.member("expires_at");

    private final transient String lockId;
    private final transient String operation;
    private final transient String lockedBy;
    private final transient Instant acquiredAt;
    private final transient Instant expiresAt;

    private WorkspaceLock(Builder builder) {
        this.lockId = builder.lockId;
        this.operation = builder.operation;
        this.lockedBy = builder.lockedBy;
        this.acquiredAt = builder.acquiredAt;
        this.expiresAt = builder.expiresAt;
    }

    /**
     * Unique identifier for the active workspace lock.
     */
    public String lockId() {
        return lockId;
    }

    /**
     * Write operation that currently holds the workspace lock.
     */
    public String operation() {
        return operation;
    }

    /**
     * User that acquired the workspace lock.
     */
    public String lockedBy() {
        return lockedBy;
    }

    /**
     * Timestamp at which the workspace lock was acquired.
     */
    public Instant acquiredAt() {
        return acquiredAt;
    }

    /**
     * Timestamp at which the workspace lock expires if it is not released first.
     */
    public Instant expiresAt() {
        return expiresAt;
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
        WorkspaceLock that = (WorkspaceLock) other;
        return Objects.equals(this.lockId, that.lockId)
               && Objects.equals(this.operation, that.operation)
               && Objects.equals(this.lockedBy, that.lockedBy)
               && Objects.equals(this.acquiredAt, that.acquiredAt)
               && Objects.equals(this.expiresAt, that.expiresAt);
    }

    @Override
    public int hashCode() {
        return Objects.hash(lockId, operation, lockedBy, acquiredAt, expiresAt);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_LOCK_ID, lockId);
        serializer.writeString($SCHEMA_OPERATION, operation);
        serializer.writeString($SCHEMA_LOCKED_BY, lockedBy);
        serializer.writeTimestamp($SCHEMA_ACQUIRED_AT, acquiredAt);
        serializer.writeTimestamp($SCHEMA_EXPIRES_AT, expiresAt);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_LOCK_ID, member, lockId);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_OPERATION, member, operation);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_LOCKED_BY, member, lockedBy);
            case 3 -> (T) SchemaUtils.validateSameMember($SCHEMA_ACQUIRED_AT, member, acquiredAt);
            case 4 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPIRES_AT, member, expiresAt);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link WorkspaceLock}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.lockId(this.lockId);
        builder.operation(this.operation);
        builder.lockedBy(this.lockedBy);
        builder.acquiredAt(this.acquiredAt);
        builder.expiresAt(this.expiresAt);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link WorkspaceLock}.
     */
    public static final class Builder implements ShapeBuilder<WorkspaceLock> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String lockId;
        private String operation;
        private String lockedBy;
        private Instant acquiredAt;
        private Instant expiresAt;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * Unique identifier for the active workspace lock.
         *
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lockId(String lockId) {
            this.lockId = Objects.requireNonNull(lockId, "lockId cannot be null");
            tracker.setMember($SCHEMA_LOCK_ID);
            return this;
        }

        /**
         * Write operation that currently holds the workspace lock.
         *
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder operation(String operation) {
            this.operation = Objects.requireNonNull(operation, "operation cannot be null");
            tracker.setMember($SCHEMA_OPERATION);
            return this;
        }

        /**
         * User that acquired the workspace lock.
         *
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lockedBy(String lockedBy) {
            this.lockedBy = Objects.requireNonNull(lockedBy, "lockedBy cannot be null");
            tracker.setMember($SCHEMA_LOCKED_BY);
            return this;
        }

        /**
         * Timestamp at which the workspace lock was acquired.
         *
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder acquiredAt(Instant acquiredAt) {
            this.acquiredAt = Objects.requireNonNull(acquiredAt, "acquiredAt cannot be null");
            tracker.setMember($SCHEMA_ACQUIRED_AT);
            return this;
        }

        /**
         * Timestamp at which the workspace lock expires if it is not released first.
         *
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder expiresAt(Instant expiresAt) {
            this.expiresAt = Objects.requireNonNull(expiresAt, "expiresAt cannot be null");
            tracker.setMember($SCHEMA_EXPIRES_AT);
            return this;
        }

        @Override
        public WorkspaceLock build() {
            tracker.validate();
            return new WorkspaceLock(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> lockId((String) SchemaUtils.validateSameMember($SCHEMA_LOCK_ID, member, value));
                case 1 -> operation((String) SchemaUtils.validateSameMember($SCHEMA_OPERATION, member, value));
                case 2 -> lockedBy((String) SchemaUtils.validateSameMember($SCHEMA_LOCKED_BY, member, value));
                case 3 -> acquiredAt((Instant) SchemaUtils.validateSameMember($SCHEMA_ACQUIRED_AT, member, value));
                case 4 -> expiresAt((Instant) SchemaUtils.validateSameMember($SCHEMA_EXPIRES_AT, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<WorkspaceLock> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_LOCK_ID)) {
                lockId("");
            }
            if (!tracker.checkMember($SCHEMA_OPERATION)) {
                operation("");
            }
            if (!tracker.checkMember($SCHEMA_LOCKED_BY)) {
                lockedBy("");
            }
            if (!tracker.checkMember($SCHEMA_ACQUIRED_AT)) {
                acquiredAt(Instant.EPOCH);
            }
            if (!tracker.checkMember($SCHEMA_EXPIRES_AT)) {
                expiresAt(Instant.EPOCH);
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
                    case 0 -> builder.lockId(de.readString(member));
                    case 1 -> builder.operation(de.readString(member));
                    case 2 -> builder.lockedBy(de.readString(member));
                    case 3 -> builder.acquiredAt(de.readTimestamp(member));
                    case 4 -> builder.expiresAt(de.readTimestamp(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

