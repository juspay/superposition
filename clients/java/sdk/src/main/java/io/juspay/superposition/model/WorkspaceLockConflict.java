
package io.juspay.superposition.model;

import java.util.Objects;
import software.amazon.smithy.java.core.error.ModeledException;
import software.amazon.smithy.java.core.schema.PreludeSchemas;
import software.amazon.smithy.java.core.schema.PresenceTracker;
import software.amazon.smithy.java.core.schema.Schema;
import software.amazon.smithy.java.core.schema.SchemaUtils;
import software.amazon.smithy.java.core.schema.ShapeBuilder;
import software.amazon.smithy.java.core.serde.ShapeDeserializer;
import software.amazon.smithy.java.core.serde.ShapeSerializer;
import software.amazon.smithy.java.core.serde.ToStringSerializer;
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.ErrorTrait;
import software.amazon.smithy.model.traits.HttpErrorTrait;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Returned when a workspace write operation cannot proceed because another write operation currently
 * holds the workspace lock.
 */
@SmithyGenerated
public final class WorkspaceLockConflict extends ModeledException {
    public static final ShapeId $ID = ShapeId.from("io.superposition#WorkspaceLockConflict");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID,
            new ErrorTrait("client"),
            new HttpErrorTrait(409))
        .putMember("message", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("lock", WorkspaceLock.$SCHEMA,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_MESSAGE = $SCHEMA.member("message");
    private static final Schema $SCHEMA_LOCK = $SCHEMA.member("lock");

    private final transient WorkspaceLock lock;

    private WorkspaceLockConflict(Builder builder) {
        super($SCHEMA, builder.message, builder.$cause, builder.$captureStackTrace, builder.$deserialized);
        this.lock = builder.lock;
    }

    public WorkspaceLock lock() {
        return lock;
    }

    @Override
    public String toString() {
        return ToStringSerializer.serialize(this);
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_MESSAGE, getMessage());
        if (lock != null) {
            serializer.writeStruct($SCHEMA_LOCK, lock);
        }
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_MESSAGE, member, getMessage());
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_LOCK, member, lock);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link WorkspaceLockConflict}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.message(getMessage());
        builder.lock(this.lock);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link WorkspaceLockConflict}.
     */
    public static final class Builder implements ShapeBuilder<WorkspaceLockConflict> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String message;
        private WorkspaceLock lock;
        private Throwable $cause;
        private Boolean $captureStackTrace;
        private boolean $deserialized;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder message(String message) {
            this.message = Objects.requireNonNull(message, "message cannot be null");
            tracker.setMember($SCHEMA_MESSAGE);
            return this;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder lock(WorkspaceLock lock) {
            this.lock = Objects.requireNonNull(lock, "lock cannot be null");
            tracker.setMember($SCHEMA_LOCK);
            return this;
        }

        public Builder withStackTrace() {
            this.$captureStackTrace = true;
            return this;
        }

        public Builder withoutStackTrace() {
            this.$captureStackTrace = false;
            return this;
        }

        public Builder withCause(Throwable cause) {
            this.$cause = cause;
            return this;
        }

        @Override
        public WorkspaceLockConflict build() {
            tracker.validate();
            return new WorkspaceLockConflict(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> message((String) SchemaUtils.validateSameMember($SCHEMA_MESSAGE, member, value));
                case 1 -> lock((WorkspaceLock) SchemaUtils.validateSameMember($SCHEMA_LOCK, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<WorkspaceLockConflict> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_MESSAGE)) {
                message("");
            }
            if (!tracker.checkMember($SCHEMA_LOCK)) {
                tracker.setMember($SCHEMA_LOCK);
            }
            return this;
        }

        @Override
        public Builder deserialize(ShapeDeserializer decoder) {
            this.$deserialized = true;
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
                    case 0 -> builder.message(de.readString(member));
                    case 1 -> builder.lock(WorkspaceLock.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

