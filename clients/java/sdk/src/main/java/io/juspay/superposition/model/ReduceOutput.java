
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
import software.amazon.smithy.model.shapes.ShapeId;
import software.amazon.smithy.model.traits.RequiredTrait;
import software.amazon.smithy.utils.SmithyGenerated;

/**
 * Response returned when a job is submitted. Contains the BJM job ID, Kronos job ID, and initial
 * status.
 */
@SmithyGenerated
public final class ReduceOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#JobCreateResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("kronos_job_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .putMember("status", BackgroundJobStatus.$SCHEMA,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_ID = $SCHEMA.member("id");
    private static final Schema $SCHEMA_KRONOS_JOB_ID = $SCHEMA.member("kronos_job_id");
    private static final Schema $SCHEMA_STATUS = $SCHEMA.member("status");

    private final transient String id;
    private final transient String kronosJobId;
    private final transient BackgroundJobStatus status;

    private ReduceOutput(Builder builder) {
        this.id = builder.id;
        this.kronosJobId = builder.kronosJobId;
        this.status = builder.status;
    }

    public String id() {
        return id;
    }

    public String kronosJobId() {
        return kronosJobId;
    }

    public BackgroundJobStatus status() {
        return status;
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
        ReduceOutput that = (ReduceOutput) other;
        return Objects.equals(this.id, that.id)
               && Objects.equals(this.kronosJobId, that.kronosJobId)
               && Objects.equals(this.status, that.status);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, kronosJobId, status);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_ID, id);
        serializer.writeString($SCHEMA_KRONOS_JOB_ID, kronosJobId);
        serializer.writeString($SCHEMA_STATUS, status.value());
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_ID, member, id);
            case 1 -> (T) SchemaUtils.validateSameMember($SCHEMA_KRONOS_JOB_ID, member, kronosJobId);
            case 2 -> (T) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, status);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link ReduceOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.id(this.id);
        builder.kronosJobId(this.kronosJobId);
        builder.status(this.status);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link ReduceOutput}.
     */
    public static final class Builder implements ShapeBuilder<ReduceOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String id;
        private String kronosJobId;
        private BackgroundJobStatus status;

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
        public Builder status(BackgroundJobStatus status) {
            this.status = Objects.requireNonNull(status, "status cannot be null");
            tracker.setMember($SCHEMA_STATUS);
            return this;
        }

        @Override
        public ReduceOutput build() {
            tracker.validate();
            return new ReduceOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> id((String) SchemaUtils.validateSameMember($SCHEMA_ID, member, value));
                case 1 -> kronosJobId((String) SchemaUtils.validateSameMember($SCHEMA_KRONOS_JOB_ID, member, value));
                case 2 -> status((BackgroundJobStatus) SchemaUtils.validateSameMember($SCHEMA_STATUS, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<ReduceOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_ID)) {
                id("");
            }
            if (!tracker.checkMember($SCHEMA_KRONOS_JOB_ID)) {
                kronosJobId("");
            }
            if (!tracker.checkMember($SCHEMA_STATUS)) {
                status(BackgroundJobStatus.unknown(""));
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
                    case 2 -> builder.status(BackgroundJobStatus.builder().deserializeMember(de, member).build());
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

