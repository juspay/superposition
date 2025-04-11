
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

@SmithyGenerated
public final class CreateExperimentOutput implements SerializableStruct {
    public static final ShapeId $ID = ShapeId.from("io.superposition#CreateExperimentResponse");

    public static final Schema $SCHEMA = Schema.structureBuilder($ID)
        .putMember("experiment_id", PreludeSchemas.STRING,
                new RequiredTrait())
        .build();

    private static final Schema $SCHEMA_EXPERIMENT_ID = $SCHEMA.member("experiment_id");

    private final transient String experimentId;

    private CreateExperimentOutput(Builder builder) {
        this.experimentId = builder.experimentId;
    }

    public String experimentId() {
        return experimentId;
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
        CreateExperimentOutput that = (CreateExperimentOutput) other;
        return Objects.equals(this.experimentId, that.experimentId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(experimentId);
    }

    @Override
    public Schema schema() {
        return $SCHEMA;
    }

    @Override
    public void serializeMembers(ShapeSerializer serializer) {
        serializer.writeString($SCHEMA_EXPERIMENT_ID, experimentId);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getMemberValue(Schema member) {
        return switch (member.memberIndex()) {
            case 0 -> (T) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_ID, member, experimentId);
            default -> throw new IllegalArgumentException("Attempted to get non-existent member: " + member.id());
        };
    }

    /**
     * Create a new builder containing all the current property values of this object.
     *
     * <p><strong>Note:</strong> This method performs only a shallow copy of the original properties.
     *
     * @return a builder for {@link CreateExperimentOutput}.
     */
    public Builder toBuilder() {
        var builder = new Builder();
        builder.experimentId(this.experimentId);
        return builder;
    }

    /**
     * @return returns a new Builder.
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * Builder for {@link CreateExperimentOutput}.
     */
    public static final class Builder implements ShapeBuilder<CreateExperimentOutput> {
        private final PresenceTracker tracker = PresenceTracker.of($SCHEMA);
        private String experimentId;

        private Builder() {}

        @Override
        public Schema schema() {
            return $SCHEMA;
        }

        /**
         * <p><strong>Required</strong>
         * @return this builder.
         */
        public Builder experimentId(String experimentId) {
            this.experimentId = Objects.requireNonNull(experimentId, "experimentId cannot be null");
            tracker.setMember($SCHEMA_EXPERIMENT_ID);
            return this;
        }

        @Override
        public CreateExperimentOutput build() {
            tracker.validate();
            return new CreateExperimentOutput(this);
        }

        @Override
        @SuppressWarnings("unchecked")
        public void setMemberValue(Schema member, Object value) {
            switch (member.memberIndex()) {
                case 0 -> experimentId((String) SchemaUtils.validateSameMember($SCHEMA_EXPERIMENT_ID, member, value));
                default -> ShapeBuilder.super.setMemberValue(member, value);
            }
        }

        @Override
        public ShapeBuilder<CreateExperimentOutput> errorCorrection() {
            if (tracker.allSet()) {
                return this;
            }
            if (!tracker.checkMember($SCHEMA_EXPERIMENT_ID)) {
                experimentId("");
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
                    case 0 -> builder.experimentId(de.readString(member));
                    default -> throw new IllegalArgumentException("Unexpected member: " + member.memberName());
                }
            }
        }
    }
}

